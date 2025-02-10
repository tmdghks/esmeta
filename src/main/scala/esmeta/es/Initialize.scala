package esmeta.es

import esmeta.cfg.CFG
import esmeta.es.builtin.*
import esmeta.ir.*
import esmeta.spec.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{Map => MMap}

class Initialize(cfg: CFG) {

  /** get initial state from source text */
  def from(sourceText: String): State =
    val (ast, semiInjected) = cfg.scriptParser.fromWithCode(sourceText)
    from(semiInjected, ast)

  /** get initial state from script */
  def from(script: Script): State = from(script.code)

  /** get initial state from JS file */
  def fromFile(filename: String): State =
    val (ast, semiInjected) = cfg.scriptParser.fromFileWithCode(filename)
    from(semiInjected, ast, Some(filename))

  /** get initial state with source text and cached AST */
  def from(
    sourceText: String,
    cachedAst: Ast,
    filename: Option[String] = None,
  ): State = State(
    cfg,
    context = Context(cfg.main),
    sourceText = Some(sourceText),
    cachedAst = Some(cachedAst),
    filename = filename,
    globals = MMap.from(initGlobal + (Global(SOURCE_TEXT) -> Str(sourceText))),
    heap = initHeap.copied,
  )

  // initial globals
  lazy val initGlobal: Map[Global, Value] =
    initTypedGlobal.map { case (k, (v, _)) => k -> v }
  lazy val initTypedGlobal: Map[Global, (Value, Ty)] = Map(
    EXECUTION_STACK ->
    (NamedAddr(EXECUTION_STACK), ListT(NameT("ExecutionContext"))),
    HOST_DEFINED -> (Undef, UndefT),
    INTRINSICS -> (NamedAddr(INTRINSICS), UnknownTy()),
    GLOBAL -> (NamedAddr(GLOBAL), UnknownTy()),
    SYMBOL -> (NamedAddr(SYMBOL), sym.ty),
    MATH_PI -> (Math(scala.math.Pi), MathT),
    AGENT_RECORD -> (NamedAddr(AGENT_RECORD), NameT("AgentRecord")),
    AGENT_SIGNIFIER -> (NamedAddr(AGENT_SIGNIFIER), StrT("AgentSignifier")),
    CANDIDATE_EXECUTION -> (
      NamedAddr("CandidateExecution"),
      NameT("CandidateExecution"),
    ),
    KEPT_ALIVE -> (NamedAddr("KeptAlive"), ListT(ObjectT || SymbolT)),
    REALM -> (NamedAddr(REALM), NameT("RealmRecord")),
    JOB_QUEUE -> (NamedAddr(JOB_QUEUE), ListT(NameT("PendingJob"))),
    SYMBOL_REGISTRY -> (NamedAddr(SYMBOL_REGISTRY), UnknownTy()),
    UNDEF_TYPE -> (Str("Undefined"), StrT("Undefined")),
    NULL_TYPE -> (Str("Null"), StrT("Null")),
    BOOL_TYPE -> (Str("Boolean"), StrT("Boolean")),
    STRING_TYPE -> (Str("String"), StrT("String")),
    SYMBOL_TYPE -> (Str("Symbol"), StrT("Symbol")),
    NUMBER_TYPE -> (Str("Number"), StrT("Number")),
    BIGINT_TYPE -> (Str("BigInt"), StrT("BigInt")),
    OBJECT_TYPE -> (Str("Object"), StrT("Object")),
  ).map { case (k, p) => Global(k) -> p }

  // initial heaps
  lazy val initHeap: Heap = {

    val map: MMap[Addr, Obj] = MMap(
      NamedAddr(INTRINSICS) -> intr.obj,
      NamedAddr(GLOBAL) -> glob.obj,
      NamedAddr(SYMBOL) -> sym.obj,
      NamedAddr(AGENT_RECORD) -> agent,
      NamedAddr(AGENT_SIGNIFIER) -> RecordObj("AgentSignifier"),
      NamedAddr(CANDIDATE_EXECUTION) -> YetObj(
        "CandidateExecution",
        "AgentRecord.[[CandidateExecution]]",
      ),
      NamedAddr(KEPT_ALIVE) -> ListObj(),
      NamedAddr(REALM) -> RecordObj("RealmRecord"),
      NamedAddr(EXECUTION_STACK) -> ListObj(),
      NamedAddr(JOB_QUEUE) -> ListObj(),
      NamedAddr(SYMBOL_REGISTRY) -> ListObj(),
    )

    // add symbols
    map ++= sym.map

    // add intrinsics
    map ++= intr.map

    // add member functions of intrinsics
    addBaseBuiltinFuncs(map)
    addPropBuiltinFuncs(map)

    // add global object
    map ++= glob.map

    Heap(map, map.size)
  }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // implicit CFG
  given CFG = cfg

  private val intr = Intrinsics(cfg)
  private val glob = GlobalObject(cfg)
  private val sym = builtin.Symbol(cfg)
  private val agent = RecordObj("AgentRecord")(
    "LittleEndian" -> Bool(true),
    "CanBlock" -> Bool(true),
    "Signifier" -> NamedAddr(AGENT_SIGNIFIER),
    "IsLockFree1" -> Bool(true),
    "IsLockFree2" -> Bool(true),
    "IsLockFree8" -> Bool(true),
    "CandidateExecution" -> NamedAddr(CANDIDATE_EXECUTION),
    "KeptAlive" -> NamedAddr(KEPT_ALIVE),
  )

  // get closures
  private def clo(name: String): Clo = Clo(cfg.fnameMap(name), Map())
  private def intrClo(name: String): Clo = clo(intrName(name))

  // get data from builtin head
  extension (str: String) {
    def getData: Option[(String, String, PureValue, String, Boolean, Boolean)] =
      BuiltinPath.from(str).getData
  }
  extension (path: BuiltinPath) {
    def getData: Option[(String, String, PureValue, String, Boolean, Boolean)] =
      import BuiltinPath.*
      path match
        case NormalAccess(b, n) if !(yets contains b.toString) =>
          Some((b.toString, n, Str(n), n, T, F))
        case SymbolAccess(b, n) if !(yets contains b.toString) =>
          Some((b.toString, s"@@$n", symbolAddr(n), s"[Symbol.$n]", T, F))
        case Getter(path) =>
          path.getData match
            case Some((base, prop, propV, propName, _, _)) =>
              Some((base, prop, propV, s"get $propName", F, T))
            case _ => None
        case Setter(path) =>
          path.getData match
            case Some((base, prop, propV, propName, _, _)) =>
              Some((base, prop, propV, s"set $propName", F, F))
            case _ => None
        case _ => None
  }

  // add member functions of intrinsics
  private def createBuiltinFunction(
    name: String,
    defaultLength: Int,
    defaultName: String,
    map: MMap[Addr, Obj],
  ): Unit = {
    val (baseName, baseAddr) = (intrName(name), intrAddr(name))
    val subAddr = mapAddr(baseName)
    val nameAddr = descAddr(name, "name")
    val lengthAddr = descAddr(name, "length")

    val baseObj = map.get(baseAddr) match
      case Some(r: RecordObj) => r
      case _                  => RecordObj("BuiltinFunctionObject")
    val mapObj = map.get(subAddr) match
      case Some(m: MapObj) => m
      case _               => MapObj()
    val nameRecordObj = map.get(nameAddr) match
      case Some(r: RecordObj) => r
      case _                  => RecordObj("PropertyDescriptor")
    val lengthRecordObj = map.get(lengthAddr) match
      case Some(r: RecordObj) => r
      case _                  => RecordObj("PropertyDescriptor")

    def updateRecord(obj: RecordObj)(
      pairs: (String, Value)*,
    ): obj.type =
      for { (f, v) <- pairs if !obj.map.contains(f) } obj.update(Str(f), v)
      obj

    def updateMap(obj: MapObj)(
      pairs: (PureValue, Value)*,
    ): obj.type =
      for { (f, v) <- pairs if !obj.map.contains(f) } obj.update(f, v)
      obj

    map += baseAddr -> updateRecord(baseObj)(
      "Extensible" -> Bool(true),
      "ScriptOrModule" -> Null,
      "Realm" -> realmAddr,
      "Code" -> intrClo(name),
      "Prototype" -> intrAddr("Function.prototype"),
      "InitialName" -> Str(defaultName),
      INNER_MAP -> subAddr,
    )

    map += subAddr -> updateMap(mapObj)(
      Str("length") -> lengthAddr,
      Str("name") -> nameAddr,
    )

    map += nameAddr -> updateRecord(nameRecordObj)(
      "Value" -> Str(defaultName),
      "Writable" -> Bool(false),
      "Enumerable" -> Bool(false),
      "Configurable" -> Bool(true),
    )

    map += lengthAddr -> updateRecord(lengthRecordObj)(
      "Value" -> Number(defaultLength),
      "Writable" -> Bool(false),
      "Enumerable" -> Bool(false),
      "Configurable" -> Bool(true),
    )
  }
  private def addBaseBuiltinFuncs(map: MMap[Addr, Obj]): Unit = for {
    func <- cfg.funcs
    (name, head) <- func.head match
      case Some(head: BuiltinHead) =>
        head.path match
          case BuiltinPath.Base(b) if !(yets contains b) => Some((b, head))
          case _                                         => None
      case _ => None
  } createBuiltinFunction(name, getLength(head), name, map)
  private def addPropBuiltinFuncs(map: MMap[Addr, Obj]): Unit =
    var propMap: Map[Addr, Property] = Map()
    for {
      func <- cfg.funcs if func.irFunc.kind == FuncKind.Builtin
      fname = func.name.stripPrefix("INTRINSICS.")
      (base, prop, propV, defaultName, isData, isGetter) <- fname.getData
      baseMapObj <- map.get(mapAddr(intrName(base))) match
        case Some(m: MapObj) => Some(m)
        case _               => None
      desc = descAddr(base, prop)
      defaultLength = func.head.fold(0)(getLength(_))
      _ = baseMapObj.update(propV, desc)
      intr = intrAddr(fname)
      property =
        if (isData) DataProperty(intr, T, F, T)
        else if (isGetter) AccessorProperty(intr, U, F, T)
        else AccessorProperty(U, intr, F, T)
      _ =
        if (yetFuncs contains fname) map += (intr -> YetObj("", fname))
        else createBuiltinFunction(fname, defaultLength, defaultName, map)
    } (propMap.get(desc), property) match
      case (Some(l: AccessorProperty), r: AccessorProperty) =>
        var ap: AccessorProperty = l
        if (l.get == U) ap = ap.copy(get = r.get)
        if (l.set == U) ap = ap.copy(set = r.set)
        propMap += desc -> ap
      case _ =>
        propMap += desc -> property
    for {
      (desc, property) <- propMap
    } map.getOrElse(desc, map += desc -> property.toObject)

  // get length value from built-in head parameters
  private def getLength(head: Head): Int =
    head.originalParams.count(_.kind == ParamKind.Normal)
}
