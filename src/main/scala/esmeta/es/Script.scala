package esmeta.es

/** ECMAScript script program */
case class Script(
  code: String,
  name: String,
  elapsed: Option[Long] = None,
  iter: Option[Int] = None,
) extends ESElem
