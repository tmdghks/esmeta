package esmeta.util

import scala.collection.mutable.{Map => MMap}
import io.circe.*, io.circe.generic.semiauto.*
import io.circe.syntax.*

/** basic JSON protocols */
trait BasicJsonProtocol {
  // decoder for map structures with string keys
  def strMapDecoder[V](using decoder: Decoder[V]): Decoder[Map[String, V]] =
    Decoder.instance(_.as[Map[String, V]])

  // encoder for map structures with string keys
  def strMapEncoder[V](using encoder: Encoder[V]): Encoder[Map[String, V]] =
    Encoder.instance { map =>
      Json.fromFields(map.toList.sortBy(_._1).map { (k, v) => (k, v.asJson) })
    }

  // decoder for double values
  def doubleDecoder: Decoder[Double] = Decoder.instance(c =>
    c.value.asString
      .map(_ match
        case "Infinity"  => Double.PositiveInfinity
        case "-Infinity" => Double.NegativeInfinity
        case "NaN"       => Double.NaN,
      )
      .orElse(c.value.asNumber.map(_.toDouble))
      .map(Right(_))
      .getOrElse(invalidFail("double", c)),
  )

  // encoder for double values
  def doubleEncoder: Encoder[Double] =
    Encoder.instance(Json.fromDoubleOrString)

  // decoder for UId: id -> UId
  def uidDecoder[T <: UId](getter: Int => Option[T]): Decoder[T] =
    Decoder.instance(c =>
      (for {
        number <- c.value.asNumber
        id <- number.toInt
        x <- getter(id)
      } yield Right(x)).getOrElse(invalidFail("id", c)),
    )

  // encoder for UId: UId -> id
  def uidEncoder[T <: UId]: Encoder[T] =
    Encoder.instance(x => Json.fromInt(x.id))

  // decoder for UId with name: { name: id } -> UId
  def uidDecoderWithName[T <: UId](
    name: String,
    getter: Int => Option[T],
  ): Decoder[T] = Decoder.instance(c =>
    (for {
      obj <- c.value.asObject
      value <- obj(name)
      number <- value.asNumber
      id <- number.toInt
      x <- getter(id)
    } yield Right(x)).getOrElse(invalidFail("id", c)),
  )

  // encoder for UId with name: UId -> { name: id }
  def uidEncoderWithName[T <: UId](name: String): Encoder[T] =
    Encoder.instance(x => Json.fromFields(Seq(name -> Json.fromInt(x.id))))

  // encoder based on discrimators
  def decoderWithDiscriminator[T](
    name: String,
    discriminators: List[(String, HCursor => Decoder.Result[T])],
  ): Decoder[T] = Decoder.instance(c =>
    (for {
      obj <- c.value.asObject
      res <- discriminators.foldLeft[Option[Decoder.Result[T]]](None) {
        case (None, (key, decoder)) if obj.contains(key) => Some(decoder(c))
        case (res, _)                                    => res
      }
    } yield res).getOrElse(invalidFail(name, c)),
  )

  // decoder based on parsers
  def decoderWithParser[T](parser: String => T): Decoder[T] =
    Decoder.instance(c =>
      c.value.asString
        .map(parser)
        .map(Right(_))
        .getOrElse(decodeFail(s"expected a string instead of ${c.value}", c)),
    )

  // encoder based on stringifiers
  def encoderWithStringifier[T](stringifier: T => String): Encoder[T] =
    Encoder.instance(x => Json.fromString(stringifier(x)))

  // decoder for set
  given setDecoder[T](using
    tDecoder: Decoder[T],
  ): Decoder[Set[T]] = new Decoder {
    final def apply(c: HCursor): Decoder.Result[Set[T]] = (for {
      vector <- c.value.asArray
    } yield vector.foldLeft[Decoder.Result[Set[T]]](Right(Set[T]())) {
      case (prev, json) =>
        for {
          set <- prev
          t <- tDecoder(json.hcursor)
        } yield set + t
    }).getOrElse(invalidFail("set", c))
  }

  // encoder for set
  given setEncoder[T](using
    tEncoder: Encoder[T],
  ): Encoder[Set[T]] = new Encoder {
    final def apply(set: Set[T]): Json =
      Json.fromValues(set.map(tEncoder.apply))
  }

  // decoder for option
  given optionDecoder[T](using
    tDecoder: Decoder[T],
  ): Decoder[Option[T]] = new Decoder {
    final def apply(c: HCursor): Decoder.Result[Option[T]] =
      if (c.value.isNull) Right(None) else tDecoder(c).map(Some(_))
  }

  // encoder for option
  given optionEncoder[T](using
    tEncoder: Encoder[T],
  ): Encoder[Option[T]] = new Encoder {
    final def apply(opt: Option[T]): Json =
      opt.fold(Json.Null)(tEncoder.apply)
  }

  // decoder for pair
  given pairDecoder[A, B](using
    aDecoder: Decoder[A],
    bDecoder: Decoder[B],
  ): Decoder[(A, B)] = new Decoder {
    final def apply(c: HCursor): Decoder.Result[(A, B)] = (for {
      Vector(x, y) <- c.value.asArray
    } yield for {
      a <- aDecoder(x.hcursor)
      b <- bDecoder(y.hcursor)
    } yield (a, b)).getOrElse(invalidFail("pair", c))
  }

  // encoder for pair
  given pairEncoder[A, B](using
    aEncoder: Encoder[A],
    bEncoder: Encoder[B],
  ): Encoder[(A, B)] = new Encoder {
    final def apply(pair: (A, B)): Json =
      val (a, b) = pair
      Json.fromValues(Seq(aEncoder(a), bEncoder(b)))
  }

  // decoder for IntId: id -> IntId
  def idDecoder[T <: IntId](getter: Int => Option[T]): Decoder[T] =
    new Decoder[T] {
      final def apply(c: HCursor): Decoder.Result[T] = (for {
        number <- c.value.asNumber
        id <- number.toInt
        x <- getter(id)
      } yield Right(x)).getOrElse(invalidFail("id", c))
    }

  // encoder for IntId: IntId -> id
  def idEncoder[T <: IntId]: Encoder[T] = new Encoder[T] {
    final def apply(x: T): Json = Json.fromInt(x.id)
  }

  // decoder for IntId with name: { name: id } -> IntId
  def idDecoder[T <: IntId](
    name: String,
    getter: Int => Option[T],
  ): Decoder[T] = new Decoder[T] {
    final def apply(c: HCursor): Decoder.Result[T] = (for {
      obj <- c.value.asObject
      value <- obj(name)
      number <- value.asNumber
      id <- number.toInt
      x <- getter(id)
    } yield Right(x)).getOrElse(invalidFail("id", c))
  }

  // encoder for IntId with name: IntId -> { name: id }
  def idEncoder[T <: IntId](name: String): Encoder[T] = new Encoder[T] {
    final def apply(x: T): Json =
      Json.fromFields(Seq(name -> Json.fromInt(x.id)))
  }

  // decoding failure
  def decodeFail[T](msg: String, c: HCursor): Decoder.Result[T] =
    Left(DecodingFailure(msg, c.history))

  // decoding failure
  def invalidFail[T](name: String, c: HCursor): Decoder.Result[T] =
    decodeFail(s"invalid $name: ${c.value}", c)
}
