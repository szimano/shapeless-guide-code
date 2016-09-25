import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy, Witness}

trait JsonSchemaEncoder[A] {
  def encode(a: A): Json
}

object JsonSchemaEncoder {
  def createEncoder[A](enc: A => Json) =
    new JsonSchemaEncoder[A] {
      override def encode(a: A) = enc(a)
    }

  implicit def intEncoder: JsonSchemaEncoder[Int] = createEncoder(i => JsonObject(List(("type", JsonString("number")))))
  implicit def stringEncoder: JsonSchemaEncoder[String] = createEncoder(s => JsonObject(List(("type", JsonString("string")))))
  implicit def boolEncoder: JsonSchemaEncoder[Boolean] = createEncoder(b => JsonObject(List(("type", JsonString("boolean")))))

  implicit val hnilEncoder: JsonSchemaEncoder[HNil] = createEncoder(hnil => JsonNull)

  implicit def hlistEncoder[K <: Symbol, H, T <: HList](
                                            implicit
                                            witness: Witness.Aux[K],
                                            hEncoder: Lazy[JsonSchemaEncoder[H]],
                                            tEncoder: JsonObjectEncoder[T]
                                          ): JsonSchemaEncoder[H :: T] =
    createEncoder {
      case h :: t =>
        val hField  = witness.value.name -> hEncoder.value.encode(h)
        val tFields = tEncoder.encode(t).fields
        JsonObject(hField :: tFields)
    }

  implicit val cnilEncoder: JsonSchemaEncoder[CNil] =
    createEncoder(cnil => ???)

  implicit def coproductEncoder[K <: Symbol, H, T <: Coproduct](
                                                    implicit
                                                    witness: Witness.Aux[K],
                                                    hEncoder: Lazy[JsonSchemaEncoder[H]],
                                                    tEncoder: JsonSchemaEncoder[T]
                                                  ): JsonSchemaEncoder[H :+: T] =
    createEncoder {
      case Inl(h) => JsonObject(List(witness.value.name -> hEncoder.value.encode(h)))
      case Inr(t) => tEncoder.encode(t)
    }

  implicit def genericEncoder[A, R](
                                     implicit
                                     gen: Generic.Aux[A, R],
                                     enc: Lazy[JsonSchemaEncoder[R]]
                                   ): JsonSchemaEncoder[A] =
    createEncoder(a => enc.value.encode(gen.to(a)))
}

sealed trait Sml {
  def name: String
}

case class Tomek(name: String) extends Sml

case class Adam(name: String) extends Sml

case class Witold(name: String, wiek: Int, madry: Boolean) extends Sml

import shapeless._

object MainSchema extends App {
  val smlowiec = Generic[Sml]
  implicit val encoder = implicitly[JsonSchemaEncoder[smlowiec.Repr]]
  val tomek = smlowiec.to(Tomek("Domek"))
  println(encoder.encode(tomek))
  println(encoder.encode(smlowiec.to(Witold("witold", 23, false))))
}