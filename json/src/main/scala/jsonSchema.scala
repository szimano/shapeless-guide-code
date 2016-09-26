import shapeless.{HList, ::, HNil}
import shapeless.{Coproduct, :+:, CNil, Inl, Inr}
import shapeless.{Lazy}
import shapeless.{LabelledGeneric, Witness}
import shapeless.labelled.FieldType

trait JsonSchemaEncoder[A] {
  def encode(a: A): Json
}

object JsonSchemaEncoder {
  def pure[A](enc: A => Json) =
    new JsonSchemaEncoder[A] {
      override def encode(a: A) = enc(a)
    }

  def pureObj[A](func: A => JsonObject): JsonObjectEncoder[A] =
    new JsonObjectEncoder[A] {
      def encode(value: A): JsonObject =
        func(value)
    }

  implicit def intEncoder: JsonSchemaEncoder[Int] = pure(i => JsonObject(List(("type", JsonString("number")))))
  implicit def stringEncoder: JsonSchemaEncoder[String] = pure(s => JsonObject(List(("type", JsonString("string")))))
  implicit def boolEncoder: JsonSchemaEncoder[Boolean] = pure(b => JsonObject(List(("type", JsonString("boolean")))))

  implicit val hnilEncoder: JsonSchemaEncoder[HNil] = pure(hnil => JsonNull)

  implicit def hlistEncoder[K <: Symbol, H, T <: HList](
                                            implicit
                                            witness: Witness.Aux[K],
                                            hEncoder: Lazy[JsonSchemaEncoder[H]],
                                            tEncoder: JsonObjectEncoder[T]
                                          ): JsonSchemaEncoder[H :: T] =
    pure {
      case h :: t =>
        val hField  = witness.value.name -> hEncoder.value.encode(h)
        val tFields = tEncoder.encode(t).fields
        JsonObject(hField :: tFields)
    }

  implicit val cnilEncoder: JsonObjectEncoder[CNil] =
    pureObj(cnil => ???)

  implicit def coproductEncoder[K <: Symbol, H, T <: Coproduct](
                                                    implicit
                                                    witness: Witness.Aux[K],
                                                    hEncoder: Lazy[JsonSchemaEncoder[H]],
                                                    tEncoder: JsonObjectEncoder[T]
                                                  ): JsonObjectEncoder[FieldType[K, H] :+: T] =
    pureObj {
      case Inl(h) => JsonObject(List(witness.value.name -> hEncoder.value.encode(h)))
      case Inr(t) => tEncoder.encode(t)
    }

  implicit def genericEncoder[A, R](
                                     implicit
                                     gen: LabelledGeneric.Aux[A, R],
                                     enc: Lazy[JsonEncoder[R]]
                                   ): JsonSchemaEncoder[A] =
    pure(a => enc.value.encode(gen.to(a)))
}


object MainSchema extends Demo {

  def createSchema[A](value: A)(implicit encoder: JsonSchemaEncoder[A]): Json =
    encoder.encode(value)

  sealed trait Sml {
    def name: String
  }

  final case class Tomek(name: String) extends Sml
  final case class Adam(name: String) extends Sml
  final case class Witold(name: String, wiek: Int, madry: Boolean) extends Sml

  println(Json.stringify(createSchema(Tomek("tomek"))))
}