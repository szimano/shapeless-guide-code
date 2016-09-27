import shapeless.labelled._
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, LabelledGeneric, Lazy, Witness}

trait JsonSchemaEncoder[A] {
  def encode(a: A): List[JsonSchemaField]
}

sealed trait JsonSchemaField {
  def type_ : String
}

case class StringField() extends JsonSchemaField {
  val type_ = "string"
}

case class BooleanField() extends JsonSchemaField {
  val type_ = "boolean"
}

case class IntField() extends JsonSchemaField {
  val type_ = "number"
}

case class NamedField(name: String, field: JsonSchemaField) extends JsonSchemaField {
  val type_ = "named"
}

object JsonSchemaSerializer {
  def stringify(fields: List[JsonSchemaField]): String =
    s"""{
        | "type": "object",
        | "properties": {
        |   ${fields.map {
              case f: NamedField => s""""${f.name}": {"type": "${f.field.type_}"}"""
              case _ => throw new RuntimeException("bam!")
              }.mkString(", ")}
        | }
        |}""".stripMargin
}

object JsonSchemaEncoder {

  def pure[A](enc: A => List[JsonSchemaField]) =
    new JsonSchemaEncoder[A] {
      override def encode(a: A) = enc(a)
    }

  implicit def intEncoder: JsonSchemaEncoder[Int] = pure(i => List(IntField()))

  implicit def stringEncoder: JsonSchemaEncoder[String] = pure(s => List(StringField()))

  implicit def boolEncoder: JsonSchemaEncoder[Boolean] = pure(b => List(BooleanField()))

  implicit val hnilEncoder: JsonSchemaEncoder[HNil] = pure(hnil => List.empty)

  implicit def hlistEncoder[K <: Symbol, H, T <: HList](
                                                         implicit
                                                         witness: Witness.Aux[K],
                                                         hEncoder: Lazy[JsonSchemaEncoder[H]],
                                                         tEncoder: JsonSchemaEncoder[T]
                                                       ): JsonSchemaEncoder[FieldType[K, H] :: T] =
    pure {
      case h :: t =>
        hEncoder.value.encode(h).map(NamedField(witness.value.name, _)) ++ tEncoder.encode(t)
    }

  implicit val cnilEncoder: JsonSchemaEncoder[CNil] =
    pure(cnil => ???)

  implicit def coproductEncoder[K <: Symbol, H, T <: Coproduct](
                                                    implicit
                                                    witness: Witness.Aux[K],
                                                    hEncoder: Lazy[JsonSchemaEncoder[H]],
                                                    tEncoder: JsonSchemaEncoder[T]
                                                  ): JsonSchemaEncoder[FieldType[K, H] :+: T] =
    pure {
      case Inl(h) => hEncoder.value.encode(h).map(NamedField(witness.value.name, _))
      case Inr(t) => tEncoder.encode(t)
    }

  implicit def genericEncoder[A, R](
                                     implicit
                                     gen: LabelledGeneric.Aux[A, R],
                                     enc: Lazy[JsonSchemaEncoder[R]]
                                   ): JsonSchemaEncoder[A] =
    pure(a => enc.value.encode(gen.to(a)))
}


object MainSchema extends Demo {

  def createSchema[A](value: A)(implicit encoder: JsonSchemaEncoder[A]): List[JsonSchemaField] =
    encoder.encode(value)

  sealed trait Sml {
    def name: String
  }

  final case class Tomek(name: String) extends Sml

  final case class Adam(name: String) extends Sml

  final case class Witold(name: String, wiek: Int, madry: Boolean) extends Sml

  println(JsonSchemaSerializer.stringify(createSchema(Tomek("tomek"))))
  println(JsonSchemaSerializer.stringify(createSchema(Witold("tomek", 30, true))))
}