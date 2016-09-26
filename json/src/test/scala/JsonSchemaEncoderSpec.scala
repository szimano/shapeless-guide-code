import org.scalatest.{Matchers, WordSpec}

class JsonSchemaEncoderSpec extends WordSpec with Matchers {

  def createSchema[A](value: A)(implicit encoder: JsonSchemaEncoder[A]): Json =
    encoder.encode(value)

  "JsonSchemaEncoder" when {
    "creating schema for strings" should {
      "generate simple example" in {
        // when
        val schema = createSchema(SingleString("Bar"))

        // then
        Json.stringify(schema) shouldBe """{"type": "object", "properties": {"foo": {"type": "string"}}}"""
      }
    }
  }
}


case class SingleString(foo: String)