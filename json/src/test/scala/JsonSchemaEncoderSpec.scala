import org.scalatest.{Matchers, WordSpec}

class JsonSchemaEncoderSpec extends WordSpec with Matchers {

  def createSchema[A](value: A)(implicit encoder: JsonSchemaEncoder[A]): List[JsonSchemaField] =
    encoder.encode(value)

  "JsonSchemaEncoder" when {
    "creating schema for strings" should {
      "generate simple example" in {
        // when
        val schema = createSchema(SingleString("Bar"))

        // then
        val schemaStringified = JsonSchemaSerializer.stringify(schema)
        schemaStringified shouldBe """{
                                     | "type": "object",
                                     | "properties": {
                                     |   "foo": {"type": "string"}
                                     | }
                                     |}""".stripMargin
      }
    }
  }
}


case class SingleString(foo: String)