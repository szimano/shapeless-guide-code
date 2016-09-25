import org.scalatest.{Matchers, WordSpec}

class JsonSchemaEncoderSpec extends WordSpec with Matchers {

  import shapeless._

  it when {
    "creating schema for strings" should {
      "generate simple example" in {
        // given
        val generic = Generic[SingleString]
        val encoder = implicitly[JsonSchemaEncoder[generic.Repr]]

        // when
        val schema = encoder.encode(generic.to(SingleString("Bar")))

        // then
        Json.stringify(schema) shouldBe """{"type": "object", "properties": {"foo": {"type": "string"}}}"""
      }
    }
  }
}


case class SingleString(foo: String)