package io.kaitai.struct.translators

import io.kaitai.struct.{CppRuntimeConfig, RuntimeConfig}
import io.kaitai.struct.datatype.DataType.{ArrayTypeInStream, CalcBytesType, Int1Type}
import io.kaitai.struct.exprlang.Expressions
import io.kaitai.struct.languages.components.CppImportList
import io.kaitai.struct.translators.TestTypeProviders.Always
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppBytesUnsignedSpec extends AnyFunSuite with Matchers {
  for (frontBack <- Seq(false, true)) {
    val config = RuntimeConfig(cppConfig = CppRuntimeConfig(stdStringFrontBack = frontBack))

    test(s"bytes indexing, first and last are unsigned with stdStringFrontBack=$frontBack") {
      val tr = new CppTranslator(Always(CalcBytesType), new CppImportList(), new CppImportList(), config)
      val expected = Seq(
        "a[1]" -> "static_cast<uint8_t>(a().at(1))",
        "a.first" -> (if (frontBack) "static_cast<uint8_t>(a().front())" else "static_cast<uint8_t>(a().at(0))"),
        "a.last" -> (if (frontBack) "static_cast<uint8_t>(a().back())" else "static_cast<uint8_t>(a().at(a().length() - 1))")
      )
      for ((source, cpp) <- expected) {
        val expression = Expressions.parse(source)
        tr.detectType(expression) shouldBe Int1Type(false)
        tr.translate(expression) shouldBe cpp
      }
      // Check promotion before arithmetic, rather than only the accessor's return type.
      tr.translate(Expressions.parse("a[1] + 1")) shouldBe "static_cast<uint8_t>(a().at(1)) + 1"
      tr.translate(Expressions.parse("a.first << 8")) shouldBe
        (if (frontBack) "static_cast<uint8_t>(a().front()) << 8" else "static_cast<uint8_t>(a().at(0)) << 8")
    }

    test(s"signed integer arrays retain signed element access with stdStringFrontBack=$frontBack") {
      val tr = new CppTranslator(Always(ArrayTypeInStream(Int1Type(true))), new CppImportList(), new CppImportList(), config)
      for ((source, cpp) <- Seq("a[1]" -> "a()->at(1)", "a.first" -> "a()->front()", "a.last" -> "a()->back()")) {
        val expression = Expressions.parse(source)
        tr.detectType(expression) shouldBe Int1Type(true)
        tr.translate(expression) shouldBe cpp
      }
    }
  }
}
