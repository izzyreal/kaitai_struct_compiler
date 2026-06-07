package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import io.kaitai.struct.format.KSVersion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteIfSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  test("cpp_stl_11 emits if_write read-write API") {
    val config = CLIConfig(
      runtime = RuntimeConfig(
        autoRead = false,
        readWrite = true,
        zeroCopySubstream = false,
        cppConfig = CppRuntimeConfig().copyAsCpp11()
      )
    )

    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/if_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val header = files("if_write.h")
    val source = files("if_write.cpp")

    header should include ("bool _is_null_label()")
    header should include ("bool _is_null_payload()")
    source should include ("if (has_label() != 0)")
    source should include ("if (!(!n_label))")
    source should include ("if (has_payload() != 0)")
    source should include ("m_payload.get()->_check();")
  }
}
