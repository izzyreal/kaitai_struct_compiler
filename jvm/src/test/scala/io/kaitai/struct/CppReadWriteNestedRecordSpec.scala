package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import io.kaitai.struct.format.KSVersion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteNestedRecordSpec extends AnyFunSuite with Matchers {
  KSVersion.current = Version.version

  test("cpp_stl_11 emits nested_record_write read-write API") {
    val config = CLIConfig(
      runtime = RuntimeConfig(
        autoRead = false,
        readWrite = true,
        zeroCopySubstream = false,
        cppConfig = CppRuntimeConfig().copyAsCpp11()
      )
    )

    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/nested_record_write.ksy", config)
    problems shouldBe empty

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val header = files("nested_record_write.h")
    val source = files("nested_record_write.cpp")

    header should include ("class entry_t;")
    header should include ("void set_entry(")
    header should include ("void _check();")
    source should include ("m_entry.get()->_check();")
    source should include ("m_entry.get()->_set_io(m__io);")
    source should include ("m_entry.get()->_write();")
  }
}
