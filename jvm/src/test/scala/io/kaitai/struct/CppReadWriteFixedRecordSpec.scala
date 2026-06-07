package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import io.kaitai.struct.format.KSVersion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteFixedRecordSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  test("cpp_stl_11 emits fixed_record_write read-write API") {
    val config = CLIConfig(
      runtime = RuntimeConfig(
        autoRead = false,
        readWrite = true,
        zeroCopySubstream = false,
        cppConfig = CppRuntimeConfig().copyAsCpp11()
      )
    )

    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/fixed_record_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val header = files("fixed_record_write.h")
    val source = files("fixed_record_write.cpp")

    header should include ("void set_magic(")
    header should include ("void set_name(")
    header should include ("void _check();")
    header should include ("void _write();")
    source should include ("m__io->write_u2le(m_version);")
    source should include ("m__io->write_u1(static_cast<uint8_t>(m_kind));")
    source should include ("m__io->write_bytes(m_name);")
    source should include ("m__io->write_bytes(m_payload);")
  }
}
