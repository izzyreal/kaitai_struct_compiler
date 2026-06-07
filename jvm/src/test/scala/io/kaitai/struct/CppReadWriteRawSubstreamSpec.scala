package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteRawSubstreamSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      readWrite = true,
      zeroCopySubstream = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("cpp_stl_11 emits raw substream writeback for fixed-size user types") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/raw_substream_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val header = files("raw_substream_write.h")
    val source = files("raw_substream_write.cpp")

    header should include ("std::string _raw_payload() const")
    header should include ("kaitai::kstream* _io__raw_payload() const")
    source should include ("m__raw_payload = std::string(static_cast<std::string::size_type>(2), '\\0');")
    source should include ("m__io__raw_payload = std::unique_ptr<kaitai::kstream>(new kaitai::kstream(m__raw_payload));")
    source should include ("m_payload.get()->_set_io(m__io__raw_payload.get());")
    source should include ("m_payload.get()->_write();")
    source should include ("m__raw_payload = m__io__raw_payload.get()->to_byte_array();")
    source should include ("m__io->write_bytes(m__raw_payload);")
  }
}
