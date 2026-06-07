package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteRawSubstreamSizedSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      readWrite = true,
      zeroCopySubstream = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("cpp_stl_11 emits size-expr raw substream writeback checks") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/raw_substream_sized_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val source = files("raw_substream_sized_write.cpp")

    source should include ("m__raw_payload = std::string(static_cast<std::string::size_type>(payload_len()), '\\0');")
    source should include ("if (m__io__raw_payload.get()->pos() != static_cast<uint64_t>(payload_len())) {")
    source should include ("serialized size mismatch")
    source should include ("if (m__io__raw_payload.get()->to_byte_array().size() != static_cast<std::string::size_type>(payload_len())) {")
  }
}
