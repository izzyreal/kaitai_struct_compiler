package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteRepeatExprRawSubstreamSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      readWrite = true,
      zeroCopySubstream = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("cpp_stl_11 emits repeated raw substream writeback for repeat-expr user types") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/repeat_expr_raw_substream_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val source = files("repeat_expr_raw_substream_write.cpp")

    source should include ("std::string _raw = std::string(static_cast<std::string::size_type>(entry_size()), '\\0');")
    source should include ("kaitai::kstream _io_raw(_raw);")
    source should include ("(*it).get()->_set_io(&_io_raw);")
    source should include ("(*it).get()->_write();")
    source should include ("if (_io_raw.pos() != static_cast<uint64_t>(entry_size())) {")
    source should include ("m__io->write_bytes(_io_raw.to_byte_array());")
  }
}
