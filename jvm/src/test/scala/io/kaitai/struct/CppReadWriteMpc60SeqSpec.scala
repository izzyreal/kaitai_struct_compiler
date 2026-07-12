package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteMpc60SeqSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      readWrite = true,
      zeroCopySubstream = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("cpp_stl_11 emits a real MPC60 sequence-like write path") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/mpc60seq_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val header = files("mpc60seq_write.h")
    val source = files("mpc60seq_write.cpp")

    header should include ("class mpc60seq_write_t")
    header should include ("void set_sequence(std::unique_ptr<mpc60all_write_t::sequence_t> _v)")
    source should include ("void mpc60seq_write_t::_write()")
    source should include ("m_sequence.get()->_write();")
  }
}
