package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteMpc60SetSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      readWrite = true,
      zeroCopySubstream = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("cpp_stl_11 emits a real MPC60 SET-like write path") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/mpc60set_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val header = files("mpc60set_write.h")
    val source = files("mpc60set_write.cpp")

    header should include ("class mpc60set_write_t")
    header should include ("void set_sound_directory_entry(std::unique_ptr<std::vector<std::unique_ptr<sound_directory_entry_t>>> _v)")
    header should include ("void set_sound_samples(std::unique_ptr<std::vector<uint64_t>> _v)")
    source should include ("void mpc60set_write_t::_write()")
    source should include ("m__io->write_bits_int_le(12, (*it));")
  }
}
