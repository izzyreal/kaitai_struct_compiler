package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import io.kaitai.struct.format.KSVersion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteOpenEndedRepeatsSpec extends AnyFunSuite with Matchers {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      readWrite = true,
      zeroCopySubstream = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("cpp_stl_11 emits repeat_until_write read-write checks") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/repeat_until_write.ksy", config)
    problems shouldBe empty

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val source = files("repeat_until_write.cpp")

    source should include ("if (m_entries->empty())")
    source should include ("const bool _is_last = (i == m_entries->size() - 1);")
    source should include ("if ((_ == -1) != _is_last)")
  }

  test("cpp_stl_11 emits repeat_eos_write fixed-capacity checks") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/repeat_eos_write.ksy", config)
    problems shouldBe empty

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val header = files("repeat_eos_write.h")
    val source = files("repeat_eos_write.cpp")

    header should include ("void set_records(")
    source should include ("const std::size_t _remaining = static_cast<std::size_t>(m__io->size() - m__io->pos());")
    source should include ("const std::size_t _actual = m_records->size() * static_cast<std::size_t>(3);")
  }
}
