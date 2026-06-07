package io.kaitai.struct

import io.kaitai.struct.problems.{CompilationProblem, StyleWarning}
import org.scalatest.matchers.should.Matchers

trait CppReadWriteSpecSupport extends Matchers {
  protected def assertNoNonStyleProblems(problems: Iterable[CompilationProblem]): Unit =
    problems.filterNot(_.isInstanceOf[StyleWarning]) shouldBe empty
}
