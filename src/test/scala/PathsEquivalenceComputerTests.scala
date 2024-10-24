import gradcc.asts.UniqueVarId
import gradcc.lang.{NamedField, Path, SelectPath, VarPath}
import gradcc.phases.typechecking.PathsEquivalenceComputer
import org.junit.Assert.assertEquals
import org.junit.Test

class PathsEquivalenceComputerTests {

  @Test
  def trackerTest(): Unit = {

    def v(s: String) = VarPath(UniqueVarId(s, 0))

    extension(p: Path) infix def dot(f: String) = SelectPath(p, NamedField(f))

    val x = v("x")
    val y = v("y")
    val z = v("z")

    val tracker = PathsEquivalenceComputer.empty
    tracker.assertEquivalent(x dot "f", y)
    tracker.assertEquivalent(y dot "g", z)

    val exp = Some(x dot "f" dot "g" dot "h")
    val act = tracker.expressAsPathFrom(x, z dot "h")
    assertEquals(exp, act)
  }

}
