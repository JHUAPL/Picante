package picante.spice.kernelpool.content;

import org.junit.Test;
import picante.math.coords.AssertTools;
import picante.math.vectorspace.VectorIJK;
import picante.spice.kernel.tk.fk.dynamic.Axis;

public class AxisTest {

  private double tol = 1E-14;

  @Test
  public void testI() {
    AssertTools.assertComponentRelativeEquality(Axis.I.getVector(), VectorIJK.I, tol);
    AssertTools.assertComponentRelativeEquality(Axis.I.getVector(),
        Axis.MINUS_I.getOpposite().getVector(), tol);
  }

  @Test
  public void testJ() {
    AssertTools.assertComponentRelativeEquality(Axis.J.getVector(), VectorIJK.J, tol);
    AssertTools.assertComponentRelativeEquality(Axis.J.getVector(),
        Axis.MINUS_J.getOpposite().getVector(), tol);
  }

  @Test
  public void testK() {
    AssertTools.assertComponentRelativeEquality(Axis.K.getVector(), VectorIJK.K, tol);
    AssertTools.assertComponentRelativeEquality(Axis.K.getVector(),
        Axis.MINUS_K.getOpposite().getVector(), tol);
  }

  @Test
  public void testMinusI() {
    AssertTools.assertComponentRelativeEquality(Axis.MINUS_I.getVector(), VectorIJK.MINUS_I, tol);
    AssertTools.assertComponentRelativeEquality(Axis.MINUS_I.getVector(),
        Axis.I.getOpposite().getVector(), tol);
  }

  @Test
  public void testMinusJ() {
    AssertTools.assertComponentRelativeEquality(Axis.MINUS_J.getVector(), VectorIJK.MINUS_J, tol);
    AssertTools.assertComponentRelativeEquality(Axis.MINUS_J.getVector(),
        Axis.J.getOpposite().getVector(), tol);
  }

  @Test
  public void testMinusK() {
    AssertTools.assertComponentRelativeEquality(Axis.MINUS_K.getVector(), VectorIJK.MINUS_K, tol);
    AssertTools.assertComponentRelativeEquality(Axis.MINUS_K.getVector(),
        Axis.K.getOpposite().getVector(), tol);
  }

}
