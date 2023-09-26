package picante.spice.kernelpool.content;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;
import picante.junit.AssertTools;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.spice.kernel.tk.fk.dynamic.Axis;
import picante.spice.kernel.tk.fk.dynamic.TwoVectorMatrix;

public class TwoVectorMatrixTest {

  private double tol = 1E-14;

  @Test(expected = IllegalArgumentException.class)
  public void testII() {
    new TwoVectorMatrix(Axis.I, Axis.I);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIMinusI() {
    new TwoVectorMatrix(Axis.I, Axis.MINUS_I);
  }


  @Test(expected = IllegalArgumentException.class)
  public void testJJ() {
    new TwoVectorMatrix(Axis.J, Axis.J);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testJMinusJ() {
    new TwoVectorMatrix(Axis.J, Axis.MINUS_J);
  }


  @Test(expected = IllegalArgumentException.class)
  public void testKK() {
    new TwoVectorMatrix(Axis.K, Axis.K);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testKMinusK() {
    new TwoVectorMatrix(Axis.K, Axis.MINUS_K);
  }

  @Test
  public void testMatrices() {
    UnwritableRotationMatrixIJK identity = RotationMatrixIJK.IDENTITY;
    for (Axis priAxis : Axis.values()) {
      for (Axis secAxis : Axis.values()) {
        if (!priAxis.equals(secAxis) && !priAxis.getOpposite().equals(secAxis)) {
          TwoVectorMatrix twoVec = new TwoVectorMatrix(priAxis, secAxis);
          RotationMatrixIJK rotMat = twoVec.makeMatrix(priAxis.getVector(), secAxis.getVector());
          AssertTools.assertComponentRelativeEquality(identity, rotMat, tol);
        }
      }
    }
  }

  /**
   * Need to implement
   */
  @Ignore
  @Test
  public void testStateTransform() {
    Assert.fail("Not yet implemented");
  }
}
