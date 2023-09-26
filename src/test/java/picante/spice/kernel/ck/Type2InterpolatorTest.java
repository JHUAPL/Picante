package picante.spice.kernel.ck;

import static org.junit.Assert.assertSame;
import static picante.junit.AssertTools.assertEqualMatrix;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.rotations.AxisAndAngle;

public class Type2InterpolatorTest {

  private Type2Interpolator interpolator;
  private RotationMatrixIJK r0;

  @Before
  public void setUp() throws Exception {
    interpolator = new Type2Interpolator();
    r0 = new AxisAndAngle(new VectorIJK(1, 1, 0.1), Math.toRadians(10.0))
        .getRotation(new RotationMatrixIJK());
  }

  @Test
  public void testInterpolateWithZeroRateVector() {
    RotationMatrixIJK buffer = new RotationMatrixIJK();
    RotationMatrixIJK result = interpolator.interpolate(0, 1, r0, VectorIJK.ZERO, 0.5, buffer);
    assertSame(buffer, result);
    assertEqualMatrix(r0, result);
  }

}
