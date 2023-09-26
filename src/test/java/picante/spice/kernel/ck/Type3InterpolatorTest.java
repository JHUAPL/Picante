package picante.spice.kernel.ck;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertEquivalentVector;
import org.junit.Test;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.rotations.AxisAndAngle;

public class Type3InterpolatorTest {

  private final static double TOLERANCE_TIGHT = 1.0E-15;

  private final Type3Interpolator INTERPOLATOR = new Type3Interpolator();

  @Test
  public void testInterpolateDoubleUnwritableRotationMatrixIJKDoubleUnwritableRotationMatrixIJKDoubleRotationMatrixIJK() {

    RotationMatrixIJK u = new RotationMatrixIJK();
    RotationMatrixIJK v = new RotationMatrixIJK();
    double tu = 10.0;
    double tv = 20.0;

    /*
     * Testing this is a matter of verifying that it properly interpolates between two rotations
     * using the appropriate constant angular rate vector.
     */
    AxisAndAngle rGen = new AxisAndAngle();
    rGen.getAxis().setTo(1.0, 1.0, 1.0);
    rGen.setAngle(Math.toRadians(15.0));
    rGen.getRotation(u);

    rGen.getAxis().setTo(3.0, 1.0, -1.0);
    rGen.setAngle(Math.toRadians(-77.5));
    rGen.getRotation(v);

    RotationMatrixIJK buffer = new RotationMatrixIJK();

    RotationMatrixIJK result = INTERPOLATOR.interpolate(tu, u, tv, v, tu, buffer);
    assertSame(result, buffer);
    assertComponentEquals(u, buffer, TOLERANCE_TIGHT);

    result = INTERPOLATOR.interpolate(tu, u, tv, v, tv, buffer);
    assertSame(result, buffer);
    assertComponentEquals(v, buffer, TOLERANCE_TIGHT);

    result = INTERPOLATOR.interpolate(tu, u, tv, v, (tu + tv) / 2.0, buffer);
    assertSame(result, buffer);

    VectorIJK expectedAxis = new VectorIJK();
    VectorIJK axis = new VectorIJK();
    double expectedAngle;
    double angle;

    rGen.setTo(RotationMatrixIJK.mtxm(u, v));
    rGen.getAxis(expectedAxis);
    expectedAngle = rGen.getAngle() / 2.0;

    rGen.setTo(RotationMatrixIJK.mtxm(u, buffer));
    rGen.getAxis(axis);
    angle = rGen.getAngle();

    assertComponentEquals(expectedAxis, axis, TOLERANCE_TIGHT);
    assertEquals(expectedAngle, angle, TOLERANCE_TIGHT);

    result = INTERPOLATOR.interpolate(tu, u, tv, v, tu + 3.0 * (tv - tu) / 4.0, buffer);

    /*
     * Convert angle to 3/4th of the expected rotation, given that it was just previously half the
     * rotation from the previous test.
     */
    expectedAngle = angle * 3.0 / 2.0;

    rGen.setTo(RotationMatrixIJK.mtxm(u, buffer));
    rGen.getAxis(axis);
    angle = rGen.getAngle();

    assertComponentEquals(expectedAxis, axis, TOLERANCE_TIGHT);
    assertEquals(expectedAngle, angle, TOLERANCE_TIGHT);
  }

  @Test
  public void testInterpolateDoubleUnwritableVectorIJKDoubleUnwritableVectorIJKDoubleVectorIJK() {

    /*
     * Testing this is a simple matter of verifying that it properly interpolates between two
     * vectors in a linear fashion.
     */
    VectorIJK u = new VectorIJK(1.0, 2.0, 3.0);
    VectorIJK v = new VectorIJK(-4.0, 5.0, 6.0);
    double tu = 10;
    double tv = 20;
    VectorIJK buffer = new VectorIJK();

    VectorIJK result = INTERPOLATOR.interpolate(tu, u, tv, v, tu, buffer);
    assertSame(result, buffer);
    assertEquivalentVector(u, buffer);

    result = INTERPOLATOR.interpolate(tu, u, tv, v, tv, buffer);
    assertSame(result, buffer);
    assertEquivalentVector(v, buffer);

    result = INTERPOLATOR.interpolate(tu, u, tv, v, (tu + tv) / 2.0, buffer);
    assertSame(result, buffer);
    assertEquivalentVector(VectorIJK.combine(0.5, u, 0.5, v), buffer);

    result = INTERPOLATOR.interpolate(tu, u, tv, v, tu + (tv - tu) / 4.0, buffer);
    assertSame(result, buffer);
    assertEquivalentVector(VectorIJK.combine(0.75, u, 0.25, v), buffer);

  }

}
