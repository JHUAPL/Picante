package picante.mechanics.rotations;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertComponentRelativeEquality;
import static picante.junit.AssertTools.assertRotationAngleEquals;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import com.google.common.base.Optional;
import com.google.common.collect.ImmutableList;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

public class RotationsTest {

  private static final double TOLERANCE = 1.0E-15;

  private UnwritableRotationMatrixIJK matrix;
  private double angle;
  private UnwritableVectorIJK axis;
  private Rotation rotation;

  @Before
  public void setUp() {
    axis = new VectorIJK(1, 1, 3).unitize();
    angle = Math.toRadians(62.0);
    matrix = UnwritableRotationMatrixIJK
        .copyOf(new AxisAndAngle(axis, angle).getRotation(new RotationMatrixIJK()));
    rotation = new EulerAngles.IJI(matrix);
  }

  @Test
  public void testSetTo() {

    Quaternion buffer = new Quaternion();
    Quaternion result = Rotations.setTo(rotation, buffer);

    assertSame(buffer, result);

    assertRotationAngleEquals(matrix, result.getRotation(new RotationMatrixIJK()), TOLERANCE);

  }

  @Test
  public void testGetRotationAxisUnwritableRotationMatrixIJK() {
    VectorIJK result = Rotations.getRotationAxis(matrix);
    assertComponentEquals(axis, result, TOLERANCE);
  }

  @Test
  public void testGetRotationAxisUnwritableRotation() {
    VectorIJK result = Rotations.getRotationAxis(rotation);
    assertComponentEquals(axis, result, TOLERANCE);
  }

  @Test
  public void testGetRotationAngleUnwritableRotationMatrixIJK() {
    double result = Rotations.getRotationAngle(matrix);
    assertEquals(angle, result, TOLERANCE);
  }

  @Test
  public void testGetRotationAngleUnwritableRotation() {
    double result = Rotations.getRotationAngle(rotation);
    assertEquals(angle, result, TOLERANCE);
  }

  // TODO: Implement this in a reasonable way
  @Ignore
  @Test
  public void testProximateAverageUnwritableRotation() {
    fail("Not implemented yet.");
  }

  @Test
  public void testProximateAverageUnwritableRotationMatrixIJK() {

    // TODO: Expand this test... this is completely trivial.

    EulerAngles.IJK angles = new EulerAngles.IJK();

    angles.setLeftAngle(Math.toRadians(5.0));
    RotationMatrixIJK pLeft = angles.getRotation(new RotationMatrixIJK());

    angles.setLeftAngle(-Math.toRadians(5.0));
    RotationMatrixIJK mLeft = angles.getRotation(new RotationMatrixIJK());

    RotationMatrixIJK buffer = new RotationMatrixIJK();
    RotationMatrixIJK result =
        Rotations.proximateAverage(ImmutableList.of(pLeft, pLeft, mLeft, mLeft), buffer);

    assertSame(result, buffer);
    assertRotationAngleEquals(RotationMatrixIJK.IDENTITY, result, 1e-6);
  }


  @Test
  public void testComputeAdjustedAngularRateAbsentResult() {

    EulerAngles.IJK angles = new EulerAngles.IJK();

    angles.setLeftAngle(Math.toRadians(10.0));
    RotationMatrixIJK start = angles.getRotation(new RotationMatrixIJK());

    angles.setLeftAngle(Math.toRadians(20.0));
    RotationMatrixIJK finish = angles.getRotation(new RotationMatrixIJK());

    VectorIJK axis = new VectorIJK(VectorIJK.I).scale(Math.toRadians(40.0));

    assertFalse(
        Rotations.computeAdjustedAngularRate(0.0, start, axis, 1.0, finish, Math.toRadians(5.0))
            .isPresent());

  }

  @Test
  public void testComputeAdjustedAngularRate() {

    EulerAngles.IJK angles = new EulerAngles.IJK();

    angles.setLeftAngle(Math.toRadians(10.0));
    RotationMatrixIJK start = angles.getRotation(new RotationMatrixIJK());

    angles.setLeftAngle(Math.toRadians(20.0));
    RotationMatrixIJK finish = angles.getRotation(new RotationMatrixIJK());

    VectorIJK axis = new VectorIJK(Math.toRadians(40.0), 0.0, 0.1);

    VectorIJK expected = new VectorIJK(Math.toRadians(10.0), 0.0, 0.0);

    Optional<VectorIJK> result =
        Rotations.computeAdjustedAngularRate(0.0, start, axis, 1.0, finish, Math.toRadians(45));

    assertTrue(result.isPresent());
    assertComponentRelativeEquality(expected, result.get(), 1.0e-12);

  }
}
