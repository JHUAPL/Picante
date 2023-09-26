package picante.mechanics.rotations;

import static org.junit.Assert.assertSame;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertRotationAngleEquals;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateTransform;
import picante.mechanics.UnwritableStateTransform;

public class DifferentiatedRotationsTest {

  private static final double TOLERANCE = 1.0E-14;

  private UnwritableRotationMatrixIJK matrix;
  private UnwritableVectorIJK axis;
  private UnwritableVectorIJK av;
  private UnwritableVectorIJK avTo;
  private double angle;
  private DifferentiatedRotation rotation;
  private UnwritableStateTransform transform;

  @Before
  public void setUp() {
    axis = new VectorIJK(1, 1, 3).unitize();
    angle = Math.toRadians(62.0);
    matrix = UnwritableRotationMatrixIJK
        .copyOf(new AxisAndAngle(axis, angle).getRotation(new RotationMatrixIJK()));
    av = new UnwritableVectorIJK(20.0, -1.0, 3.0);
    avTo = matrix.mxv(av);
    rotation = new WrapperWithRate<EulerAngles.IJK>(new EulerAngles.IJK(matrix), av);
    transform = UnwritableStateTransform.copyOf(rotation.getTransform(new StateTransform()));
  }

  @Test
  public void testSetTo() {
    DifferentiatedEulerAngles.IKI buffer = new DifferentiatedEulerAngles.IKI();
    DifferentiatedEulerAngles.IKI result = DifferentiatedRotations.setTo(rotation, buffer);

    assertSame(buffer, result);
    assertRotationAngleEquals(matrix, result.getRotation().getRotation(new RotationMatrixIJK()),
        TOLERANCE);

    assertComponentEquals(transform, result.getTransform(new StateTransform()), TOLERANCE);
  }

  @Test
  public void testGetAngularVelocityInFromFrameUnwritableStateTransform() {
    VectorIJK result = DifferentiatedRotations.getAngularVelocityInFromFrame(transform);
    assertComponentEquals(result, av, TOLERANCE);
  }

  @Test
  public void testGetAngularVelocityInFromFrameUnwritableDifferentiatedRotation() {
    VectorIJK result = DifferentiatedRotations.getAngularVelocityInFromFrame(rotation);
    assertComponentEquals(result, av, TOLERANCE);
  }

  @Test
  public void testGetAngularVelocityInToFrameUnwritableStateTransform() {
    VectorIJK result = DifferentiatedRotations.getAngularVelocityInToFrame(transform);
    assertComponentEquals(result, avTo, TOLERANCE);
  }

  @Test
  public void testGetAngularVelocityInToFrameUnwritableDifferentiatedRotation() {
    VectorIJK result = DifferentiatedRotations.getAngularVelocityInToFrame(rotation);
    assertComponentEquals(result, avTo, TOLERANCE);
  }

}
