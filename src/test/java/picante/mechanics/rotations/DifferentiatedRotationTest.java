package picante.mechanics.rotations;

import static org.junit.Assert.assertSame;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertEquivalentStateTransform;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.StateTransform;
import picante.mechanics.UnwritableStateTransform;

public abstract class DifferentiatedRotationTest {

  private double tolerance = 1.0E-16;

  protected StateTransform transform;
  protected StateTransform testTransform;
  protected RotationMatrixIJK testMatrix;

  protected DifferentiatedRotation rotation;

  public abstract DifferentiatedRotation createRotation(UnwritableStateTransform transform);

  protected void setTolerance(double tolerance) {
    this.tolerance = tolerance;
  }

  @Before
  public void setUp() throws Exception {
    transform = new StateTransform(
        new RotationMatrixIJK(0.9512512425641977, -0.25488700224417876, 0.17364817766693033,
            0.3016166128991695, 0.6512328797187343, -0.6963642403200189, 0.06440879088526905,
            0.7147925240657044, 0.696364240320019),
        new MatrixIJK(0.00342471295953521, 0.0649669236093831, 0.07660006643520209,
            -0.006589685295675284, -0.18124700103886787, -0.17235457830333545, -0.0197209710316274,
            0.18829692015410146, -0.19145587784669776));
    testTransform = new StateTransform();
    testMatrix = new RotationMatrixIJK();
    rotation = createRotation(transform);
  }

  @Test
  public void testInterfaceSetTo() {

    transform = new StateTransform(
        new RotationMatrixIJK(0.9990482215818578, -0.03437254630962194, 0.02685477637814414,
            0.04232370514336272, 0.6149337422478219, -0.7874421862130749, 0.010552484876318281,
            0.7878293093718222, 0.6158032350983981),
        new MatrixIJK(-0.000005688281904479898, 0.12681917476197335, 0.16253275633033337,
            0.04889716259146107, -0.08050155825266332, -0.06023758469093261, -0.1955772738020353,
            0.06836788348670658, -0.08411517389693633));

    DifferentiatedRotation returned = rotation.setTo(transform);

    assertEquivalentStateTransform(new StateTransform(
        new RotationMatrixIJK(0.9990482215818578, -0.03437254630962194, 0.02685477637814414,
            0.04232370514336272, 0.6149337422478219, -0.7874421862130749, 0.010552484876318281,
            0.7878293093718222, 0.6158032350983981),
        new MatrixIJK(-0.000005688281904479898, 0.12681917476197335, 0.16253275633033337,
            0.04889716259146107, -0.08050155825266332, -0.06023758469093261, -0.1955772738020353,
            0.06836788348670658, -0.08411517389693633)),
        transform);

    assertSame(returned, rotation);
    assertComponentEquals(transform, rotation.getTransform(new StateTransform()), tolerance);

  }

  @Test
  public void testInterfaceGetTransform() {

    StateTransform returned = rotation.getTransform(testTransform);

    assertSame(returned, testTransform);
    assertComponentEquals(transform, testTransform, tolerance);
  }

  @Test
  public void testInterfaceGetRotation() {

    Rotation returned = rotation.getRotation();

    RotationMatrixIJK returnedMatrix = returned.getRotation(testMatrix);

    assertSame(returnedMatrix, testMatrix);
    assertComponentEquals(transform.getRotation(), testMatrix, tolerance);
  }

}
