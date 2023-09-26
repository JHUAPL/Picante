package picante.mechanics.rotations;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import picante.math.intervals.UnwritableInterval;
import picante.mechanics.StateTransform;
import picante.mechanics.rotations.EulerAngles.Axis;
import picante.mechanics.rotations.EulerAnglesTest.AxisTriple;

public class DifferentiatedEulerAnglesABCLeftTest extends DifferentiatedEulerAnglesTest {

  private static final UnwritableInterval CENTER_RANGE =
      new UnwritableInterval(-Math.PI / 2.0, Math.PI / 2.0);

  @Override
  public int getNumberOfCases() {
    return 3;
  }

  @Override
  public DifferentiatedEulerAngles createAngles(int caseIndex) {
    switch (caseIndex) {
      case 0:
        return new DifferentiatedEulerAngles.IKJ();
      case 1:
        return new DifferentiatedEulerAngles.JIK();
      case 2:
        return new DifferentiatedEulerAngles.KJI();
      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public DifferentiatedEulerAngles createAngles(int caseIndex, double left, double center,
      double right, double leftDerivative, double centerDerivative, double rightDerivative) {
    switch (caseIndex) {
      case 0:
        return new DifferentiatedEulerAngles.IKJ(left, center, right, leftDerivative,
            centerDerivative, rightDerivative);
      case 1:
        return new DifferentiatedEulerAngles.JIK(left, center, right, leftDerivative,
            centerDerivative, rightDerivative);
      case 2:
        return new DifferentiatedEulerAngles.KJI(left, center, right, leftDerivative,
            centerDerivative, rightDerivative);
      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public Class<?> getClass(int caseIndex) {
    switch (caseIndex) {
      case 0:
        return DifferentiatedEulerAngles.IKJ.class;
      case 1:
        return DifferentiatedEulerAngles.JIK.class;
      case 2:
        return DifferentiatedEulerAngles.KJI.class;

      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public AxisTriple getTriple(int caseIndex) {
    switch (caseIndex) {
      case 0:
        return new EulerAnglesTest.AxisTriple(Axis.I, Axis.K, Axis.J);
      case 1:
        return new EulerAnglesTest.AxisTriple(Axis.J, Axis.I, Axis.K);
      case 2:
        return new EulerAnglesTest.AxisTriple(Axis.K, Axis.J, Axis.I);
      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public double computeU(Axis left, Axis center, double centerAngle) {
    return -left.getCrossSign(center) * Math.sin(centerAngle);
  }

  @SuppressWarnings("unused")
  @Override
  public double computeV(Axis left, Axis center, double centerAngle) {
    return Math.cos(centerAngle);
  }

  private double getDegenerateCenterAngle() {
    return Math.PI / 2.0;
  }

  @Override
  @Test
  public void testDegenerateSetToCosAcceptedBranch() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      DifferentiatedEulerAngles angles =
          createAngles(i, 0.1, getDegenerateCenterAngle(), 1.1, 0.4, 1.0, -0.5);
      StateTransform transform = angles.getTransform(new StateTransform());
      angles.setTo(transform);
      assertEquals(0.0, angles.getLeftAngle(), 0.0);
      assertEquals(getDegenerateCenterAngle(), angles.getCenterAngle(), 0.0);
      assertEquals(1.2, angles.getRightAngle(), ANGLE_TOLERANCE);
      assertEquals(0.0, angles.getLeftAngleDerivative(), 0.0);
      assertEquals(0.9950041652780259, angles.getCenterAngleDerivative(), ANGLE_TOLERANCE);
      assertEquals(-0.1, angles.getRightAngleDerivative(), ANGLE_TOLERANCE);
    }
  }

  @Override
  @Test
  public void testDegenerateSetToCosRejectedBranch() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      DifferentiatedEulerAngles angles =
          createAngles(i, 1.1, getDegenerateCenterAngle(), 0.5, 0.4, 1.0, -0.5);
      StateTransform transform = angles.getTransform(new StateTransform());
      angles.setTo(transform);
      assertEquals(0.0, angles.getLeftAngle(), 0.0);
      assertEquals(getDegenerateCenterAngle(), angles.getCenterAngle(), 0.0);
      assertEquals(1.6, angles.getRightAngle(), ANGLE_TOLERANCE);
      assertEquals(0.0, angles.getLeftAngleDerivative(), 0.0);
      assertEquals(0.45359612142557737, angles.getCenterAngleDerivative(), ANGLE_TOLERANCE);
      assertEquals(-0.1, angles.getRightAngleDerivative(), ANGLE_TOLERANCE);
    }
  }

  @Override
  public UnwritableInterval getCenterAngleRange() {
    return CENTER_RANGE;
  }
}
