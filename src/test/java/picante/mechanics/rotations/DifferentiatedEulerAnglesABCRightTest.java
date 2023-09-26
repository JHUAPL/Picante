package picante.mechanics.rotations;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import picante.math.intervals.UnwritableInterval;
import picante.mechanics.StateTransform;
import picante.mechanics.rotations.EulerAngles.Axis;
import picante.mechanics.rotations.EulerAnglesTest.AxisTriple;

public class DifferentiatedEulerAnglesABCRightTest extends DifferentiatedEulerAnglesTest {

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
        return new DifferentiatedEulerAngles.IJK();
      case 1:
        return new DifferentiatedEulerAngles.JKI();
      case 2:
        return new DifferentiatedEulerAngles.KIJ();
      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public DifferentiatedEulerAngles createAngles(int caseIndex, double left, double center,
      double right, double leftDerivative, double centerDerivative, double rightDerivative) {
    switch (caseIndex) {
      case 0:
        return new DifferentiatedEulerAngles.IJK(left, center, right, leftDerivative,
            centerDerivative, rightDerivative);
      case 1:
        return new DifferentiatedEulerAngles.JKI(left, center, right, leftDerivative,
            centerDerivative, rightDerivative);
      case 2:
        return new DifferentiatedEulerAngles.KIJ(left, center, right, leftDerivative,
            centerDerivative, rightDerivative);
      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public Class<?> getClass(int caseIndex) {
    switch (caseIndex) {
      case 0:
        return DifferentiatedEulerAngles.IJK.class;
      case 1:
        return DifferentiatedEulerAngles.JKI.class;
      case 2:
        return DifferentiatedEulerAngles.KIJ.class;
      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public AxisTriple getTriple(int caseIndex) {
    switch (caseIndex) {
      case 0:
        return new EulerAnglesTest.AxisTriple(Axis.I, Axis.J, Axis.K);
      case 1:
        return new EulerAnglesTest.AxisTriple(Axis.J, Axis.K, Axis.I);
      case 2:
        return new EulerAnglesTest.AxisTriple(Axis.K, Axis.I, Axis.J);
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
      assertEquals(1.0, angles.getRightAngle(), ANGLE_TOLERANCE);
      assertEquals(0.0, angles.getLeftAngleDerivative(), 0.0);
      assertEquals(0.9950041652780259, angles.getCenterAngleDerivative(), ANGLE_TOLERANCE);
      assertEquals(-0.9, angles.getRightAngleDerivative(), ANGLE_TOLERANCE);
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
      assertEquals(-0.6, angles.getRightAngle(), ANGLE_TOLERANCE);
      assertEquals(0.0, angles.getLeftAngleDerivative(), 0.0);
      assertEquals(0.45359612142557737, angles.getCenterAngleDerivative(), ANGLE_TOLERANCE);
      assertEquals(-0.9, angles.getRightAngleDerivative(), ANGLE_TOLERANCE);
    }
  }

  @Override
  public UnwritableInterval getCenterAngleRange() {
    return CENTER_RANGE;
  }
}
