package picante.mechanics.rotations;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import picante.math.intervals.UnwritableInterval;
import picante.mechanics.StateTransform;
import picante.mechanics.rotations.EulerAngles.Axis;
import picante.mechanics.rotations.EulerAnglesTest.AxisTriple;

public class DifferentiatedEulerAnglesABATest extends DifferentiatedEulerAnglesTest {

  private static final UnwritableInterval CENTER_RANGE = new UnwritableInterval(0.0, Math.PI);

  @Override
  public int getNumberOfCases() {
    return 6;
  }

  @Override
  public DifferentiatedEulerAngles createAngles(int caseIndex) {
    switch (caseIndex) {
      case 0:
        return new DifferentiatedEulerAngles.IJI();
      case 1:
        return new DifferentiatedEulerAngles.IKI();
      case 2:
        return new DifferentiatedEulerAngles.JIJ();
      case 3:
        return new DifferentiatedEulerAngles.JKJ();
      case 4:
        return new DifferentiatedEulerAngles.KIK();
      case 5:
        return new DifferentiatedEulerAngles.KJK();
      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public Class<?> getClass(int caseIndex) {
    switch (caseIndex) {
      case 0:
        return DifferentiatedEulerAngles.IJI.class;
      case 1:
        return DifferentiatedEulerAngles.IKI.class;
      case 2:
        return DifferentiatedEulerAngles.JIJ.class;
      case 3:
        return DifferentiatedEulerAngles.JKJ.class;
      case 4:
        return DifferentiatedEulerAngles.KIK.class;
      case 5:
        return DifferentiatedEulerAngles.KJK.class;

      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public DifferentiatedEulerAngles createAngles(int caseIndex, double left, double center,
      double right, double leftDerivative, double centerDerivative, double rightDerivative) {
    switch (caseIndex) {
      case 0:
        return new DifferentiatedEulerAngles.IJI(left, center, right, leftDerivative,
            centerDerivative, rightDerivative);
      case 1:
        return new DifferentiatedEulerAngles.IKI(left, center, right, leftDerivative,
            centerDerivative, rightDerivative);
      case 2:
        return new DifferentiatedEulerAngles.JIJ(left, center, right, leftDerivative,
            centerDerivative, rightDerivative);
      case 3:
        return new DifferentiatedEulerAngles.JKJ(left, center, right, leftDerivative,
            centerDerivative, rightDerivative);
      case 4:
        return new DifferentiatedEulerAngles.KIK(left, center, right, leftDerivative,
            centerDerivative, rightDerivative);
      case 5:
        return new DifferentiatedEulerAngles.KJK(left, center, right, leftDerivative,
            centerDerivative, rightDerivative);
      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public AxisTriple getTriple(int caseIndex) {
    switch (caseIndex) {
      case 0:
        return new EulerAnglesTest.AxisTriple(Axis.I, Axis.J, Axis.I);
      case 1:
        return new EulerAnglesTest.AxisTriple(Axis.I, Axis.K, Axis.I);
      case 2:
        return new EulerAnglesTest.AxisTriple(Axis.J, Axis.I, Axis.J);
      case 3:
        return new EulerAnglesTest.AxisTriple(Axis.J, Axis.K, Axis.J);
      case 4:
        return new EulerAnglesTest.AxisTriple(Axis.K, Axis.I, Axis.K);
      case 5:
        return new EulerAnglesTest.AxisTriple(Axis.K, Axis.J, Axis.K);

      default:
        throw new UnsupportedOperationException();
    }
  }

  @SuppressWarnings("unused")
  @Override
  public double computeU(Axis left, Axis center, double centerAngle) {
    return Math.cos(centerAngle);
  }

  @Override
  public double computeV(Axis left, Axis center, double centerAngle) {
    return left.getCrossSign(center) * Math.sin(centerAngle);
  }

  private double getDegenerateCenterAngle() {
    return Math.PI;
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
