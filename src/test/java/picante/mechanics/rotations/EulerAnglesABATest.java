package picante.mechanics.rotations;

import static picante.mechanics.rotations.EulerAngles.Axis.I;
import static picante.mechanics.rotations.EulerAngles.Axis.J;
import static picante.mechanics.rotations.EulerAngles.Axis.K;
import picante.math.intervals.UnwritableInterval;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;

/**
 * Test harness that exercises the 6 decompositions of the Axis, AxisPrime, Axis form.
 */
public class EulerAnglesABATest extends EulerAnglesTest {

  private static final UnwritableInterval CENTER_RANGE = new UnwritableInterval(0.0, Math.PI);

  @Override
  public EulerAngles createAngles(int caseIndex, double left, double center, double right) {
    switch (caseIndex) {
      case 0:
        return new EulerAngles.IJI(left, center, right);
      case 1:
        return new EulerAngles.IKI(left, center, right);
      case 2:
        return new EulerAngles.JIJ(left, center, right);
      case 3:
        return new EulerAngles.JKJ(left, center, right);
      case 4:
        return new EulerAngles.KIK(left, center, right);
      case 5:
        return new EulerAngles.KJK(left, center, right);
      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public EulerAngles createAngles(int caseIndex) {
    switch (caseIndex) {
      case 0:
        return new EulerAngles.IJI();
      case 1:
        return new EulerAngles.IKI();
      case 2:
        return new EulerAngles.JIJ();
      case 3:
        return new EulerAngles.JKJ();
      case 4:
        return new EulerAngles.KIK();
      case 5:
        return new EulerAngles.KJK();
      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public EulerAngles createAngles(int caseIndex, UnwritableRotationMatrixIJK matrix) {
    switch (caseIndex) {
      case 0:
        return new EulerAngles.IJI(matrix);
      case 1:
        return new EulerAngles.IKI(matrix);
      case 2:
        return new EulerAngles.JIJ(matrix);
      case 3:
        return new EulerAngles.JKJ(matrix);
      case 4:
        return new EulerAngles.KIK(matrix);
      case 5:
        return new EulerAngles.KJK(matrix);
      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public AxisTriple getTriple(int caseIndex) {
    switch (caseIndex) {
      case 0:
        return new AxisTriple(I, J, I);
      case 1:
        return new AxisTriple(I, K, I);
      case 2:
        return new AxisTriple(J, I, J);
      case 3:
        return new AxisTriple(J, K, J);
      case 4:
        return new AxisTriple(K, I, K);
      case 5:
        return new AxisTriple(K, J, K);
      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public int getNumberOfCases() {
    return 6;
  }

  @Override
  public UnwritableInterval getCenterAngleRange() {
    return CENTER_RANGE;
  }

  @Override
  public double expectedDegenerateAngleBeginCase(EulerAngles angles) {
    return angles.getLeftAngle() + angles.getRightAngle();
  }

  @Override
  public double expectedDegenerateAngleEndCase(EulerAngles angles) {
    return angles.getRightAngle() - angles.getLeftAngle();
  }

}
