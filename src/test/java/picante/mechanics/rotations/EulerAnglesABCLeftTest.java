package picante.mechanics.rotations;

import static picante.mechanics.rotations.EulerAngles.Axis.I;
import static picante.mechanics.rotations.EulerAngles.Axis.J;
import static picante.mechanics.rotations.EulerAngles.Axis.K;
import picante.math.intervals.UnwritableInterval;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;

/**
 * Test harness for the Axis, AxisPrime, AxisDoublePrime case when the axes are related in a right
 * handed fashion.
 */
public class EulerAnglesABCLeftTest extends EulerAnglesTest {

  private static final UnwritableInterval CENTER_RANGE =
      new UnwritableInterval(-Math.PI / 2.0, Math.PI / 2.0);

  @Override
  public EulerAngles createAngles(int caseIndex, double left, double center, double right) {
    switch (caseIndex) {
      case 0:
        return new EulerAngles.IKJ(left, center, right);
      case 1:
        return new EulerAngles.JIK(left, center, right);
      case 2:
        return new EulerAngles.KJI(left, center, right);
      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public EulerAngles createAngles(int caseIndex) {
    switch (caseIndex) {
      case 0:
        return new EulerAngles.IKJ();
      case 1:
        return new EulerAngles.JIK();
      case 2:
        return new EulerAngles.KJI();
      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public EulerAngles createAngles(int caseIndex, UnwritableRotationMatrixIJK matrix) {
    switch (caseIndex) {
      case 0:
        return new EulerAngles.IKJ(matrix);
      case 1:
        return new EulerAngles.JIK(matrix);
      case 2:
        return new EulerAngles.KJI(matrix);
      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public AxisTriple getTriple(int caseIndex) {
    switch (caseIndex) {
      case 0:
        return new AxisTriple(I, K, J);
      case 1:
        return new AxisTriple(J, I, K);
      case 2:
        return new AxisTriple(K, J, I);
      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public UnwritableInterval getCenterAngleRange() {
    return CENTER_RANGE;
  }

  @Override
  public int getNumberOfCases() {
    return 3;
  }

  @Override
  public double expectedDegenerateAngleBeginCase(EulerAngles angles) {
    return angles.getRightAngle() - angles.getLeftAngle();
  }

  @Override
  public double expectedDegenerateAngleEndCase(EulerAngles angles) {
    return angles.getRightAngle() + angles.getLeftAngle();
  }

}
