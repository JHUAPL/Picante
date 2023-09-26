package picante.mechanics.rotations;

import static picante.mechanics.rotations.EulerAngles.Axis.I;
import static picante.mechanics.rotations.EulerAngles.Axis.J;
import static picante.mechanics.rotations.EulerAngles.Axis.K;
import picante.math.intervals.UnwritableInterval;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;

/**
 * Test harness for the Axis, AxisPrime, AxisDoublePrime form of Euler decomposition where the axes
 * are in a left-handed relationship.
 */
public class EulerAnglesABCRightTest extends EulerAnglesTest {

  private static final UnwritableInterval CENTER_RANGE =
      new UnwritableInterval(-Math.PI / 2.0, Math.PI / 2.0);

  @Override
  public EulerAngles createAngles(int caseIndex, double left, double center, double right) {
    switch (caseIndex) {
      case 0:
        return new EulerAngles.IJK(left, center, right);
      case 1:
        return new EulerAngles.JKI(left, center, right);
      case 2:
        return new EulerAngles.KIJ(left, center, right);
      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public EulerAngles createAngles(int caseIndex) {
    switch (caseIndex) {
      case 0:
        return new EulerAngles.IJK();
      case 1:
        return new EulerAngles.JKI();
      case 2:
        return new EulerAngles.KIJ();
      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public EulerAngles createAngles(int caseIndex, UnwritableRotationMatrixIJK matrix) {
    switch (caseIndex) {
      case 0:
        return new EulerAngles.IJK(matrix);
      case 1:
        return new EulerAngles.JKI(matrix);
      case 2:
        return new EulerAngles.KIJ(matrix);
      default:
        throw new UnsupportedOperationException();
    }
  }

  @Override
  public AxisTriple getTriple(int caseIndex) {
    switch (caseIndex) {
      case 0:
        return new AxisTriple(I, J, K);
      case 1:
        return new AxisTriple(J, K, I);
      case 2:
        return new AxisTriple(K, I, J);
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
    return angles.getRightAngle() + angles.getLeftAngle();
  }

  @Override
  public double expectedDegenerateAngleEndCase(EulerAngles angles) {
    return angles.getRightAngle() - angles.getLeftAngle();
  }

}
