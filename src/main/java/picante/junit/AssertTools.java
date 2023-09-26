package picante.junit;

import static org.junit.Assert.assertEquals;

import java.awt.geom.Point2D;
import picante.math.intervals.UnwritableInterval;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableMatrixIJ;
import picante.math.vectorspace.UnwritableMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJ;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.mechanics.UnwritableStateTransform;
import picante.mechanics.UnwritableStateVector;
import picante.mechanics.rotations.AxisAndAngle;

public class AssertTools {

  /**
   * Assert two doubles are exactly equal.
   * 
   * @param expected Expected double value.
   * @param actual Actual double value.
   */
  public static void assertEqualDouble(double expected, double actual) {
    assertEquals(expected, actual, 0.0);
  }

  /**
   * Assert two doubles are equivalent.
   * 
   * @param expected Expected double value.
   * @param actual Actual double value.
   */
  public static void assertEquivalentDouble(double expected, double actual) {
    assertEquals(expected, actual, Math.ulp(expected));
  }

  /**
   * Assert two doubles are within the appropriate relative tolerance.
   * 
   * @param message Failure message to emit
   * @param expected Expected double value.
   * @param actual Actual double value.
   * @param delta Required relative tolerance.
   */
  public static void assertRelativeEquality(String message, double expected, double actual,
      double delta) {
    if (expected != actual) {
      assertEquals(message, expected, actual,
          delta * Math.max(Math.abs(expected), Math.abs(actual)));
    }
  }


  /**
   * Assert two doubles are within the appropriate relative tolerance.
   * 
   * @param expected Expected double value.
   * @param actual Actual double value.
   * @param delta Required relative tolerance.
   */
  public static void assertRelativeEquality(double expected, double actual, double delta) {
    if (expected != actual) {
      assertEquals(expected, actual, delta * Math.max(Math.abs(expected), Math.abs(actual)));
    }
  }

  /**
   * Assert two matrices are equivalent.
   * 
   * @param expected Expected matrix.
   * @param actual Actual matrix.
   */
  public static void assertEquivalentMatrix(UnwritableMatrixIJK expected,
      UnwritableMatrixIJK actual) {
    assertEquivalentDouble(expected.getII(), actual.getII());
    assertEquivalentDouble(expected.getIJ(), actual.getIJ());
    assertEquivalentDouble(expected.getIK(), actual.getIK());
    assertEquivalentDouble(expected.getJI(), actual.getJI());
    assertEquivalentDouble(expected.getJJ(), actual.getJJ());
    assertEquivalentDouble(expected.getJK(), actual.getJK());
    assertEquivalentDouble(expected.getKI(), actual.getKI());
    assertEquivalentDouble(expected.getKJ(), actual.getKJ());
    assertEquivalentDouble(expected.getKK(), actual.getKK());
  }

  /**
   * Assert two matrices are exactly equal.
   * 
   * @param expected Expected matrix.
   * @param actual Actual matrix.
   */
  public static void assertEqualMatrix(UnwritableMatrixIJK expected, UnwritableMatrixIJK actual) {
    assertEqualDouble(expected.getII(), actual.getII());
    assertEqualDouble(expected.getIJ(), actual.getIJ());
    assertEqualDouble(expected.getIK(), actual.getIK());
    assertEqualDouble(expected.getJI(), actual.getJI());
    assertEqualDouble(expected.getJJ(), actual.getJJ());
    assertEqualDouble(expected.getJK(), actual.getJK());
    assertEqualDouble(expected.getKI(), actual.getKI());
    assertEqualDouble(expected.getKJ(), actual.getKJ());
    assertEqualDouble(expected.getKK(), actual.getKK());
  }

  public static void assertComponentEquals(String message, UnwritableMatrixIJK expected,
      UnwritableMatrixIJK actual, double delta) {
    assertEquals(message, expected.getII(), actual.getII(), delta);
    assertEquals(message, expected.getJI(), actual.getJI(), delta);
    assertEquals(message, expected.getKI(), actual.getKI(), delta);
    assertEquals(message, expected.getIJ(), actual.getIJ(), delta);
    assertEquals(message, expected.getJJ(), actual.getJJ(), delta);
    assertEquals(message, expected.getKJ(), actual.getKJ(), delta);
    assertEquals(message, expected.getIK(), actual.getIK(), delta);
    assertEquals(message, expected.getJK(), actual.getJK(), delta);
    assertEquals(message, expected.getKK(), actual.getKK(), delta);
  }

  public static void assertComponentEquals(UnwritableMatrixIJK expected, UnwritableMatrixIJK actual,
      double delta) {
    assertEquals(expected.getII(), actual.getII(), delta);
    assertEquals(expected.getJI(), actual.getJI(), delta);
    assertEquals(expected.getKI(), actual.getKI(), delta);
    assertEquals(expected.getIJ(), actual.getIJ(), delta);
    assertEquals(expected.getJJ(), actual.getJJ(), delta);
    assertEquals(expected.getKJ(), actual.getKJ(), delta);
    assertEquals(expected.getIK(), actual.getIK(), delta);
    assertEquals(expected.getJK(), actual.getJK(), delta);
    assertEquals(expected.getKK(), actual.getKK(), delta);
  }

  public static void assertComponentRelativeEquality(UnwritableMatrixIJK expected,
      UnwritableMatrixIJK actual, double delta) {
    assertRelativeEquality(expected.getII(), actual.getII(), delta);
    assertRelativeEquality(expected.getJI(), actual.getJI(), delta);
    assertRelativeEquality(expected.getKI(), actual.getKI(), delta);
    assertRelativeEquality(expected.getIJ(), actual.getIJ(), delta);
    assertRelativeEquality(expected.getJJ(), actual.getJJ(), delta);
    assertRelativeEquality(expected.getKJ(), actual.getKJ(), delta);
    assertRelativeEquality(expected.getIK(), actual.getIK(), delta);
    assertRelativeEquality(expected.getJK(), actual.getJK(), delta);
    assertRelativeEquality(expected.getKK(), actual.getKK(), delta);

  }

  /**
   * Assert two vectors are equivalent.
   * 
   * @param expected Expected vector.
   * @param actual Actual vector.
   */
  public static void assertEquivalentVector(UnwritableVectorIJK expected,
      UnwritableVectorIJK actual) {
    assertEquivalentDouble(expected.getI(), actual.getI());
    assertEquivalentDouble(expected.getJ(), actual.getJ());
    assertEquivalentDouble(expected.getK(), actual.getK());
  }

  /**
   * Assert two vectors are exactly equal.
   * 
   * @param expected Expected vector.
   * @param actual Actual vector.
   */
  public static void assertEqualVector(UnwritableVectorIJK expected, UnwritableVectorIJK actual) {
    assertEqualDouble(expected.getI(), actual.getI());
    assertEqualDouble(expected.getJ(), actual.getJ());
    assertEqualDouble(expected.getK(), actual.getK());
  }

  /**
   * Assert component-wise equality with tolerance.
   * 
   * @param expected Expected vector.
   * @param actual Actual vector.
   * @param delta Component-wise delta factor.
   */
  public static void assertComponentEquals(UnwritableVectorIJK expected, UnwritableVectorIJK actual,
      double delta) {
    assertEquals(expected.getI(), actual.getI(), delta);
    assertEquals(expected.getJ(), actual.getJ(), delta);
    assertEquals(expected.getK(), actual.getK(), delta);
  }

  public static void assertComponentRelativeEquality(UnwritableVectorIJK expected,
      UnwritableVectorIJK actual, double delta) {
    assertRelativeEquality(expected.getI(), actual.getI(), delta);
    assertRelativeEquality(expected.getJ(), actual.getJ(), delta);
    assertRelativeEquality(expected.getK(), actual.getK(), delta);
  }

  public static void assertRotationAngleEquals(UnwritableRotationMatrixIJK expected,
      UnwritableRotationMatrixIJK actual, double toleranceInRadians) {
    assertEquals(0.0,
        Math.abs(new AxisAndAngle(RotationMatrixIJK.mtxm(actual, expected)).getAngle()),
        toleranceInRadians);
  }

  /**
   * Assert two matrices are equivalent.
   * 
   * @param expected Expected matrix.
   * @param actual Actual matrix.
   */
  public static void assertEquivalentMatrix(UnwritableMatrixIJ expected,
      UnwritableMatrixIJ actual) {
    assertEquivalentDouble(expected.getII(), actual.getII());
    assertEquivalentDouble(expected.getIJ(), actual.getIJ());
    assertEquivalentDouble(expected.getJI(), actual.getJI());
    assertEquivalentDouble(expected.getJJ(), actual.getJJ());
  }

  /**
   * Assert two matrices are exactly equal.
   * 
   * @param expected Expected matrix.
   * @param actual Actual matrix.
   */
  public static void assertEqualMatrix(UnwritableMatrixIJ expected, UnwritableMatrixIJ actual) {
    assertEqualDouble(expected.getII(), actual.getII());
    assertEqualDouble(expected.getIJ(), actual.getIJ());
    assertEqualDouble(expected.getJI(), actual.getJI());
    assertEqualDouble(expected.getJJ(), actual.getJJ());
  }

  public static void assertComponentEquals(String message, UnwritableMatrixIJ expected,
      UnwritableMatrixIJ actual, double delta) {
    assertEquals(message, expected.getII(), actual.getII(), delta);
    assertEquals(message, expected.getJI(), actual.getJI(), delta);
    assertEquals(message, expected.getIJ(), actual.getIJ(), delta);
    assertEquals(message, expected.getJJ(), actual.getJJ(), delta);
  }

  public static void assertComponentEquals(UnwritableMatrixIJ expected, UnwritableMatrixIJ actual,
      double delta) {
    assertEquals(expected.getII(), actual.getII(), delta);
    assertEquals(expected.getJI(), actual.getJI(), delta);
    assertEquals(expected.getIJ(), actual.getIJ(), delta);
    assertEquals(expected.getJJ(), actual.getJJ(), delta);
  }

  public static void assertComponentRelativeEquality(UnwritableMatrixIJ expected,
      UnwritableMatrixIJ actual, double delta) {
    assertRelativeEquality(expected.getII(), actual.getII(), delta);
    assertRelativeEquality(expected.getJI(), actual.getJI(), delta);
    assertRelativeEquality(expected.getIJ(), actual.getIJ(), delta);
    assertRelativeEquality(expected.getJJ(), actual.getJJ(), delta);
  }

  /**
   * Assert two vectors are equivalent.
   * 
   * @param expected Expected vector.
   * @param actual Actual vector.
   */
  public static void assertEquivalentVector(UnwritableVectorIJ expected,
      UnwritableVectorIJ actual) {
    assertEquivalentDouble(expected.getI(), actual.getI());
    assertEquivalentDouble(expected.getJ(), actual.getJ());
  }

  /**
   * Assert two vectors are exactly equal.
   * 
   * @param expected Expected vector.
   * @param actual Actual vector.
   */
  public static void assertEqualVector(UnwritableVectorIJ expected, UnwritableVectorIJ actual) {
    assertEqualDouble(expected.getI(), actual.getI());
    assertEqualDouble(expected.getJ(), actual.getJ());
  }

  /**
   * Assert component-wise equality with tolerance.
   * 
   * @param expected Expected vector.
   * @param actual Actual vector.
   * @param delta Component-wise delta factor.
   */
  public static void assertComponentEquals(UnwritableVectorIJ expected, UnwritableVectorIJ actual,
      double delta) {
    assertEquals(expected.getI(), actual.getI(), delta);
    assertEquals(expected.getJ(), actual.getJ(), delta);
  }

  public static void assertComponentRelativeEquality(UnwritableVectorIJ expected,
      UnwritableVectorIJ actual, double delta) {
    assertRelativeEquality(expected.getI(), actual.getI(), delta);
    assertRelativeEquality(expected.getJ(), actual.getJ(), delta);
  }

  public static void assertEquivalentStateTransform(UnwritableStateTransform expected,
      UnwritableStateTransform actual) {
    assertEquivalentMatrix(expected.getRotation(), actual.getRotation());
    assertEquivalentMatrix(expected.getRotationDerivative(), actual.getRotationDerivative());
  }

  public static void assertEqualStateTransform(UnwritableStateTransform expected,
      UnwritableStateTransform actual) {
    assertEqualMatrix(expected.getRotation(), actual.getRotation());
    assertEqualMatrix(expected.getRotationDerivative(), actual.getRotationDerivative());
  }

  public static void assertComponentEquals(UnwritableStateTransform expected,
      UnwritableStateTransform actual, double delta) {
    assertComponentEquals(expected.getRotation(), actual.getRotation(), delta);
    assertComponentEquals(expected.getRotationDerivative(), actual.getRotationDerivative(), delta);
  }

  public static void assertComponentEquals(String message, UnwritableStateTransform expected,
      UnwritableStateTransform actual, double delta) {
    assertComponentEquals(message, expected.getRotation(), actual.getRotation(), delta);
    assertComponentEquals(message, expected.getRotationDerivative(), actual.getRotationDerivative(),
        delta);
  }

  public static void assertComponentRelativeEquality(UnwritableStateTransform expected,
      UnwritableStateTransform actual, double delta) {
    assertComponentRelativeEquality(expected.getRotation(), actual.getRotation(), delta);
    assertComponentRelativeEquality(expected.getRotationDerivative(),
        actual.getRotationDerivative(), delta);
  }

  public static void assertEquivalentStateVector(UnwritableStateVector expected,
      UnwritableStateVector actual) {
    assertEquivalentVector(expected.getPosition(), actual.getPosition());
    assertEquivalentVector(expected.getVelocity(), actual.getVelocity());
  }

  public static void assertEqualStateVector(UnwritableStateVector expected,
      UnwritableStateVector actual) {
    assertEqualVector(expected.getPosition(), actual.getPosition());
    assertEqualVector(expected.getVelocity(), actual.getVelocity());
  }

  public static void assertComponentEquals(UnwritableStateVector expected,
      UnwritableStateVector actual, double delta) {
    assertComponentEquals(expected.getPosition(), actual.getPosition(), delta);
    assertComponentEquals(expected.getVelocity(), actual.getVelocity(), delta);
  }

  public static void assertComponentRelativeEquality(UnwritableStateVector expected,
      UnwritableStateVector actual, double delta) {
    assertComponentRelativeEquality(expected.getPosition(), actual.getPosition(), delta);
    assertComponentRelativeEquality(expected.getVelocity(), actual.getVelocity(), delta);
  }

  public static void assertEqualInterval(UnwritableInterval expected, UnwritableInterval actual,
      double tolerance) {
    assertEquals(expected.getBegin(), actual.getBegin(), tolerance);
    assertEquals(expected.getEnd(), actual.getEnd(), tolerance);
  }

  public static void assertComponentEquals(Point2D expected, Point2D actual, double delta) {
    assertEquals(expected.getX(), actual.getX(), delta);
    assertEquals(expected.getY(), actual.getY(), delta);
  }

}
