package picante.mechanics.rotations;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.mechanics.rotations.EulerAngles.Axis.I;
import static picante.mechanics.rotations.EulerAngles.Axis.J;
import static picante.mechanics.rotations.EulerAngles.Axis.K;
import org.junit.Test;
import picante.math.PicanteMath;
import picante.math.vectorspace.RotationMatrixIJK;

public class EulerAnglesAxisTest {

  private final static double THIRDPI = Math.PI / 3.0;

  @Test
  public void testAxisEnumerationBufferReturnValue() {
    RotationMatrixIJK matrix = new RotationMatrixIJK();
    for (EulerAngles.Axis axis : EulerAngles.Axis.values()) {
      RotationMatrixIJK result = axis.getRotation(Math.PI / 10.0, matrix);
      assertSame(matrix, result);
    }
  }

  @Test
  public void testIgetRotation() {
    RotationMatrixIJK matrix = new RotationMatrixIJK(1.0, 0.0, 0.0, 0.0, PicanteMath.cos(THIRDPI),
        -PicanteMath.sin(THIRDPI), 0.0, PicanteMath.sin(THIRDPI), PicanteMath.cos(THIRDPI));
    // this matrix is constructed with the JDK math functions
    RotationMatrixIJK matrixMath = new RotationMatrixIJK(1.0, 0.0, 0.0, 0.0, Math.cos(THIRDPI),
        -Math.sin(THIRDPI), 0.0, Math.sin(THIRDPI), Math.cos(THIRDPI));
    RotationMatrixIJK buffer = new RotationMatrixIJK();
    RotationMatrixIJK result = I.getRotation(THIRDPI, buffer);

    assertSame(result, buffer);
    assertEquals(matrix, result);
    // comparison with the JDK math
    assertComponentEquals(matrixMath, result, 1.2E-16);
  }

  @Test
  public void testIgetCompletingAxis() {
    assertEquals(K, I.getCompletingAxis(J));
    assertEquals(J, I.getCompletingAxis(K));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testISelfCompletingAxisException() {
    I.getCompletingAxis(I);
  }

  @Test
  public void testIgetCrossSign() {
    assertEquals(1.0, I.getCrossSign(J), 0.0);
    assertEquals(-1.0, I.getCrossSign(K), 0.0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIgetCrossSignSelfException() {
    I.getCrossSign(I);
  }

  @Test
  public void testJgetRotation() {
    RotationMatrixIJK matrix =
        new RotationMatrixIJK(PicanteMath.cos(THIRDPI), 0.0, PicanteMath.sin(THIRDPI), 0.0, 1.0,
            0.0, -PicanteMath.sin(THIRDPI), 0.0, PicanteMath.cos(THIRDPI));
    // this matrix is constructed with the JDK math functions
    RotationMatrixIJK matrixMath = new RotationMatrixIJK(Math.cos(THIRDPI), 0.0, Math.sin(THIRDPI),
        0.0, 1.0, 0.0, -Math.sin(THIRDPI), 0.0, Math.cos(THIRDPI));
    RotationMatrixIJK buffer = new RotationMatrixIJK();
    RotationMatrixIJK result = J.getRotation(THIRDPI, buffer);

    assertSame(result, buffer);
    assertEquals(matrix, result);
    // comparison with the JDK math
    assertComponentEquals(matrixMath, result, 1.2E-16);
  }

  @Test
  public void testJgetCompletingAxis() {
    assertEquals(K, J.getCompletingAxis(I));
    assertEquals(I, J.getCompletingAxis(K));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testJSelfCompletingAxisException() {
    J.getCompletingAxis(J);
  }

  @Test
  public void testJgetCrossSign() {
    assertEquals(1.0, J.getCrossSign(K), 0.0);
    assertEquals(-1.0, J.getCrossSign(I), 0.0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testJgetCrossSignSelfException() {
    J.getCrossSign(J);
  }

  @Test
  public void testKgetRotation() {
    RotationMatrixIJK matrix =
        new RotationMatrixIJK(PicanteMath.cos(THIRDPI), -PicanteMath.sin(THIRDPI), 0.0,
            PicanteMath.sin(THIRDPI), PicanteMath.cos(THIRDPI), 0.0, 0.0, 0.0, 1.0);
    RotationMatrixIJK matrixMath = new RotationMatrixIJK(Math.cos(THIRDPI), -Math.sin(THIRDPI), 0.0,
        Math.sin(THIRDPI), Math.cos(THIRDPI), 0.0, 0.0, 0.0, 1.0);
    RotationMatrixIJK buffer = new RotationMatrixIJK();
    RotationMatrixIJK result = K.getRotation(THIRDPI, buffer);

    assertSame(result, buffer);
    assertEquals(matrix, result);
    // comparison with the JDK math
    assertComponentEquals(matrixMath, result, 1.2E-16);
  }

  @Test
  public void testKgetCompletingAxis() {
    assertEquals(J, K.getCompletingAxis(I));
    assertEquals(I, K.getCompletingAxis(J));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testKSelfCompletingAxisException() {
    K.getCompletingAxis(K);
  }

  @Test
  public void testKgetCrossSign() {
    assertEquals(1.0, K.getCrossSign(I), 0.0);
    assertEquals(-1.0, K.getCrossSign(J), 0.0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testKgetCrossSignSelfException() {
    K.getCrossSign(K);
  }

}
