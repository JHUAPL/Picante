package picante.math;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static picante.junit.AssertTools.assertRelativeEquality;
import org.junit.Ignore;
import org.junit.Test;
import net.jafama.FastMath;

public class PicanteMathTest {

  private final int num = 100;
  private final double dx = 0.1;

  private final double TOL = 1.78E-15;
  private final double RTOL = 2.46E-14;

  @Test
  public void testUseJdkMath() {

    double x = -0.24738;

    double diff1 = Math.sin(x) - PicanteMath.sin(x);
    double diff2 = FastMath.sin(x) - PicanteMath.sin(x);

    // initially, CrucibleMath = FastMath
    assertFalse(diff1 == 0);
    assertTrue(diff2 == 0);

    PicanteMath.useJdkMath();

    diff1 = Math.sin(x) - PicanteMath.sin(x);
    diff2 = FastMath.sin(x) - PicanteMath.sin(x);

    // Now, CrucibleMath = Math
    assertTrue(diff1 == 0);
    assertFalse(diff2 == 0);

    double y = 0.8913573;

    int i = -3;
    int j = -4;

    // Now test all the methods to make sure you didn't make a mistake
    assertEquals(Math.sin(x), PicanteMath.sin(x), 0.0);
    assertEquals(Math.cos(x), PicanteMath.cos(x), 0.0);
    assertEquals(Math.tan(x), PicanteMath.tan(x), 0.0);
    assertEquals(Math.asin(x), PicanteMath.asin(x), 0.0);
    assertEquals(Math.acos(x), PicanteMath.acos(x), 0.0);
    assertEquals(Math.atan(x), PicanteMath.atan(x), 0.0);
    assertEquals(Math.toRadians(x), PicanteMath.toRadians(x), 0.0);
    assertEquals(Math.toDegrees(x), PicanteMath.toDegrees(x), 0.0);
    assertEquals(Math.exp(x), PicanteMath.exp(x), 0.0);
    assertEquals(Math.log(x), PicanteMath.log(x), 0.0);
    assertEquals(Math.log10(x), PicanteMath.log10(x), 0.0);
    assertEquals(Math.sqrt(x), PicanteMath.sqrt(x), 0.0);
    assertEquals(Math.cbrt(x), PicanteMath.cbrt(x), 0.0);
    assertEquals(Math.IEEEremainder(x, y), PicanteMath.IEEEremainder(x, y), 0.0);
    assertEquals(Math.ceil(x), PicanteMath.ceil(x), 0.0);
    assertEquals(Math.floor(x), PicanteMath.floor(x), 0.0);
    assertEquals(Math.atan2(y, x), PicanteMath.atan2(y, x), 0.0);
    assertEquals(Math.pow(x, y), PicanteMath.pow(x, y), 0.0);
    assertEquals(Math.round(x), PicanteMath.round(x), 0.0);
    assertEquals(Math.abs(i), PicanteMath.abs(i), 0.0);
    assertEquals(Math.abs(x), PicanteMath.abs(x), 0.0);
    assertEquals(Math.max(i, j), PicanteMath.max(i, j));
    assertEquals(Math.max(x, y), PicanteMath.max(x, y), 0.0);
    assertEquals(Math.min(i, j), PicanteMath.min(i, j));
    assertEquals(Math.min(x, y), PicanteMath.min(x, y), 0.0);
    assertEquals(Math.ulp(x), PicanteMath.ulp(x), 0.0);
    assertEquals(Math.signum(x), PicanteMath.signum(x), 0.0);
    assertEquals(Math.sinh(x), PicanteMath.sinh(x), 0.0);
    assertEquals(Math.cosh(x), PicanteMath.cosh(x), 0.0);
    assertEquals(Math.tanh(x), PicanteMath.tanh(x), 0.0);
    assertEquals(Math.hypot(x, y), PicanteMath.hypot(x, y), 0.0);

  }

  @Test
  public void testSin() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.sin(x);
      double c = PicanteMath.sin(x);
      double f = FastMath.sin(x);

      assertEquals(j, c, TOL);
      assertRelativeEquality(j, c, RTOL);
      assertEquals(f, c, 0.0);
    }

  }

  @Test
  public void testCos() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.cos(x);
      double c = PicanteMath.cos(x);
      double f = FastMath.cos(x);

      assertEquals(j, c, TOL);
      assertRelativeEquality(j, c, RTOL);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  public void testTan() {

    PicanteMath.useFastMath();

    double TOL = 1.8E-14;

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.tan(x);
      double c = PicanteMath.tan(x);
      double f = FastMath.tan(x);

      assertEquals(j, c, TOL);
      assertRelativeEquality(j, c, RTOL);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  public void testAsin() {

    PicanteMath.useFastMath();

    double dx = 0.011;

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.asin(x);
      double c = PicanteMath.asin(x);
      double f = FastMath.asin(x);

      assertEquals(j, c, TOL);
      assertRelativeEquality(j, c, RTOL);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  public void testAcos() {

    PicanteMath.useFastMath();

    double dx = 0.011;

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.acos(x);
      double c = PicanteMath.acos(x);
      double f = FastMath.acos(x);

      assertEquals(j, c, TOL);
      assertRelativeEquality(j, c, RTOL);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  public void testAtan() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.atan(x);
      double c = PicanteMath.atan(x);
      double f = FastMath.atan(x);

      assertEquals(j, c, TOL);
      assertRelativeEquality(j, c, RTOL);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  public void testToRadians() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.toRadians(x);
      double c = PicanteMath.toRadians(x);
      double f = FastMath.toRadians(x);

      assertEquals(j, c, TOL);
      assertRelativeEquality(j, c, RTOL);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  public void testToDegrees() {

    PicanteMath.useFastMath();

    double TOL = 2.3E-13;

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.toDegrees(x);
      double c = PicanteMath.toDegrees(x);
      double f = FastMath.toDegrees(x);

      assertEquals(j, c, TOL);
      assertRelativeEquality(j, c, RTOL);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  public void testExp() {

    PicanteMath.useFastMath();

    double TOL = 1.9E-12;

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.exp(x);
      double c = PicanteMath.exp(x);
      double f = FastMath.exp(x);

      assertEquals(j, c, TOL);
      assertRelativeEquality(j, c, RTOL);
      assertEquals(f, c, 0.0);
    }

  }

  @Test
  public void testLog() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.log(x);
      double c = PicanteMath.log(x);
      double f = FastMath.log(x);

      assertEquals(j, c, TOL);
      assertRelativeEquality(j, c, RTOL);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  public void testLog10() {

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.log10(x);
      double c = PicanteMath.log10(x);
      double f = FastMath.log10(x);

      assertEquals(j, c, TOL);
      assertRelativeEquality(j, c, RTOL);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  public void testSqrt() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.sqrt(x);
      double c = PicanteMath.sqrt(x);
      double f = FastMath.sqrt(x);

      assertEquals(j, c, TOL);
      assertRelativeEquality(j, c, RTOL);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  public void testCbrt() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.cbrt(x);
      double c = PicanteMath.cbrt(x);
      double f = FastMath.cbrt(x);

      assertEquals(j, c, TOL);
      assertRelativeEquality(j, c, RTOL);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  public void testIEEEremainder() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {
      for (int jj = -num; jj <= num; jj++) {

        double x = i * dx;
        double y = jj * dx;

        double j = Math.IEEEremainder(y, x);
        double c = PicanteMath.IEEEremainder(y, x);
        double f = FastMath.IEEEremainder(y, x);

        assertEquals(j, c, TOL);
        assertRelativeEquality(j, c, RTOL);
        assertEquals(f, c, 0.0);
      }
    }
  }

  @Test
  public void testCeil() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.ceil(x);
      double c = PicanteMath.ceil(x);
      double f = FastMath.ceil(x);

      assertEquals(j, c, 0.0);
      assertRelativeEquality(j, c, 0.0);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  public void testFloor() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.floor(x);
      double c = PicanteMath.floor(x);
      double f = FastMath.floor(x);

      assertEquals(j, c, 0.0);
      assertRelativeEquality(j, c, 0.0);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  public void testAtan2() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {
      for (int jj = -num; jj <= num; jj++) {

        double x = i * dx;
        double y = jj * dx;

        double j = Math.atan2(y, x);
        double c = PicanteMath.atan2(y, x);
        double f = FastMath.atan2(y, x);

        assertEquals(j, c, TOL);
        assertRelativeEquality(j, c, RTOL);
        assertEquals(f, c, 0.0);
      }
    }
  }

  @Test
  public void testPow() {

    PicanteMath.useFastMath();

    double TOL = 4.1E-5;

    for (int i = -num; i <= num; i++) {
      for (int jj = -num; jj <= num; jj++) {

        double x = i * dx;
        double y = jj * dx;

        double j = Math.pow(x, y);
        double c = PicanteMath.pow(x, y);
        double f = FastMath.pow(x, y);

        assertEquals(j, c, TOL);
        assertRelativeEquality(j, c, RTOL);
        assertEquals(f, c, 0.0);
      }
    }
  }

  @Test
  public void testRound() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.round(x);
      double c = PicanteMath.round(x);
      double f = FastMath.round(x);

      assertEquals(j, c, 0.0);
      assertRelativeEquality(j, c, 0.0);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  public void testAbsInt() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {

      int j = Math.abs(i);
      int c = PicanteMath.abs(i);

      assertEquals(j, c);
    }
  }

  @Test
  public void testAbsDouble() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.abs(x);
      double c = PicanteMath.abs(x);
      double f = FastMath.abs(x);

      assertEquals(j, c, 0.0);
      assertRelativeEquality(j, c, 0.0);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  public void testMaxIntInt() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {
      for (int jj = -num; jj <= num; jj++) {

        int j = Math.max(i, jj);
        int c = PicanteMath.max(i, jj);

        assertEquals(j, c);
      }
    }
  }

  @Test
  public void testMaxDoubleDouble() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {
      for (int jj = -num; jj <= num; jj++) {

        double x = i * dx;
        double y = jj * dx;

        double j = Math.max(x, y);
        double c = PicanteMath.max(x, y);
        double f = FastMath.max(x, y);

        assertEquals(j, c, 0.0);
        assertRelativeEquality(j, c, 0.0);
        assertEquals(f, c, 0.0);
      }
    }
  }

  @Test
  public void testMinIntInt() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {
      for (int jj = -num; jj <= num; jj++) {

        int j = Math.min(i, jj);
        int c = PicanteMath.min(i, jj);

        assertEquals(j, c);
      }
    }
  }

  @Test
  public void testMinDoubleDouble() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {
      for (int jj = -num; jj <= num; jj++) {

        double x = i * dx;
        double y = jj * dx;

        double j = Math.min(x, y);
        double c = PicanteMath.min(x, y);
        double f = FastMath.min(x, y);

        assertEquals(j, c, 0.0);
        assertRelativeEquality(j, c, 0.0);
        assertEquals(f, c, 0.0);
      }
    }
  }

  @Test
  public void testUlp() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.ulp(x);
      double c = PicanteMath.ulp(x);
      double f = FastMath.ulp(x);

      assertEquals(j, c, 0.0);
      assertRelativeEquality(j, c, 0.0);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  public void testSignum() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.signum(x);
      double c = PicanteMath.signum(x);
      double f = FastMath.signum(x);

      assertEquals(j, c, 0.0);
      assertRelativeEquality(j, c, 0.0);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  public void testSinh() {

    PicanteMath.useFastMath();

    double TOL = 1.0E-12;

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.sinh(x);
      double c = PicanteMath.sinh(x);
      double f = FastMath.sinh(x);

      assertEquals(j, c, TOL);
      assertRelativeEquality(j, c, RTOL);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  public void testCosh() {

    PicanteMath.useFastMath();

    double TOL = 1.0E-12;

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.cosh(x);
      double c = PicanteMath.cosh(x);
      double f = FastMath.cosh(x);

      assertEquals(j, c, TOL);
      assertRelativeEquality(j, c, RTOL);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  public void testTanh() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {

      double x = i * dx;

      double j = Math.tanh(x);
      double c = PicanteMath.tanh(x);
      double f = FastMath.tanh(x);

      assertEquals(j, c, TOL);
      assertRelativeEquality(j, c, RTOL);
      assertEquals(f, c, 0.0);
    }
  }

  @Test
  @Ignore
  public void testAcosh() {
    fail("Not yet implemented");
  }

  @Test
  @Ignore
  public void testAsinh() {
    fail("Not yet implemented");
  }

  @Test
  @Ignore
  public void testAtanh() {
    fail("Not yet implemented");
  }

  @Test
  public void testHypot() {

    PicanteMath.useFastMath();

    for (int i = -num; i <= num; i++) {
      for (int jj = -num; jj <= num; jj++) {

        double x = i * dx;
        double y = jj * dx;

        double j = Math.hypot(x, y);
        double c = PicanteMath.hypot(x, y);
        double f = FastMath.hypot(x, y);

        assertEquals(j, c, TOL);
        assertRelativeEquality(j, c, RTOL);
        assertEquals(f, c, 0.0);
      }
    }
  }

  @Test
  @Ignore
  public void testRint() {
    fail("Not yet implemented");
  }

}
