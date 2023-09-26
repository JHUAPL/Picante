package picante.math.functions;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static picante.junit.AssertTools.assertRelativeEquality;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;


public class LagrangePolynomialTest {

  private static final double TOLERANCE = 1.0E-13;
  private static final double TOLERANCE_TIGHT = 1.0E-14;

  private LagrangePolynomial polynomial;
  private double[] buffer;
  private double[] xvals;
  private double[] yvals;
  private int nPoints;

  @Before
  public void setUp() throws Exception {

    nPoints = 5;

    xvals = new double[nPoints];
    yvals = new double[nPoints];

    for (int i = 0; i < nPoints; i++) {
      xvals[i] = i;
      yvals[i] = Math.sin(Math.PI / 10.0 * xvals[i]);
    }

    polynomial = new LagrangePolynomial(nPoints, xvals, yvals);
    buffer = new double[2];
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testLagrangePolynomialXvalsException() {
    new LagrangePolynomial(10, new double[8], new double[10]);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testLagrangePolynomialYvalsException() {
    new LagrangePolynomial(10, new double[10], new double[5]);
  }

  @Test
  public void testLagrangePolynomial() {
    LagrangePolynomial p = new LagrangePolynomial();
    assertEquals(0.0, p.evaluate(0.0), 0.0);
    assertEquals(0.0, p.evaluate(1.0), 0.0);
    assertEquals(0.0, p.evaluate(Double.MAX_VALUE), 0.0);
    assertEquals(0.0, p.evaluate(Double.MIN_VALUE), 0.0);
  }

  @Test
  public void testGetNumberOfCoefficients() {
    assertEquals(nPoints, polynomial.getNumberOfCoefficients());
  }

  @Test
  public void testGetDegree() {
    assertEquals(nPoints - 1, polynomial.getDegree());
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetDataPointsXvalsException() {
    double[] xvals = new double[nPoints - 2];
    double[] yvals = new double[nPoints];
    polynomial.getDataPoints(xvals, yvals);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetDataPointsYvalsException() {
    double[] xvals = new double[nPoints];
    double[] yvals = new double[nPoints - 1];
    polynomial.getDataPoints(xvals, yvals);
  }

  @Test
  public void testGetDataPoints() {
    double[] rXvals = new double[nPoints];
    double[] rYvals = new double[nPoints];
    polynomial.getDataPoints(rXvals, rYvals);

    /*
     * These arrays should be identical to the bit.
     */
    assertTrue(Arrays.equals(xvals, rXvals));
    assertTrue(Arrays.equals(yvals, rYvals));
  }

  @Test
  public void testSetDataPoints() {

    double[] xvals = new double[1];
    double[] yvals = new double[1];

    LagrangePolynomial lpoly = new LagrangePolynomial(1, xvals, yvals);
    assertEquals(0.0, lpoly.evaluate(1.0), 0.0);
    lpoly.evaluate(1.0, buffer);
    assertEquals(0.0, buffer[0], 0.0);
    assertEquals(0.0, buffer[1], 0.0);

    xvals = new double[7];
    yvals = new double[7];
    for (int i = 0; i < 7; i++) {
      xvals[i] = i;
      yvals[i] = i * i;
    }

    lpoly.setDataPoints(7, xvals, yvals);
    assertRelativeEquality(2.25, lpoly.evaluate(1.5), TOLERANCE_TIGHT);
    assertRelativeEquality(37.60320230102501, lpoly.evaluate(6.132145), TOLERANCE_TIGHT);
  }

  @Test
  public void testEvaluateDoubleDoubleArray() {
    double[] result = polynomial.evaluate(-1.0, buffer);
    assertSame(result, buffer);
    assertRelativeEquality(-0.30634587640432615, buffer[0], TOLERANCE_TIGHT);
    assertRelativeEquality(0.29262901384706475, buffer[1], TOLERANCE_TIGHT);
    result = polynomial.evaluate(-0.5, buffer);
    assertSame(result, buffer);
    assertRelativeEquality(-0.15578432178190033, buffer[0], TOLERANCE_TIGHT);
    assertRelativeEquality(0.3079522067992623, buffer[1], TOLERANCE_TIGHT);
    result = polynomial.evaluate(0.0, buffer);
    assertSame(result, buffer);
    assertRelativeEquality(0.0, buffer[0], TOLERANCE_TIGHT);
    assertRelativeEquality(0.31363741738184503, buffer[1], TOLERANCE_TIGHT);
    result = polynomial.evaluate(0.5, buffer);
    assertSame(result, buffer);
    assertRelativeEquality(0.15636410010189275, buffer[0], TOLERANCE_TIGHT);
    assertRelativeEquality(0.3103886549761158, buffer[1], TOLERANCE_TIGHT);
    result = polynomial.evaluate(1.0, buffer);
    assertSame(result, buffer);
    assertRelativeEquality(0.3090169943749474, buffer[0], TOLERANCE_TIGHT);
    assertRelativeEquality(0.298909928963376, buffer[1], TOLERANCE_TIGHT);
    result = polynomial.evaluate(1.5, buffer);
    assertSame(result, buffer);
    assertRelativeEquality(0.4540197033609839, buffer[0], TOLERANCE_TIGHT);
    assertRelativeEquality(0.27990524872492695, buffer[1], TOLERANCE_TIGHT);
    result = polynomial.evaluate(2.0, buffer);
    assertSame(result, buffer);
    assertRelativeEquality(0.5877852522924731, buffer[0], TOLERANCE_TIGHT);
    assertRelativeEquality(0.25407862364207057, buffer[1], TOLERANCE_TIGHT);
    result = polynomial.evaluate(2.5, buffer);
    assertSame(result, buffer);
    assertRelativeEquality(0.7070786710925367, buffer[0], TOLERANCE_TIGHT);
    assertRelativeEquality(0.22213406309610834, buffer[1], TOLERANCE_TIGHT);
    result = polynomial.evaluate(3.0, buffer);
    assertSame(result, buffer);
    assertRelativeEquality(0.8090169943749475, buffer[0], TOLERANCE_TIGHT);
    assertRelativeEquality(0.18477557646834195, buffer[1], TOLERANCE_TIGHT);
    result = polynomial.evaluate(3.5, buffer);
    assertSame(result, buffer);
    assertRelativeEquality(0.8910692614441282, buffer[0], TOLERANCE_TIGHT);
    assertRelativeEquality(0.14270717314007295, buffer[1], TOLERANCE_TIGHT);
    result = polynomial.evaluate(4.0, buffer);
    assertSame(result, buffer);
    assertRelativeEquality(0.9510565162951535, buffer[0], TOLERANCE_TIGHT);
    assertRelativeEquality(0.09663286249260299, buffer[1], TOLERANCE_TIGHT);
    result = polynomial.evaluate(4.5, buffer);
    assertSame(result, buffer);
    assertRelativeEquality(0.9871518076137477, buffer[0], TOLERANCE_TIGHT);
    assertRelativeEquality(0.047256653907233145, buffer[1], TOLERANCE_TIGHT);
    result = polynomial.evaluate(5.0, buffer);
    assertSame(result, buffer);
    assertRelativeEquality(0.9978801887762878, buffer[0], TOLERANCE_TIGHT);
    assertRelativeEquality(-0.0047174432347332155, buffer[1], TOLERANCE);
    result = polynomial.evaluate(3.12217, buffer);
    assertSame(result, buffer);
    assertRelativeEquality(0.8309908586402249, buffer[0], TOLERANCE_TIGHT);
    assertRelativeEquality(0.17490443954044482, buffer[1], TOLERANCE_TIGHT);
  }

  @Test
  public void testDifferentiate() {
    assertRelativeEquality(0.17490443954044482, polynomial.differentiate(3.12217), TOLERANCE_TIGHT);
    assertRelativeEquality(0.047256653907233145, polynomial.differentiate(4.5), TOLERANCE_TIGHT);
    assertRelativeEquality(0.25407862364207057, polynomial.differentiate(2.0), TOLERANCE_TIGHT);
  }

  @Test
  public void testEvaluateDouble() {
    assertRelativeEquality(0.8309908586402249, polynomial.evaluate(3.12217), TOLERANCE_TIGHT);
    assertRelativeEquality(0.9871518076137477, polynomial.evaluate(4.5), TOLERANCE_TIGHT);
    assertRelativeEquality(0.5877852522924731, polynomial.evaluate(2.0), TOLERANCE_TIGHT);
  }

}
