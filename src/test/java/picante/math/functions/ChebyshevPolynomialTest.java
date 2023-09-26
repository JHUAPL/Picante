package picante.math.functions;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static picante.junit.AssertTools.assertRelativeEquality;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;


public class ChebyshevPolynomialTest {

  private static final double TOLERANCE_TIGHT = 1.0E-15;

  private ChebyshevPolynomial polynomial;
  private double[] buffer;

  @Before
  public void setUp() throws Exception {

    double[] coeffs = new double[10];
    for (int i = 0; i < 10; i++) {
      coeffs[i] = 10 - i;
    }

    double[] x2s = new double[2];
    x2s[0] = 2.0;
    x2s[1] = 4.0;

    polynomial = new ChebyshevPolynomial(9, coeffs, x2s);
    buffer = new double[2];
  }

  @Test
  public void testChebyshevPolynomial() {
    /*
     * The default polynomial should just evaluate to 0 for all inputs.
     */
    ChebyshevPolynomial defPoly = new ChebyshevPolynomial();

    assertEquals(0.0, defPoly.evaluate(0.0), 0.0);
    assertEquals(0.0, defPoly.evaluate(-1.0), 0.0);
    assertEquals(0.0, defPoly.evaluate(1.0), 0.0);

    assertEquals(0.0, defPoly.differentiate(0.0), 0.0);
    assertEquals(0.0, defPoly.differentiate(-1.0), 0.0);
    assertEquals(0.0, defPoly.differentiate(1.0), 0.0);

  }

  @Ignore
  @Test
  public void testChebyshevPolynomialIntDoubleArrayDoubleArray() {
    /*
     * This is implicitly tested by the setUp() method and other tests exercising the contents of
     * polynomial.
     */
    fail();
  }

  @Test
  public void testChebyshevPolynomialChebyshevPolynomial() {

    ChebyshevPolynomial copy = new ChebyshevPolynomial(polynomial);
    assertEquals(polynomial, copy);

  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testChebyshevPolynomialCoeffsException() {
    new ChebyshevPolynomial(10, new double[5], new double[2]);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testChebyshevPolynomialX2SException() {
    new ChebyshevPolynomial(10, new double[10], new double[1]);
  }

  @Test
  public void testGetNumberOfCoefficients() {
    assertEquals(10, polynomial.getNumberOfCoefficients());
  }

  @Test
  public void testGetDegree() {
    assertEquals(9, polynomial.getDegree());
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetCoefficientsCoeffsException() {
    double[] coeffs = new double[5];
    double[] x2s = new double[2];
    polynomial.getCoefficients(coeffs, x2s);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetCoefficientsX2SException() {
    double[] coeffs = new double[10];
    double[] x2s = new double[1];
    polynomial.getCoefficients(coeffs, x2s);
  }

  @Test
  public void testGetCoefficients() {
    double[] coeffs = new double[10];
    double[] x2s = new double[2];
    polynomial.getCoefficients(coeffs, x2s);

    assertEquals(2.0, x2s[0], 0.0);
    assertEquals(4.0, x2s[1], 0.0);

    for (int i = 0; i < 10; i++) {
      assertEquals("At index " + i, 10 - i, coeffs[i], 0.0);
    }
  }

  @Test
  public void testGetCoefficientsWithOffsets() {
    double[] data = new double[14];
    polynomial.getCoefficients(data, 2, data, 0);

    assertEquals(2.0, data[0], 0.0);
    assertEquals(4.0, data[1], 0.0);

    for (int i = 2; i < 12; i++) {
      assertEquals("At index " + i, 10 - (i - 2), data[i], 0.0);
    }
  }

  @Test
  public void testSetCoefficientsChebyshevPolynomial() {
    ChebyshevPolynomial p =
        new ChebyshevPolynomial(1, new double[] {1.0, 1.0}, new double[] {1.0, 2.0});

    p.setCoefficients(polynomial);

    double[] coeffs = new double[10];
    double[] x2s = new double[2];
    polynomial.getCoefficients(coeffs, x2s);

    assertEquals(2.0, x2s[0], 0.0);
    assertEquals(4.0, x2s[1], 0.0);

    for (int i = 0; i < 10; i++) {
      assertEquals("At index " + i, 10 - i, coeffs[i], 0.0);
    }

  }

  @Test
  public void testSetCoefficients() {
    double[] coeffs = new double[1];
    coeffs[0] = 1.0;
    double[] x2s = new double[2];
    x2s[0] = 0.0;
    x2s[1] = 2.0;

    ChebyshevPolynomial cheby = new ChebyshevPolynomial(0, coeffs, x2s);
    assertEquals(1.0, cheby.evaluate(1.0), 0.0);
    cheby.evaluate(1.0, buffer);
    assertEquals(1.0, buffer[0], 0.0);
    assertEquals(0.0, buffer[1], 0.0);

    coeffs = new double[10];
    for (int i = 0; i < 10; i++) {
      coeffs[i] = 10 - i;
    }
    x2s[0] = 2.0;
    x2s[1] = 4.0;

    cheby.setCoefficients(9, coeffs, x2s);
    assertRelativeEquality(5.1220703125, cheby.evaluate(3.0), TOLERANCE_TIGHT);
    cheby.evaluate(3.0, buffer);
    assertRelativeEquality(5.1220703125, buffer[0], TOLERANCE_TIGHT);
    assertRelativeEquality(-0.9521484375, buffer[1], TOLERANCE_TIGHT);

  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testSetCoefficientsCoeffsIndexOutOfBoundsException() {
    polynomial.setCoefficients(10, new double[5], new double[2]);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testSetCoefficientsX2SIndexOutOfBoundsException() {
    polynomial.setCoefficients(10, new double[10], new double[1]);
  }

  @Test
  public void testSetCoefficientsWithOffsets() {

    double[] data = new double[20];
    for (int i = 0; i < data.length; i++) {
      data[i] = i;
    }

    ChebyshevPolynomial cheby = new ChebyshevPolynomial(1, new double[2], new double[2]);

    cheby.setCoefficients(3, data, 4, data, 18);

    double[] coeffs = new double[4];
    double[] x2s = new double[2];

    cheby.getCoefficients(coeffs, x2s);

    for (int i = 0; i < coeffs.length; i++) {
      assertEquals(data[4 + i], coeffs[i], 0.0);
    }

    assertEquals(data[18], x2s[0], 0.0);
    assertEquals(data[19], x2s[1], 0.0);

  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testSetCoefficientsWithOffsetsCoeffsIndexOutOfBoundsException() {
    polynomial.setCoefficients(5, new double[100], 98, new double[2], 0);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testSetCoefficientsWithOffsetsX2SIndexOutOfBoundsException() {
    polynomial.setCoefficients(5, new double[10], 3, new double[10], 9);
  }

  @Test
  public void testEvaluateDoubleDoubleArray() {
    double[] result = polynomial.evaluate(3.0, buffer);
    assertSame(result, buffer);
    assertRelativeEquality(5.1220703125, buffer[0], TOLERANCE_TIGHT);
    assertRelativeEquality(-0.9521484375, buffer[1], TOLERANCE_TIGHT);
    result = polynomial.evaluate(-1.0, buffer);
    assertSame(result, buffer);
    assertRelativeEquality(5.1181640625, buffer[0], TOLERANCE_TIGHT);
    assertRelativeEquality(0.8916015625, buffer[1], TOLERANCE_TIGHT);
    result = polynomial.evaluate(14.0, buffer);
    assertSame(result, buffer);
    assertRelativeEquality(5654889.0, buffer[0], TOLERANCE_TIGHT);
    assertRelativeEquality(4291398.25, buffer[1], TOLERANCE_TIGHT);

  }

  @Test
  public void testEvaluateDouble() {
    assertRelativeEquality(5.1220703125, polynomial.evaluate(3.0), TOLERANCE_TIGHT);
    assertRelativeEquality(5.1181640625, polynomial.evaluate(-1.0), TOLERANCE_TIGHT);
    assertRelativeEquality(5654889, polynomial.evaluate(14.0), TOLERANCE_TIGHT);
  }

  @Test
  public void testDifferentiateDouble() {
    assertRelativeEquality(-0.9521484375, polynomial.differentiate(3.0), TOLERANCE_TIGHT);
    assertRelativeEquality(0.8916015625, polynomial.differentiate(-1.0), TOLERANCE_TIGHT);
    assertRelativeEquality(4291398.25, polynomial.differentiate(14.0), TOLERANCE_TIGHT);
  }

  @Test
  public void testHashCode() {
    /*
     * Simply check that two equal polynomials have the same hash code. Note: this test results in
     * the internal data store being different lengths.
     */
    ChebyshevPolynomial copy = new ChebyshevPolynomial(30, new double[31], new double[2]);
    copy.setCoefficients(polynomial);

    assertEquals(copy.hashCode(), polynomial.hashCode());
    assertNotSame(copy, polynomial);

  }

  @Test
  public void testEquals() {

    /*
     * This constructs a polynomial with an internal coefficient storage that exceeds that of the
     * polynomial it is copying. This is necessary given the internal implementation of the
     * polynomial class.
     */
    ChebyshevPolynomial copy = new ChebyshevPolynomial(30, new double[31], new double[2]);
    copy.setCoefficients(polynomial);

    assertTrue(copy.equals(polynomial));
    assertNotSame(copy, polynomial);

    assertTrue(polynomial.equals(polynomial));

    assertFalse(polynomial.equals(null));

    assertFalse(polynomial.equals(""));

  }

}
