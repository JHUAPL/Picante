package picante.math.functions;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static picante.junit.AssertTools.assertRelativeEquality;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

public class HermitePolynomialTest {

  private static final double TOLERANCE = 1.0E-13;

  private HermitePolynomial polynomial;
  private double[] buffer;
  private double[] xvals;
  private double[] yvals;
  private double[] testXs;
  private double[] expectedYs;
  private double[] expectedDys;

  @Before
  public void setUp() throws Exception {

    xvals = new double[10];
    yvals = new double[20];

    for (int i = 0; i < xvals.length; i++) {
      xvals[i] = i * Math.PI / (xvals.length - 1);
      yvals[2 * i] = Math.sin(xvals[i]);
      yvals[2 * i + 1] = Math.cos(xvals[i]);
    }

    polynomial = new HermitePolynomial(xvals.length, xvals, yvals);
    buffer = new double[2];

    /*
     * These values and their corresponding interpolated value and derivatives were produced
     * directly from a FORTRAN SPICE program using HRMINT.
     */
    testXs = new double[] {0.0000000000000000, 0.10833078115826873, 0.21666156231653746,
        0.32499234347480621, 0.43332312463307493, 0.54165390579134365, 0.64998468694961242,
        0.75831546810788109, 0.86664624926614986, 0.97497703042441852, 1.0833078115826873,
        1.1916385927409558, 1.2999693738992248, 1.4083001550574936, 1.5166309362157622,
        1.6249617173740307, 1.7332924985322997, 1.8416232796905685, 1.9499540608488370,
        2.0582848420071058, 2.1666156231653746, 2.2749464043236434, 2.3832771854819117,
        2.4916079666401805, 2.5999387477984497, 2.7082695289567185, 2.8166003101149872,
        2.9249310912732556, 3.0332618724315243, 3.1415926535897931};

    expectedYs = new double[] {0.0000000000000000, 0.10811901842394468, 0.21497044021102463,
        0.31930153013598012, 0.41988910156026460, 0.51555385717702207, 0.60517421519376535,
        0.68769945885342354, 0.76216205512763713, 0.82768899815689045, 0.88351204444602272,
        0.92897671981679153, 0.96354999251922335, 0.98682652254152636, 0.99853341385112382,
        0.99853341385112393, 0.98682652254152647, 0.96354999251922324, 0.92897671981679164,
        0.88351204444602260, 0.82768899815689045, 0.76216205512763635, 0.68769945885342354,
        0.60517421519376535, 0.51555385717702162, 0.41988910156026421, 0.31930153013597967,
        0.21497044021102554, 0.10811901842393642, 1.22464679914735321E-016};

    expectedDys = new double[] {1.0000000000000000, 0.99413795715436037, 0.97662055571007456,
        0.94765317118280601, 0.90757541967095656, 0.85685717616758872, 0.79609306570564253,
        0.72599549192313217, 0.64738628478182791, 0.56118706536238283, 0.46840844069979093,
        0.37013815533991473, 0.26752833852922125, 0.16178199655276462, 5.41389085854177346E-002,
        -5.41389085854171864E-002, -0.16178199655276423, -0.26752833852921970, -0.37013815533991312,
        -0.46840844069979010, -0.56118706536238250, -0.64738628478182880, -0.72599549192313129,
        -0.79609306570564276, -0.85685717616758827, -0.90757541967095656, -0.94765317118280656,
        -0.97662055571006412, -0.99413795715436204, -1.0000000000000000};

  }

  @Ignore
  @Test
  public void testHermitePolynomialIntDoubleArrayDoubleArray() {
    /*
     * This is implicitly exercised by the setUp() method and the other tests present here.
     */
    fail();
  }

  @Test
  public void testHermitePolynomialHermitePolynomial() {
    HermitePolynomial copy = new HermitePolynomial(polynomial);
    assertEquals(polynomial, copy);
  }

  @Test
  public void testGetNumberOfPoints() {
    assertEquals(xvals.length, polynomial.getNumberOfPoints());
  }

  @Test
  public void testGetDegree() {
    assertEquals(2 * xvals.length - 1, polynomial.getDegree());
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetPointsDoubleArrayDoubleArrayXvalsException() {
    double[] xvals = new double[this.xvals.length - 1];
    double[] yvals = new double[this.yvals.length];
    polynomial.getPoints(xvals, yvals);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetPointsDoubleArrayDoubleArrayYvalsException() {
    double[] xvals = new double[this.xvals.length];
    double[] yvals = new double[this.yvals.length - 1];
    polynomial.getPoints(xvals, yvals);
  }

  @Test
  public void testGetPointsDoubleArrayDoubleArray() {
    double[] testX = new double[xvals.length];
    double[] testY = new double[yvals.length];
    polynomial.getPoints(testX, testY);

    assertArrayEquals(xvals, testX, 0.0);
    assertArrayEquals(yvals, testY, 0.0);
  }

  @Test
  public void testGetPointsDoubleArrayIntDoubleArrayInt() {
    double[] data = new double[xvals.length * 3 + 5];

    /*
     * Offset must be less than the value added to the required length of the data array above.
     */
    final int offset = 1;

    polynomial.getPoints(data, offset, data, xvals.length + offset);

    for (int i = offset; i < xvals.length + offset; i++) {
      assertEquals("At index " + i, xvals[i - offset], data[i], 0.0);
    }

    for (int i = offset + xvals.length; i < 3 * xvals.length + offset; i++) {
      assertEquals("At index " + i, yvals[i - offset - xvals.length], data[i], 0.0);
    }
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetPointsDoubleArrayIntDoubleArrayIntXvalsException() {
    double[] xvals = new double[this.xvals.length - 1];
    double[] yvals = new double[this.yvals.length];

    polynomial.getPoints(xvals, 0, yvals, 0);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetPointsDoubleArrayIntDoubleArrayIntYvalsException() {
    double[] xvals = new double[this.xvals.length];
    double[] yvals = new double[this.yvals.length - 1];

    polynomial.getPoints(xvals, 0, yvals, 0);
  }

  @Test
  public void testSetPointsHermitePolynomial() {

    HermitePolynomial p =
        new HermitePolynomial(3, new double[] {1, 2, 3}, new double[] {4, 5, 6, 7, 8, 9});

    p.setPoints(polynomial);

    double[] xvals = new double[this.xvals.length];
    double[] yvals = new double[this.yvals.length];

    p.getPoints(xvals, yvals);

    assertArrayEquals(this.xvals, xvals, 0.0);
    assertArrayEquals(this.yvals, yvals, 0.0);
  }

  @Test
  public void testSetPointsIntDoubleArrayDoubleArray() {

    double[] xvals = new double[] {1, 2, 3};
    double[] yvals = new double[] {4, 5, 6, 7, 8, 9};
    HermitePolynomial p = new HermitePolynomial(3, xvals, yvals);

    p.setPoints(this.xvals.length, this.xvals, this.yvals);

    xvals = new double[this.xvals.length];
    yvals = new double[this.yvals.length];

    p.getPoints(xvals, yvals);

    assertArrayEquals(this.xvals, xvals, 0.0);
    assertArrayEquals(this.yvals, yvals, 0.0);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testSetPointsIntDoubleArrayDoubleArrayXvalsException() {
    polynomial.setPoints(5, new double[3], new double[10]);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testSetPointsIntDoubleArrayDoubleArrayYvalsException() {
    polynomial.setPoints(5, new double[5], new double[5]);
  }

  @Test
  public void testSetPointsIntDoubleArrayIntDoubleArrayInt() {
    HermitePolynomial p = new HermitePolynomial(5, new double[5], new double[10]);

    double[] data = new double[xvals.length + yvals.length + 2];
    final int xvalsOffset = 1;
    final int yvalsOffset = 1 + xvals.length + 1;
    for (int i = xvalsOffset; i < xvals.length + xvalsOffset; i++) {
      data[i] = xvals[i - xvalsOffset];
    }
    for (int i = yvalsOffset; i < yvals.length + yvalsOffset; i++) {
      data[i] = yvals[i - yvalsOffset];
    }

    p.setPoints(xvals.length, data, xvalsOffset, data, yvalsOffset);

    double xvals[] = new double[this.xvals.length];
    double yvals[] = new double[this.yvals.length];

    p.getPoints(xvals, yvals);

    assertArrayEquals(this.xvals, xvals, 0.0);
    assertArrayEquals(this.yvals, yvals, 0.0);

  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testSetPointsIntDoubleArrayIntDoubleArrayIntXvalsException() {
    polynomial.setPoints(5, new double[5], 2, new double[10], 0);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testSetPointsIntDoubleArrayIntDoubleArrayIntYvalsException() {
    polynomial.setPoints(5, new double[5], 0, new double[10], 4);
  }

  @Test
  public void testEvaluateDoubleDoubleArray() {
    for (int i = 0; i < testXs.length; i++) {
      polynomial.evaluate(testXs[i], buffer);
      assertRelativeEquality(expectedYs[i], buffer[0], TOLERANCE);
      assertRelativeEquality(expectedDys[i], buffer[1], TOLERANCE);
    }
  }

  @Test
  public void testEvaluateDouble() {
    for (double x : testXs) {
      polynomial.evaluate(x, buffer);
      assertEquals(polynomial.evaluate(x), buffer[0], 0.0);
    }
  }

  @Test
  public void testDifferentiate() {
    for (double x : testXs) {
      polynomial.evaluate(x, buffer);
      assertEquals(polynomial.differentiate(x), buffer[1], 0.0);
    }
  }

  @Test
  public void testHashCode() {

    /*
     * Simply check that two equal polynomials have the same hash code. Note: this test results in
     * the internal data store being different lengths.
     */

    HermitePolynomial copy = new HermitePolynomial(30, new double[30], new double[60]);
    copy.setPoints(polynomial);

    assertEquals(copy.hashCode(), polynomial.hashCode());
    assertNotSame(copy, polynomial);

  }

  @Test
  public void testEqualsObject() {

    /*
     * This constructs a polynomial with an internal storage that exceeds that of the polynomial it
     * is copying. This is necessary to exercise given the internal implementation of the polynomial
     * class.
     */
    HermitePolynomial copy = new HermitePolynomial(30, new double[30], new double[60]);
    copy.setPoints(polynomial);

    assertTrue(copy.equals(polynomial));
    assertNotSame(copy, polynomial);

    assertTrue(polynomial.equals(polynomial));

    assertFalse(polynomial.equals(null));

    assertFalse(polynomial.equals(""));
  }

}
