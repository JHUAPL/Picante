package picante.math.functions;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class UnivariatePolynomialTest {
  private final UnivariatePolynomial p3 =
      new UnivariatePolynomial(new double[] {2.0, 3.5, 8.75, 11.5});

  private final double epsilon = 1.e-12;

  @Test
  public void testGetDegreeP3() {
    assertEquals(3, p3.getDegree());
  }

  @Test
  public void testEvaluateP3() {
    double x = 2.25;
    double knownValue = 2.0 + 3.5 * x + 8.75 * x * x + 11.5 * Math.pow(x, 3);
    assertEquals("test func P3 at x = " + String.format("%4.2f", x), knownValue, p3.evaluate(x),
        epsilon);
  }

  @Test
  public void testDifferentiateP3() {
    double x = 2.25;
    double knownValue = 3.5 + 2 * x * 8.75 + 3 * 11.5 * x * x;
    assertEquals("test func p3 deriv. at x = " + String.format("%4.2f", x), knownValue,
        p3.differentiate(x), epsilon);
  }

}
