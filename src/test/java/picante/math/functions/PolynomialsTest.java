package picante.math.functions;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class PolynomialsTest {

  @Test
  public void testConst() {
    // This creates a Constant polynomial
    Polynomial result = Polynomials.create(new double[] {1.5});
    assertEquals("degree", 0, result.getDegree(), 0.0);
    assertEquals("constant polynomial", 1.5, result.evaluate(27.5), 0.0);
    assertEquals("constant polynomial deriviative", 0.0, result.differentiate(99.0), 0.0);
  }

  @Test
  public void testLine() {
    // Each of these is the same line: y = x-2
    Polynomial result1 = Polynomials.create(new double[] {-2.0, 1.0});
    Polynomial result2 = Polynomials.createLine(1.0, -2.0);
    Polynomial result3 = Polynomials.createLine(0, -2, 2, 0);

    assertEquals("degree", 1, result1.getDegree(), 0.0);

    assertEquals(1.0, result1.evaluate(3.0), 0.0);
    assertEquals(1.0, result2.evaluate(3.0), 0.0);
    assertEquals(1.0, result3.evaluate(3.0), 0.0);

    // the point doesn't matter (3.0 or 13.0 or anything), the slope will be the same
    assertEquals(1.0, result1.differentiate(3.5), 0.0);
    assertEquals(1.0, result2.differentiate(13.0), 0.0);
    assertEquals(1.0, result3.differentiate(13.0), 0.0);
  }

  @Test
  public void testQuad() {
    // Each of these is the same quadratic: y = 1 + 2 x + 3 x^2
    Polynomial result1 = Polynomials.create(new double[] {1.0, 2.0, 3.0});
    Polynomial result2 = Polynomials.createQuadratic(1.0, 2.0, 3.0);

    assertEquals("degree", 2, result1.getDegree(), 0.0);

    assertEquals(17.0, result1.evaluate(2.0), 0.0);
    assertEquals(17.0, result2.evaluate(2.0), 0.0);

    assertEquals(20.0, result1.differentiate(3.0), 0.0);
    assertEquals(20.0, result2.differentiate(3.0), 0.0);
  }

  @Test
  public void testCubic() {
    // y = 1 + 2 x + 3 x^2 + 4 x^3
    Polynomial result1 = Polynomials.create(new double[] {1.0, 2.0, 3.0, 4.0});

    assertEquals("degree", 3, result1.getDegree(), 0.0);

    assertEquals(10.0, result1.evaluate(1.0), 0.0);
    assertEquals(49.0, result1.evaluate(2.0), 0.0);
    assertEquals(2 + 3 * 2 * 2 + 4 * 3 * 2 * 2, result1.differentiate(2.0), 0.0);
  }

}
