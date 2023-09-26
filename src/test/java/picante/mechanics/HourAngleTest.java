package picante.mechanics;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class HourAngleTest {

  @Test
  public void testFractionToHMSA() {
    HourAngle ha = new HourAngle(13.5);
    assertEquals("integer hours", 13, ha.getHours());
    assertEquals("integer minutes", 30, ha.getMinutes());
    double tol = 1.e-10;
    assertEquals("seoncds", 0, ha.getSeconds(), tol);
  }

  @Test
  public void testHMStoFractionA() {
    HourAngle ha = new HourAngle(13, 30, 0);
    double tol = 1.e-10;
    assertEquals("fractional hours", 13.5, ha.getFractionalHours(), tol);
  }

  @Test
  public void testFractionToHMSB() {
    HourAngle ha = new HourAngle(23.791388888888889);
    assertEquals("integer hours", 23, ha.getHours());
    assertEquals("integer minutes", 47, ha.getMinutes());
    double tol = 1.e-10;
    assertEquals("seoncds", 29, ha.getSeconds(), tol);
  }

  @Test
  public void testHMStoFractionB() {
    HourAngle ha = new HourAngle(23, 47, 29);
    double tol = 1.e-10;
    assertEquals("fractional hours", 23.791388888888889, ha.getFractionalHours(), tol);
  }

}
