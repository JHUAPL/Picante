package picante.spice;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import org.junit.Ignore;
import org.junit.Test;

public class UtilitiesTest {

  @Ignore
  @Test
  public void testCanonicalizeSpiceName() {
    fail("Not yet implemented");
  }

  @Test
  public void testRound() {
    assertEquals(5.0, Utilities.round(5.4), 0.0);
    assertEquals(6.0, Utilities.round(5.5), 0.0);
    assertEquals(6.0, Utilities.round(5.6), 0.0);

    assertEquals(-5.0, Utilities.round(-5.4), 0.0);
    assertEquals(-6.0, Utilities.round(-5.5), 0.0);
    assertEquals(-6.0, Utilities.round(-5.6), 0.0);

    assertEquals(4.0, Utilities.round(4.4), 0.0);
    assertEquals(5.0, Utilities.round(4.5), 0.0);
    assertEquals(5.0, Utilities.round(4.6), 0.0);

    assertEquals(-4.0, Utilities.round(-4.4), 0.0);
    assertEquals(-5.0, Utilities.round(-4.5), 0.0);
    assertEquals(-5.0, Utilities.round(-4.6), 0.0);

    assertEquals(0.0, Utilities.round(0.0), 0.0);

  }

}
