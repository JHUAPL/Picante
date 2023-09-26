package picante.spice.adapters;

import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;

public class SpiceEphemerisIDTest {

  private SpiceEphemerisID codeInt;
  private SpiceEphemerisID codeIntString;

  @Before
  public void setUp() throws Exception {
    codeInt = new SpiceEphemerisID(200);
    codeIntString = new SpiceEphemerisID(210, "CODE210");
  }

  @Test
  public void testSpiceEphemerisIDInt() {
    assertEquals(200, codeInt.getIDCode());
    assertEquals("SPICE Object[200]", codeInt.getName());
  }

  @Test
  public void testSpiceEphemerisIDIntString() {
    assertEquals(210, codeIntString.getIDCode());
    assertEquals("CODE210", codeIntString.getName());
  }

  /*
   * We don't need to test the accessor methods, as they are already examined in the constructor
   * tests.
   */
}
