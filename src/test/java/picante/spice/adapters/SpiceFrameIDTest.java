package picante.spice.adapters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;
import picante.mechanics.FrameID;

public class SpiceFrameIDTest {

  private SpiceFrameID codeInt;
  private SpiceFrameID codeIntTrue;
  private SpiceFrameID codeIntFalse;
  private SpiceFrameID codeIntTrueString;
  private SpiceFrameID codeIntFalseString;

  @Before
  public void setUp() throws Exception {
    codeInt = new SpiceFrameID(200);
    codeIntTrue = new SpiceFrameID(210, true);
    codeIntFalse = new SpiceFrameID(-210, false);
    codeIntFalseString = new SpiceFrameID(220, false, "CODE220");
    codeIntTrueString = new SpiceFrameID(-220, true, "CODE-220");
  }

  @Test
  public void testSpiceFrameIDInt() {
    assertEquals(200, codeInt.getIDCode());
    assertEquals("SPICE Frame[200]", codeInt.getName());
    assertEquals(false, codeInt.isInertial());
  }

  @Test
  public void testSpiceFrameIDIntBooleanTrue() {
    assertEquals(210, codeIntTrue.getIDCode());
    assertEquals("SPICE Frame[210]", codeIntTrue.getName());
    assertEquals(true, codeIntTrue.isInertial());
  }

  @Test
  public void testSpiceFrameIDIntBooleanFalse() {
    assertEquals(-210, codeIntFalse.getIDCode());
    assertEquals("SPICE Frame[-210]", codeIntFalse.getName());
    assertEquals(false, codeIntFalse.isInertial());
  }

  @Test
  public void testSpiceFrameIDIntBooleanTrueString() {
    assertEquals(-220, codeIntTrueString.getIDCode());
    assertEquals("CODE-220", codeIntTrueString.getName());
    assertEquals(true, codeIntTrueString.isInertial());
  }

  @Test
  public void testSpiceFrameIDIntBooleanFalseString() {
    assertEquals(220, codeIntFalseString.getIDCode());
    assertEquals("CODE220", codeIntFalseString.getName());
    assertEquals(false, codeIntFalseString.isInertial());
  }

  @Test
  public void testHashCode() {

    /*
     * As equals() only considers the ID code, so too hashCode().
     */
    SpiceFrameID idA = new SpiceFrameID(-100, true, "A");
    SpiceFrameID idA2 = new SpiceFrameID(-100, false, "A");
    SpiceFrameID idA3 = new SpiceFrameID(-100, true, "B");

    assertEquals(idA.hashCode(), idA2.hashCode());
    assertEquals(idA.hashCode(), idA3.hashCode());

  }

  @Test
  public void testEquals() {

    /*
     * The equals() method only considers the ID code.
     */
    SpiceFrameID idA = new SpiceFrameID(-100, true, "A");
    SpiceFrameID idA2 = new SpiceFrameID(-100, false, "A");
    SpiceFrameID idA3 = new SpiceFrameID(-100, true, "B");
    SpiceFrameID id = new SpiceFrameID(-101);

    assertTrue(idA.equals(idA));
    assertTrue(idA.equals(idA2));
    assertTrue(idA.equals(idA3));
    assertFalse(idA.equals(id));

    assertTrue(idA2.equals(idA));
    assertTrue(idA2.equals(idA2));
    assertTrue(idA2.equals(idA3));
    assertFalse(idA2.equals(id));

    assertTrue(idA3.equals(idA));
    assertTrue(idA3.equals(idA2));
    assertTrue(idA3.equals(idA3));
    assertFalse(idA3.equals(id));

    assertFalse(idA.equals(new FrameID() {

      @Override
      public boolean isInertial() {
        return false;
      }

      @Override
      public String getName() {
        return "ANONYMOUS";
      }
    }));

  }

}
