package picante.spice.adapters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;
import picante.mechanics.FrameID;
import picante.spice.kernel.tk.fk.FrameType;

public class SpiceClassedFrameIDTest {

  private SpiceClassedFrameID inertial;
  private SpiceClassedFrameID noninertial;

  @Before
  public void setUp() throws Exception {
    inertial = new SpiceClassedFrameID(FrameType.INERTIAL, -10, true);
    noninertial = new SpiceClassedFrameID(FrameType.CK, -100);
  }

  @Test
  public void testHashCode() {
    SpiceClassedFrameID a = new SpiceClassedFrameID(FrameType.CK, -100);
    SpiceClassedFrameID a2 = new SpiceClassedFrameID(FrameType.CK, -100);

    assertEquals(a.hashCode(), a2.hashCode());
  }

  @Test
  public void testSpiceClassedFrameIDFrameTypeInt() {
    assertFalse(noninertial.isInertial());
    assertEquals(FrameType.CK, noninertial.getType());
    assertEquals(-100, noninertial.getClassID());
  }

  @Test
  public void testSpiceClassedFrameIDFrameTypeIntBoolean() {
    assertTrue(inertial.isInertial());
    assertEquals(FrameType.INERTIAL, inertial.getType());
    assertEquals(-10, inertial.getClassID());
  }

  @Test
  public void testGetClassID() {
    assertEquals(-100, noninertial.getClassID());
    assertEquals(-10, inertial.getClassID());
  }

  @Test
  public void testGetType() {
    assertEquals(FrameType.INERTIAL, inertial.getType());
    assertEquals(FrameType.CK, noninertial.getType());
  }

  @Test
  public void testGetName() {
    assertEquals("SPICE CK Frame[-100]", noninertial.getName());
    assertEquals("SPICE INERTIAL Frame[-10]", inertial.getName());
  }

  @Test
  public void testIsInertial() {
    assertFalse(noninertial.isInertial());
    assertTrue(inertial.isInertial());
  }

  @Test
  public void testEqualsObject() {

    /*
     * Equals is determined from the frame type and the classID only.
     */
    SpiceClassedFrameID a = new SpiceClassedFrameID(FrameType.CK, -100);
    SpiceClassedFrameID a2 = new SpiceClassedFrameID(FrameType.CK, -100);
    SpiceClassedFrameID b = new SpiceClassedFrameID(FrameType.CK, -101);
    SpiceClassedFrameID c = new SpiceClassedFrameID(FrameType.PCK, -100);
    SpiceClassedFrameID d = new SpiceClassedFrameID(FrameType.INERTIAL, -1000);

    assertTrue(a.equals(a));
    assertTrue(a.equals(a2));
    assertFalse(a.equals(b));
    assertFalse(a.equals(c));
    assertFalse(a.equals(d));
    assertFalse(a.equals(new FrameID() {

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
