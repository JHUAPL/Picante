package picante.mechanics.utilities;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;

public class SimpleEphemerisIDTest {

  private SimpleEphemerisID idA;
  private SimpleEphemerisID idB;
  private SimpleEphemerisID ida;

  @Before
  public void setUp() throws Exception {
    idA = new SimpleEphemerisID("A");
    idB = new SimpleEphemerisID("B");
    ida = new SimpleEphemerisID("a");
  }

  @Test
  public void testHashCode() {
    SimpleEphemerisID newID = new SimpleEphemerisID("A");
    assertNotSame(newID, idA);
    assertEquals(newID.hashCode(), idA.hashCode());
  }

  @Test
  public void testGetName() {
    assertEquals("A", idA.getName());
    assertEquals("B", idB.getName());
    assertEquals("a", ida.getName());
  }

  @Test
  public void testToString() {
    assertEquals("SimpleEphemerisID[A]", idA.toString());
    assertEquals("SimpleEphemerisID[B]", idB.toString());
    assertEquals("SimpleEphemerisID[a]", ida.toString());
  }

  @Test
  public void testEqualsObject() {
    SimpleEphemerisID newID = new SimpleEphemerisID("A");
    assertTrue(idA.equals(newID));
    assertTrue(newID.equals(idA));
    assertFalse(idA.equals(idB));
    assertFalse(idA.equals(ida));
    assertFalse(idA.equals(null));
    assertFalse(idA.equals("A"));
  }

}
