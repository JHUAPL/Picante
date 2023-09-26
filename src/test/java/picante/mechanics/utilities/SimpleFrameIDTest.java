package picante.mechanics.utilities;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;

public class SimpleFrameIDTest {

  private SimpleFrameID idA;
  private SimpleFrameID idAi;
  private SimpleFrameID idB;
  private SimpleFrameID ida;

  @Before
  public void setUp() throws Exception {
    idA = new SimpleFrameID("A", false);
    idAi = new SimpleFrameID("A", true);
    idB = new SimpleFrameID("B", false);
    ida = new SimpleFrameID("a", false);
  }

  @Test
  public void testHashCode() {
    SimpleFrameID newID = new SimpleFrameID("A", false);
    assertNotSame(newID, idA);
    assertEquals(newID.hashCode(), idA.hashCode());
    newID = new SimpleFrameID("A", true);
    assertEquals(newID.hashCode(), idAi.hashCode());
  }

  @Test
  public void testSimpleFrameIDString() {
    SimpleFrameID newID = new SimpleFrameID("A");
    assertEquals("A", newID.getName());
    assertFalse(newID.isInertial());
  }

  @Test
  public void testGetName() {
    assertEquals("A", idA.getName());
    assertEquals("B", idB.getName());
    assertEquals("A", idAi.getName());
    assertEquals("a", ida.getName());
  }

  @Test
  public void testToString() {
    assertEquals("SimpleFrameID[A]", idA.toString());
    assertEquals("SimpleFrameID[B]", idB.toString());
    assertEquals("SimpleFrameID[A, inertial]", idAi.toString());
    assertEquals("SimpleFrameID[a]", ida.toString());
  }

  @Test
  public void testIsInertial() {
    assertFalse(idA.isInertial());
    assertFalse(idB.isInertial());
    assertFalse(ida.isInertial());
    assertTrue(idAi.isInertial());
  }

  @Test
  public void testEqualsObject() {
    SimpleFrameID newID = new SimpleFrameID("A", false);
    assertTrue(newID.equals(idA));
    assertTrue(idA.equals(newID));
    assertFalse(idA.equals(idB));
    assertFalse(idA.equals(ida));
    assertFalse(idA.equals(idB));
    assertFalse(idA.equals(null));
    assertFalse(idA.equals("A"));
  }

}
