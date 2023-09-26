package picante.spice.kernel.tk.fk;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;

public class FrameInfoTest {

  private FrameInfo info;
  private FrameInfo infoMixedCase;
  private FrameInfo infoAlternate;

  private FrameInfo infoDifferentType;
  private FrameInfo infoDifferentCenter;
  private FrameInfo infoDifferentClassID;
  private FrameInfo infoDifferentCode;

  @Before
  public void setUp() throws Exception {
    info = new FrameInfo("INFO", -1, FrameType.CK, -2, -3);
    infoMixedCase = new FrameInfo("InFo", -1, FrameType.CK, -2, -3);
    infoAlternate = new FrameInfo("Alternate", -10, FrameType.DYNAMIC, -11, -12);

    infoDifferentType = new FrameInfo(info.getName(), info.getCode(), FrameType.INERTIAL,
        info.getClassID(), info.getCenterID());
    infoDifferentCenter =
        new FrameInfo(info.getName(), info.getCode(), info.getType(), info.getClassID(), -4);
    infoDifferentClassID =
        new FrameInfo(info.getName(), info.getCode(), info.getType(), -5, info.getCenterID());
    infoDifferentCode =
        new FrameInfo(info.getName(), 0, info.getType(), info.getClassID(), info.getCenterID());
  }

  @Test
  public void testHashCode() {
    assertEquals(info.hashCode(), infoMixedCase.hashCode());
  }

  @Test
  public void testGetCode() {
    assertEquals(-1, info.getCode());
    assertEquals(-1, infoMixedCase.getCode());
    assertEquals(-10, infoAlternate.getCode());
  }

  @Test
  public void testGetClassID() {
    assertEquals(-2, info.getClassID());
    assertEquals(-2, infoMixedCase.getClassID());
    assertEquals(-11, infoAlternate.getClassID());
  }

  @Test
  public void testGetCenterID() {
    assertEquals(-3, info.getCenterID());
    assertEquals(-3, infoMixedCase.getCenterID());
    assertEquals(-12, infoAlternate.getCenterID());
  }

  @Test
  public void testGetName() {
    assertEquals("INFO", info.getName());
    assertEquals("INFO", infoMixedCase.getName());
    assertEquals("ALTERNATE", infoAlternate.getName());
  }

  @Test
  public void testGetType() {
    assertEquals(FrameType.CK, info.getType());
    assertEquals(FrameType.CK, infoMixedCase.getType());
    assertEquals(FrameType.DYNAMIC, infoAlternate.getType());
  }

  @Test
  public void testEqualsObject() {

    assertTrue(info.equals(infoMixedCase));
    assertFalse(info.equals(infoAlternate));

    assertFalse(info.equals(infoDifferentCenter));
    assertFalse(info.equals(infoDifferentClassID));
    assertFalse(info.equals(infoDifferentCode));
    assertFalse(info.equals(infoDifferentType));

  }

}
