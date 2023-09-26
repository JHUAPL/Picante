package picante.spice.kernel.tk.fk;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class FrameTypeTest {

  @Test
  public void testEnumerationHasExpectedLength() {
    assertEquals(5, FrameType.values().length);
  }

  @Test
  public void testGetTypeID() {
    assertEquals(1, FrameType.INERTIAL.getTypeID());
    assertEquals(2, FrameType.PCK.getTypeID());
    assertEquals(3, FrameType.CK.getTypeID());
    assertEquals(4, FrameType.TK.getTypeID());
    assertEquals(5, FrameType.DYNAMIC.getTypeID());
  }

  @Test
  public void testGetTypeForClassInteger() {
    assertEquals(FrameType.INERTIAL, FrameType.getTypeForClassInteger(1));
    assertEquals(FrameType.PCK, FrameType.getTypeForClassInteger(2));
    assertEquals(FrameType.CK, FrameType.getTypeForClassInteger(3));
    assertEquals(FrameType.TK, FrameType.getTypeForClassInteger(4));
    assertEquals(FrameType.DYNAMIC, FrameType.getTypeForClassInteger(5));
  }

}
