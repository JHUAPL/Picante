package picante.spice.kernel.tk.fk;

import static org.junit.Assert.assertEquals;
import static picante.junit.AssertTools.assertEqualMatrix;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.RotationMatrixIJK;

public class TKFrameFunctionTest {

  private int fromID;
  private int toID;

  private RotationMatrixIJK rotation;

  private TKFrameFunction function;

  @Before
  public void setUp() throws Exception {
    toID = 0;
    fromID = 10;
    rotation = new RotationMatrixIJK(RotationMatrixIJK.IDENTITY);
    function = new TKFrameFunction(fromID, toID, rotation);
  }

  @Test
  public void testGetFromID() {
    assertEquals(fromID, function.getFromID());
  }

  @Test
  public void testGetToID() {
    assertEquals(toID, function.getToID());
  }

  @Test
  public void testGetTransform() {
    assertEqualMatrix(rotation, function.getTransform(0.0, new RotationMatrixIJK()));
  }

}
