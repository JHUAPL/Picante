package picante.spice.adapters;

import static org.easymock.EasyMock.createMock;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.spice.provided.InertialFrames;

public class InertialFrameAdapterTest {

  private InertialFrameAdapter inertialFrameAdapter;

  private FrameID fromID;
  private FrameID toID;
  private InertialFrames inertialFrames;

  @Before
  public void setUp() throws Exception {
    fromID = createMock(FrameID.class);
    toID = createMock(FrameID.class);

    inertialFrames = InertialFrames.ECLIPJ2000;

    inertialFrameAdapter = new InertialFrameAdapter(fromID, toID, inertialFrames);
  }

  @Test
  public void testInertialFrameAdapter() {
    assertEquals(Coverage.ALL_TIME, inertialFrameAdapter.getCoverage());
    assertEquals(fromID, inertialFrameAdapter.getFromID());
    assertNotSame(fromID, inertialFrameAdapter.getToID());
    assertEquals(toID, inertialFrameAdapter.getToID());
    assertNotSame(toID, inertialFrameAdapter.getFromID());
    assertEquals(inertialFrames.getTransform(0.0, new RotationMatrixIJK()),
        inertialFrameAdapter.getTransform(0.0, new RotationMatrixIJK()));
    assertEquals(inertialFrames.getStateTransform(0.0, new StateTransform()),
        inertialFrameAdapter.getStateTransform(0.0, new StateTransform()));
  }

  @Test
  public void testGetCoverage() {
    assertEquals(Coverage.ALL_TIME, inertialFrameAdapter.getCoverage());
  }

  @Test
  public void testGetFromID() {
    assertEquals(fromID, inertialFrameAdapter.getFromID());
    assertNotSame(fromID, inertialFrameAdapter.getToID());
  }

  @Test
  public void testGetToID() {
    assertEquals(toID, inertialFrameAdapter.getToID());
    assertNotSame(toID, inertialFrameAdapter.getFromID());
  }

  @Test
  public void testGetTransform() {
    assertEquals(inertialFrames.getTransform(0.0, new RotationMatrixIJK()),
        inertialFrameAdapter.getTransform(0.0, new RotationMatrixIJK()));
  }

  @Test
  public void testGetStateTransform() {
    assertEquals(inertialFrames.getStateTransform(0.0, new StateTransform()),
        inertialFrameAdapter.getStateTransform(0.0, new StateTransform()));
  }

}
