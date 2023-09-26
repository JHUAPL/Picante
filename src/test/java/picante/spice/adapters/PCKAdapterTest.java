package picante.spice.adapters;

import static org.easymock.EasyMock.capture;
import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.eq;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.reset;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import static picante.junit.AssertTools.assertComponentEquals;
import org.junit.Before;
import org.junit.Test;
import picante.junit.CaptureAndAnswer;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.FrameID;
import picante.mechanics.rotations.AxisAndAngle;
import picante.spice.kernel.tk.pck.PCKFrameFunction;

public class PCKAdapterTest {

  private PCKAdapter pckAdapter;

  private FrameID fromID;
  private FrameID toID;
  private PCKFrameFunction pff;

  private RotationMatrixIJK sampleRotation = new RotationMatrixIJK();

  @Before
  public void setUp() throws Exception {

    new AxisAndAngle(6.0, 3.1, 2.1, Math.toRadians(38.1)).getRotation(sampleRotation);

    fromID = createMock(FrameID.class);
    toID = createMock(FrameID.class);
    pff = createMock(PCKFrameFunction.class);
    pckAdapter = new PCKAdapter(fromID, toID, pff);
  }

  void configureMock() {

    CaptureAndAnswer<RotationMatrixIJK> capture = CaptureAndAnswer.create(sampleRotation);

    expect(pff.getTransform(eq(0.0), capture(capture.getCapture()))).andAnswer(capture).anyTimes();
  }

  @Test
  public void testPCKAdapter() {
    assertEquals(Coverage.ALL_TIME, pckAdapter.getCoverage());
    assertEquals(fromID, pckAdapter.getFromID());
    assertNotSame(fromID, pckAdapter.getToID());
    assertEquals(toID, pckAdapter.getToID());
    assertNotSame(toID, pckAdapter.getFromID());
  }

  @Test
  public void testGetCoverage() {
    assertEquals(Coverage.ALL_TIME, pckAdapter.getCoverage());
  }

  @Test
  public void testGetFromID() {
    assertEquals(fromID, pckAdapter.getFromID());
    assertNotSame(fromID, pckAdapter.getToID());
  }

  @Test
  public void testGetToID() {
    assertEquals(toID, pckAdapter.getToID());
    assertNotSame(toID, pckAdapter.getFromID());
  }

  @Test
  public void testGetTransform() {
    reset(pff);
    configureMock();
    replay(pff);

    assertEquals(sampleRotation.createTranspose(),
        pckAdapter.getTransform(0.0, new RotationMatrixIJK()));
    assertComponentEquals(sampleRotation.createTranspose(),
        pckAdapter.getTransform(0.0, new RotationMatrixIJK()), 0.0);

    verify(pff);
  }

}
