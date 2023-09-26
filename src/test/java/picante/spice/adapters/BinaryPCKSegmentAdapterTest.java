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
import picante.mechanics.Coverages;
import picante.mechanics.FrameID;
import picante.mechanics.rotations.AxisAndAngle;
import picante.spice.kernel.pck.PCKSegment;

public class BinaryPCKSegmentAdapterTest {

  private BinaryPCKSegmentAdapter segmentAdapter;

  private PCKSegment segment;
  private FrameID fromID;
  private FrameID toID;

  private RotationMatrixIJK sampleRotation = new RotationMatrixIJK();

  @Before
  public void setUp() throws Exception {

    segment = createMock(PCKSegment.class);
    fromID = createMock(FrameID.class);
    toID = createMock(FrameID.class);

    new AxisAndAngle(3.8, 2.9, 5.1, Math.toRadians(83.9)).getRotation(sampleRotation);

    segmentAdapter = new BinaryPCKSegmentAdapter(fromID, toID, segment);
  }

  void configureMock() {

    CaptureAndAnswer<RotationMatrixIJK> capture = CaptureAndAnswer.create(sampleRotation);

    expect(segment.getTransform(eq(0.0), capture(capture.getCapture()))).andAnswer(capture)
        .anyTimes();
    expect(segment.getCoverage()).andReturn(Coverages.create(30.9, 35.1)).anyTimes();
  }

  @Test
  public void testBinaryPCKSegmentAdapter() {
    reset(segment);
    configureMock();
    replay(segment);

    assertEquals(segment.getCoverage(), segmentAdapter.getCoverage());
    assertEquals(fromID, segmentAdapter.getFromID());
    assertNotSame(fromID, segmentAdapter.getToID());
    assertEquals(toID, segmentAdapter.getToID());
    assertNotSame(toID, segmentAdapter.getFromID());

    verify(segment);
  }

  @Test
  public void testGetFromID() {
    assertEquals(fromID, segmentAdapter.getFromID());
    assertNotSame(fromID, segmentAdapter.getToID());
  }

  @Test
  public void testGetToID() {
    assertEquals(toID, segmentAdapter.getToID());
    assertNotSame(toID, segmentAdapter.getFromID());
  }

  @Test
  public void testGetCoverage() {
    reset(segment);
    configureMock();
    replay(segment);

    assertEquals(segment.getCoverage(), segmentAdapter.getCoverage());

    verify(segment);
  }

  @Test
  public void testGetTransform() {
    reset(segment);
    configureMock();
    replay(segment);

    assertEquals(sampleRotation.createTranspose(),
        segmentAdapter.getTransform(0.0, new RotationMatrixIJK()));
    assertEquals(segment.getTransform(0.0, new RotationMatrixIJK()).createTranspose(),
        segmentAdapter.getTransform(0.0, new RotationMatrixIJK()));

    assertComponentEquals(sampleRotation.createTranspose(),
        segmentAdapter.getTransform(0.0, new RotationMatrixIJK()), 0.0);
    assertComponentEquals(segment.getTransform(0.0, new RotationMatrixIJK()).createTranspose(),
        segmentAdapter.getTransform(0.0, new RotationMatrixIJK()), 0.0);

    verify(segment);
  }

}
