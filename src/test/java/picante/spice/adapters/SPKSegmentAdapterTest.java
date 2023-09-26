package picante.spice.adapters;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.reset;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import static picante.junit.AssertTools.assertComponentEquals;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.Coverages;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.StateVector;
import picante.spice.kernel.spk.SPKSegment;

public class SPKSegmentAdapterTest {

  private SPKSegmentAdapter spkSegmentAdapter;

  private EphemerisID targetID;
  private EphemerisID observerID;
  private FrameID frameID;
  private SPKSegment segment;

  private Coverage coverage;
  private VectorIJK position;
  private StateVector stateVector;

  @Before
  public void setUp() throws Exception {
    targetID = createMock(EphemerisID.class);
    observerID = createMock(EphemerisID.class);
    frameID = createMock(FrameID.class);

    segment = createMock(SPKSegment.class);
    coverage = Coverages.create(3.0, 8.0);
    position = new VectorIJK(3.8, 1.2, 6.2);
    stateVector = new StateVector(position, new VectorIJK(12.56, 2132.0, 3.0));

    spkSegmentAdapter = new SPKSegmentAdapter(targetID, observerID, frameID, segment);
  }

  void configureSegmentMock() {
    expect(segment.getCoverage()).andReturn(coverage).anyTimes();
    expect(segment.getPosition(0.0, new VectorIJK())).andReturn(position).anyTimes();
    expect(segment.getState(0.0, new StateVector())).andReturn(stateVector).anyTimes();
  }

  @Test
  public void testSPKSegmentAdapter() {
    reset(segment);
    configureSegmentMock();
    replay(segment);

    assertEquals(coverage, spkSegmentAdapter.getCoverage());

    assertComponentEquals(stateVector.getPosition(),
        spkSegmentAdapter.getState(0.0, new StateVector()).getPosition(), 0.0);
    assertComponentEquals(stateVector.getVelocity(),
        spkSegmentAdapter.getState(0.0, new StateVector()).getVelocity(), 0.0);

    assertEquals(frameID, spkSegmentAdapter.getFrameID());
    assertEquals(observerID, spkSegmentAdapter.getObserverID());
    assertEquals(observerID.hashCode(), spkSegmentAdapter.getObserverID().hashCode());
    assertNotSame(observerID, spkSegmentAdapter.getTargetID());
    assertEquals(targetID, spkSegmentAdapter.getTargetID());
    assertEquals(targetID.hashCode(), spkSegmentAdapter.getTargetID().hashCode());
    assertNotSame(targetID, spkSegmentAdapter.getObserverID());

    verify(segment);

  }

  @Test
  public void testGetState() {
    reset(segment);
    configureSegmentMock();
    replay(segment);

    assertEquals(stateVector, spkSegmentAdapter.getState(0.0, new StateVector()));
    assertComponentEquals(stateVector.getPosition(),
        spkSegmentAdapter.getState(0.0, new StateVector()).getPosition(), 0.0);
    assertComponentEquals(stateVector.getVelocity(),
        spkSegmentAdapter.getState(0.0, new StateVector()).getVelocity(), 0.0);

    verify(segment);
  }

  @Test
  public void testGetPosition() {
    reset(segment);
    configureSegmentMock();
    replay(segment);

    assertEquals(position, spkSegmentAdapter.getPosition(0, new VectorIJK()));
    assertComponentEquals(position, spkSegmentAdapter.getPosition(0.0, new VectorIJK()), 0.0);

    verify(segment);
  }

  @Test
  public void testGetCoverage() {
    reset(segment);
    configureSegmentMock();
    replay(segment);

    assertEquals(coverage, spkSegmentAdapter.getCoverage());

    verify(segment);
  }

  @Test
  public void testGetFrameID() {
    assertEquals(frameID, spkSegmentAdapter.getFrameID());
  }

  @Test
  public void testGetObserverID() {
    assertEquals(observerID, spkSegmentAdapter.getObserverID());
    assertEquals(observerID.hashCode(), spkSegmentAdapter.getObserverID().hashCode());
    assertNotSame(observerID, spkSegmentAdapter.getTargetID());

  }

  @Test
  public void testGetTargetID() {
    assertEquals(targetID, spkSegmentAdapter.getTargetID());
    assertEquals(targetID.hashCode(), spkSegmentAdapter.getTargetID().hashCode());
    assertNotSame(targetID, spkSegmentAdapter.getObserverID());
  }

}
