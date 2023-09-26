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
import org.junit.Before;
import org.junit.Test;
import picante.junit.CaptureAndAnswer;
import picante.math.intervals.Interval;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.Coverages;
import picante.mechanics.FrameID;
import picante.mechanics.rotations.AxisAndAngle;
import picante.spice.kernel.ck.CKCoverage;
import picante.spice.kernel.ck.CKSegment;
import picante.spice.kernel.tk.sclk.EncodedSCLKConverter;

public class CKSegmentAdapterTest {

  private CKSegmentAdapter segmentAdapter;

  private FrameID fromID;
  private FrameID toID;
  private EncodedSCLKConverter converter;

  private ConcreteCoverage intervalCoverage;
  private TdbCoverage tdbCoverage;

  private CKSegment segment;

  private RotationMatrixIJK sampleRotation = new RotationMatrixIJK();

  @Before
  public void setUp() throws Exception {
    fromID = createMock(FrameID.class);
    toID = createMock(FrameID.class);
    converter = new Convert();
    segment = createMock(CKSegment.class);

    intervalCoverage = new ConcreteCoverage(Coverages.create(8.0, 12.0));
    tdbCoverage = new TdbCoverage(intervalCoverage, converter);

    new AxisAndAngle(3.8, 2.9, 5.1, Math.toRadians(83.9)).getRotation(sampleRotation);

    reset(segment);
    configureMock();
    replay(segment);

    segmentAdapter = new CKSegmentAdapter(fromID, toID, converter, segment);

  }

  void configureMock() {

    CaptureAndAnswer<RotationMatrixIJK> capture = CaptureAndAnswer.create(sampleRotation);

    expect(segment.getCoverage()).andReturn(intervalCoverage).anyTimes();
    expect(segment.getTransform(eq(0.0), capture(capture.getCapture()))).andAnswer(capture)
        .anyTimes();
  }

  @Test
  public void testCKSegmentAdapter() {
    reset(segment);
    configureMock();
    replay(segment);

    assertEquals(fromID, segmentAdapter.getFromID());
    assertNotSame(fromID, segmentAdapter.getToID());

    assertEquals(toID, segmentAdapter.getToID());
    assertNotSame(toID, segmentAdapter.getFromID());

    Coverage actualCoverage = segmentAdapter.getCoverage();
    Coverage expectedCoverage = tdbCoverage;

    assertEquals(expectedCoverage.getBoundingInterval(new Interval()),
        actualCoverage.getBoundingInterval(new Interval()));

    verify(segment);
  }

  @Test
  public void testGetCoverage() {

    reset(segment);
    configureMock();
    replay(segment);

    Coverage actualCoverage = segmentAdapter.getCoverage();
    Coverage expectedCoverage = tdbCoverage;

    assertEquals(expectedCoverage.getBoundingInterval(new Interval()),
        actualCoverage.getBoundingInterval(new Interval()));

    assertEquals(sampleRotation.createTranspose(),
        segmentAdapter.getTransform(converter.convertToTDB(0.0), new RotationMatrixIJK()));

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
  public void testGetTransform() {
    reset(segment);
    configureMock();
    replay(segment);

    assertEquals(sampleRotation.createTranspose(),
        segmentAdapter.getTransform(converter.convertToTDB(0.0), new RotationMatrixIJK()));

    verify(segment);
  }

  class ConcreteCoverage implements CKCoverage {

    Coverage coverage;

    public ConcreteCoverage(Coverage coverage) {
      this.coverage = coverage;
    }

    @Override
    public boolean contains(double time) {
      return coverage.contains(time);
    }

    @Override
    public Interval getBoundingInterval(Interval buffer) {
      return coverage.getBoundingInterval(buffer);
    }

    @Override
    public Interval getBracketingInterval(double time, Interval buffer) {
      return coverage.getBracketingInterval(time, buffer);
    }

    @Override
    public boolean hasNextInterval(double time) {
      return coverage.hasNextInterval(time);
    }

    @Override
    public Interval getNextInterval(double time, Interval buffer) {
      return coverage.getNextInterval(time, buffer);
    }

    @Override
    public boolean equals(@SuppressWarnings("unused") Object o) {
      throw new UnsupportedOperationException();
    }

    @Override
    public int hashCode() {
      throw new UnsupportedOperationException();
    }

  }

  @SuppressWarnings("unused")
  class Convert implements EncodedSCLKConverter {

    @Override
    public double convertToTDB(double encodedSCLK) {
      return ((encodedSCLK - 1.0) / 2.0);

    }

    @Override
    public double convertToEncodedSclk(double tdb) {
      return (tdb * 2.0 + 1.0);
    }

    @Override
    public Interval getTDBRange(Interval interval) {
      throw new UnsupportedOperationException();
    }

    @Override
    public Interval getEncodedSclkRange(Interval interval) {
      throw new UnsupportedOperationException();
    }

  }

}
