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
import picante.math.intervals.Interval;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.Coverages;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.mechanics.rotations.AxisAndAngle;
import picante.mechanics.rotations.MatrixWrapper;
import picante.mechanics.rotations.WrapperWithRate;
import picante.spice.kernel.ck.CKCoverage;
import picante.spice.kernel.ck.CKSegmentWithAV;
import picante.spice.kernel.tk.sclk.EncodedSCLKConverter;

public class CKSegmentWithAVAdapterTest {

  private CKSegmentWithAVAdapter segmentAdapter;

  private FrameID fromID;
  private FrameID toID;
  private EncodedSCLKConverter converter;

  private CKSegmentWithAV segment;

  private ConcreteCoverage intervalCoverage;
  private TdbCoverage tdbCoverage;

  private RotationMatrixIJK sampleRotation = new RotationMatrixIJK();
  private VectorIJK sampleAngularRate = new VectorIJK();

  @Before
  public void setUp() throws Exception {
    fromID = createMock(FrameID.class);
    toID = createMock(FrameID.class);
    converter = new Convert();
    segment = createMock(CKSegmentWithAV.class);

    intervalCoverage = new ConcreteCoverage(Coverages.create(8.0, 12.0));
    tdbCoverage = new TdbCoverage(intervalCoverage, converter);

    new AxisAndAngle(3.8, 2.9, 5.1, Math.toRadians(83.9)).getRotation(sampleRotation);

    sampleAngularRate = new VectorIJK(1.0, 2.0, 3.0);

    reset(segment);
    configureMock();
    replay(segment);

    segmentAdapter = new CKSegmentWithAVAdapter(fromID, toID, converter, segment);
  }

  void configureMock() {

    CaptureAndAnswer<RotationMatrixIJK> captureTransform = CaptureAndAnswer.create(sampleRotation);

    CaptureAndAnswer<VectorIJK> captureRate = CaptureAndAnswer.create(sampleAngularRate);

    expect(segment.getCoverage()).andReturn(intervalCoverage).anyTimes();

    expect(segment.getTransform(eq(0.0), capture(captureTransform.getCapture())))
        .andAnswer(captureTransform).anyTimes();

    expect(segment.getAngularRate(eq(0.0), capture(captureRate.getCapture())))
        .andAnswer(captureRate).anyTimes();
  }

  @Test
  public void testCKSegmentWithAVAdapter() {
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
  public void testGetStateTransform() {

    reset(segment);
    configureMock();
    replay(segment);

    WrapperWithRate<MatrixWrapper> r =
        new WrapperWithRate<MatrixWrapper>(new MatrixWrapper(sampleRotation), sampleAngularRate);

    StateTransform expected = r.getTransform(new StateTransform()).invert();
    StateTransform actual =
        segmentAdapter.getStateTransform(converter.convertToTDB(0.0), new StateTransform());

    assertEquals(expected.getRotation(), actual.getRotation());
    assertComponentEquals(expected.getRotation(), actual.getRotation(), 0.0);

    assertEquals(expected.getRotationDerivative(), actual.getRotationDerivative());
    assertComponentEquals(expected.getRotationDerivative(), actual.getRotationDerivative(), 0.0);

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
    public Interval getTDBRange(Interval buffer) {
      return buffer.setTo(Interval.ALL_DOUBLES);
    }

    @Override
    public Interval getEncodedSclkRange(Interval buffer) {
      return buffer.setTo(Interval.ALL_DOUBLES);
    }

  }

}
