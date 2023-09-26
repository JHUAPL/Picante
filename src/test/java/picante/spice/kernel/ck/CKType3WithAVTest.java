package picante.spice.kernel.ck;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static picante.junit.AssertTools.assertComponentEquals;
import org.junit.Before;
import org.junit.Test;
import picante.data.list.AbstractGaugedRetrievable;
import picante.math.intervals.Interval;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.TimeOutOfBoundsException;
import picante.mechanics.rotations.Quaternion;
import picante.mechanics.rotations.WrapperWithRate;

public class CKType3WithAVTest {

  private final static double TOLERANCE = 1.0E-15;

  private double[][] qdata =
      {{-0.00229000000000, 0.83699000000000, 0.00623000000000, 0.54717792618124},
          {-0.00235000000000, 0.83912000000000, 0.00616000000000, 0.54390638670639},
          {-0.00238000000000, 0.84135000000000, 0.00613000000000, 0.54045067878577},
          {-0.00241000000000, 0.84352000000000, 0.00610000000000, 0.53705771710311},
          {-0.00244000000000, 0.84565000000000, 0.00607000000000, 0.53369774123562}};

  private double[] times =
      {8.709648044E+9, 8.709649864E+9, 8.709651684E+9, 8.709653504E+9, 8.709655324E+9};

  private double[][] avdata = {{0.00002537483673, 0.00431760227027, -0.00004353256607},
      {0.00002527973349, 0.00440506078599, -0.00004298120768},
      {0.00001578362916, 0.00447288147018, -0.00001673284629},
      {0.00001557320485, 0.00439896590788, -0.00001718616874},
      {0.00000680024873, 0.00452062084461, -0.00003032140302}};

  private CKType3WithAV segment;
  private CKType3WithAV gapSegment;
  private CkType3AvRecordTable table;

  @Before
  public void setUp() throws Exception {

    table = new CkType3AvRecordTable(times, qdata, avdata);
    TimeListArrayWrapper intTable = new TimeListArrayWrapper(new double[] {times[0]});

    TimeListArrayWrapper gapTable = new TimeListArrayWrapper(new double[] {times[0], times[2]});

    segment =
        new CKType3WithAV("Segment", 50, 250, times[0], times[times.length - 1], table, intTable);

    gapSegment = new CKType3WithAV("GapSegment", 100, 200, times[0], times[times.length - 1], table,
        gapTable);

  }

  @Test
  public void testGetCoverage() {

    /*
     * Simply check to see that the coverage object returned makes sense.
     */
    CKCoverage coverage = segment.getCoverage();

    /*
     * This coverage should exactly match the start and end of the times array.
     */
    Interval interval = coverage.getNextInterval(-Double.MAX_VALUE, new Interval());
    assertEquals(times[0], interval.getBegin(), 0.0);
    assertEquals(times[times.length - 1], interval.getEnd(), 0.0);

    /*
     * Make sure the next interval retrieval blows up with the appropriate exception.
     */
    try {
      coverage.getNextInterval(interval.getEnd(), new Interval());
      fail("Expected exception not thrown.");
    } catch (TimeOutOfBoundsException te) {
    }

    /*
     * This segment has two distinct intervals of coverage.
     */
    coverage = gapSegment.getCoverage();

    coverage.getNextInterval(-Double.MAX_VALUE, interval);
    assertEquals(times[0], interval.getBegin(), 0.0);
    assertEquals(times[1], interval.getEnd(), 0.0);

    coverage.getNextInterval(interval.getEnd(), interval);
    assertEquals(times[2], interval.getBegin(), 0.0);
    assertEquals(times[times.length - 1], interval.getEnd(), 0.0);

    try {
      coverage.getNextInterval(interval.getEnd(), new Interval());
      fail("Expected exception not thrown.");
    } catch (TimeOutOfBoundsException te) {
    }
  }

  @Test
  public void testTransformCaching() {

    int startCounter = table.getQueryCount();

    /*
     * Perform an attitude lookup that will cause a cache hit.
     */
    segment.getTransform((times[3] + times[2]) / 2.0, new RotationMatrixIJK());

    /*
     * Check that the queryCounter was updated.
     */
    int updatedCounter = table.getQueryCount();
    assertNotSame(startCounter, updatedCounter);

    /*
     * Now perform a query that lies between times[3] and times[2], which should not result in an
     * interrogation of the record table.
     */
    segment.getTransform((0.9 * times[3] + 0.1 * times[2]), new RotationMatrixIJK());

    int counter = table.getQueryCount();

    assertEquals(updatedCounter, counter);

    /*
     * And check the two boundary cases. This should not result in an update as the cache is valid
     * from times including the left endpoint up to the right (but not including).
     */
    segment.getTransform(times[2], new RotationMatrixIJK());
    counter = table.getQueryCount();
    assertEquals(updatedCounter, counter);

    segment.getTransform(times[3], new RotationMatrixIJK());
    counter = table.getQueryCount();
    assertNotSame(updatedCounter, counter);

  }

  @Test
  public void testRateCaching() {

    int startCounter = table.getQueryCount();

    /*
     * Perform an attitude lookup that will cause a cache hit.
     */
    segment.getAngularRate((times[3] + times[2]) / 2.0, new VectorIJK());

    /*
     * Check that the queryCounter was updated.
     */
    int updatedCounter = table.getQueryCount();
    assertNotSame(startCounter, updatedCounter);

    /*
     * Now perform a query that lies between times[3] and times[2], which should not result in an
     * interrogation of the record table.
     */
    segment.getAngularRate((0.9 * times[3] + 0.1 * times[2]), new VectorIJK());

    int counter = table.getQueryCount();

    assertEquals(updatedCounter, counter);

    /*
     * And check the two boundary cases. This should not result in an update as the cache is valid
     * from times including the left endpoint up to the right (but not including).
     */
    segment.getAngularRate(times[2], new VectorIJK());
    counter = table.getQueryCount();
    assertEquals(updatedCounter, counter);

    segment.getAngularRate(times[3], new VectorIJK());
    counter = table.getQueryCount();
    assertNotSame(updatedCounter, counter);

  }

  @Test
  public void testGetAngularRateAtRecordTimes() {

    VectorIJK buffer = new VectorIJK();
    VectorIJK expected = new VectorIJK();

    for (int i = 0; i < times.length; i++) {
      VectorIJK result = segment.getAngularRate(times[i], buffer);
      assertSame("At index: " + i, buffer, result);
      expected.setTo(avdata[i][0], avdata[i][1], avdata[i][2]);
      assertComponentEquals(expected, result, 0.0);

      result = gapSegment.getAngularRate(times[i], buffer);
      assertSame("At index: " + i, buffer, result);
      assertComponentEquals(expected, result, 0.0);
    }

  }

  @Test
  public void testGetAngularRateAtInterpolatedTimes() {

    /*
     * Check a time that is 0.25 off times[3] to times[4].
     */
    double queryTime = 0.75 * times[3] + 0.25 * times[4];

    VectorIJK buffer = new VectorIJK();
    segment.getAngularRate(queryTime, buffer);

    /*
     * We have already tested the interpolator, just make sure it is wired up properly under the
     * hood.
     */
    Type3Interpolator interpolator = new Type3Interpolator();

    VectorIJK expected = new VectorIJK();
    interpolator.interpolate(times[3], new VectorIJK(avdata[3]), times[4], new VectorIJK(avdata[4]),
        queryTime, expected);

    assertComponentEquals(expected, buffer, TOLERANCE);

    gapSegment.getAngularRate(queryTime, buffer);

    assertComponentEquals(expected, buffer, TOLERANCE);

  }

  @Test(expected = CKEvaluationException.class)
  public void testGetAngularRateInGap() {
    gapSegment.getAngularRate((times[1] + times[2]) / 2.0, new VectorIJK());
  }

  @Test(expected = CKEvaluationException.class)
  public void testAVLowerBoundTimeException() {
    segment.getAngularRate(times[0] - 0.1, new VectorIJK());
  }

  @Test(expected = CKEvaluationException.class)
  public void testAVUpperBoundTimeException() {
    segment.getAngularRate(times[times.length - 1] + 0.1, new VectorIJK());
  }

  @Test(expected = CKEvaluationException.class)
  public void testAVGapSegmentLowerBoundTimeException() {
    gapSegment.getAngularRate(times[0] - 0.1, new VectorIJK());
  }

  @Test(expected = CKEvaluationException.class)
  public void testAVGapSegmentUpperBoundTimeException() {
    gapSegment.getAngularRate(times[times.length - 1] + 0.1, new VectorIJK());
  }

  @Test
  public void testGetTransformAtRecordTimes() {

    RotationMatrixIJK buffer = new RotationMatrixIJK();
    RotationMatrixIJK expected = new RotationMatrixIJK();
    Quaternion q = new Quaternion();

    for (int i = 0; i < times.length; i++) {
      RotationMatrixIJK result = segment.getTransform(times[i], buffer);
      assertSame("At index: " + i, buffer, result);
      q.setTo(qdata[i][0], qdata[i][1], qdata[i][2], qdata[i][3]);
      q.getRotation(expected);
      assertComponentEquals(expected, result, TOLERANCE);

      result = gapSegment.getTransform(times[i], buffer);
      assertSame("At index: " + i, buffer, result);
      q.setTo(qdata[i][0], qdata[i][1], qdata[i][2], qdata[i][3]);
      q.getRotation(expected);
      assertComponentEquals(expected, result, TOLERANCE);
    }

  }

  @Test
  public void testGetTransformAtInterpolatedTimes() {

    /*
     * Check a time that is 0.25 off times[3] to times[4].
     */
    double queryTime = 0.75 * times[3] + 0.25 * times[4];

    RotationMatrixIJK buffer = new RotationMatrixIJK();
    segment.getTransform(queryTime, buffer);

    /*
     * We have already tested the interpolator, just make sure it is wired up properly under the
     * hood.
     */
    Type3Interpolator interpolator = new Type3Interpolator();

    RotationMatrixIJK expected = new RotationMatrixIJK();
    interpolator.interpolate(times[3],
        new Quaternion(qdata[3][0], qdata[3][1], qdata[3][2], qdata[3][3])
            .getRotation(new RotationMatrixIJK()),
        times[4], new Quaternion(qdata[4][0], qdata[4][1], qdata[4][2], qdata[4][3])
            .getRotation(new RotationMatrixIJK()),
        queryTime, expected);

    assertComponentEquals(expected, buffer, TOLERANCE);

    gapSegment.getTransform(queryTime, buffer);

    assertComponentEquals(expected, buffer, TOLERANCE);

  }

  @Test(expected = CKEvaluationException.class)
  public void testGetTransformExceptionInGap() {
    gapSegment.getTransform((times[1] + times[2]) / 2.0, new RotationMatrixIJK());
  }

  @Test(expected = CKEvaluationException.class)
  public void testLowerBoundTimeException() {
    segment.getTransform(times[0] - 0.1, new RotationMatrixIJK());
  }

  @Test(expected = CKEvaluationException.class)
  public void testUpperBoundTimeException() {
    segment.getTransform(times[times.length - 1] + 0.1, new RotationMatrixIJK());
  }

  @Test(expected = CKEvaluationException.class)
  public void testGapSegmentLowerBoundTimeException() {
    gapSegment.getTransform(times[0] - 0.1, new RotationMatrixIJK());
  }

  @Test(expected = CKEvaluationException.class)
  public void testGapSegmentUpperBoundTimeException() {
    gapSegment.getTransform(times[times.length - 1] + 0.1, new RotationMatrixIJK());
  }

  @Test
  public void testGetType() {
    assertEquals(3, segment.getType());
    assertEquals(3, gapSegment.getType());
  }

  @Test
  public void testHasAngularVelocity() {
    assertTrue(segment.hasAngularVelocity());
    assertTrue(gapSegment.hasAngularVelocity());
  }

  @Test
  public void testGetInstrumentID() {
    assertEquals(50, segment.getInstrumentID());
    assertEquals(100, gapSegment.getInstrumentID());
  }

  @Test
  public void testGetReferenceID() {
    assertEquals(250, segment.getReferenceID());
    assertEquals(200, gapSegment.getReferenceID());
  }

  @Test
  public void testGetName() {
    assertEquals("Segment", segment.getName());
    assertEquals("GapSegment", gapSegment.getName());
  }

  @Test
  public void testGetInitialEncodedSCLK() {
    assertEquals(times[0], segment.getInitialEncodedSCLK(), 0.0);
    assertEquals(times[0], gapSegment.getInitialEncodedSCLK(), 0.0);
  }

  @Test
  public void testGetFinalEncodedSCLK() {
    assertEquals(times[times.length - 1], segment.getFinalEncodedSCLK(), 0.0);
    assertEquals(times[times.length - 1], gapSegment.getFinalEncodedSCLK(), 0.0);
  }

}


class CkType3AvRecordTable extends AbstractGaugedRetrievable<WrapperWithRate<Quaternion>> {

  private double[] times;
  private double[][] qdata;
  private double[][] avdata;
  private int queryCount = 0;

  public CkType3AvRecordTable(double[] times, double[][] qdata, double[][] avdata) {
    this.times = times;
    this.qdata = qdata;
    this.avdata = avdata;
  }

  @Override
  public double getGauge(int index) {
    return times[index];
  }

  @Override
  public int size() {
    return times.length;
  }

  @Override
  public WrapperWithRate<Quaternion> get(int index, WrapperWithRate<Quaternion> buffer) {
    queryCount++;
    buffer.getRotation().setTo(qdata[index][0], qdata[index][1], qdata[index][2], qdata[index][3]);
    buffer.getRate().setTo(avdata[index][0], avdata[index][1], avdata[index][2]);
    return buffer;
  }

  public int getQueryCount() {
    return queryCount;
  }

}
