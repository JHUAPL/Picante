package picante.spice.kernel.ck;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.fail;
import static picante.junit.AssertTools.assertComponentEquals;
import org.junit.Before;
import org.junit.Test;
import picante.data.list.AbstractGaugedRetrievable;
import picante.math.intervals.Interval;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.TimeOutOfBoundsException;
import picante.mechanics.rotations.Quaternion;


public class CKType3Test {

  private final static double TOLERANCE = 1.0E-15;

  private double[][] qdata =
      {{-0.00229000000000, 0.83699000000000, 0.00623000000000, 0.54717792618124},
          {-0.00235000000000, 0.83912000000000, 0.00616000000000, 0.54390638670639},
          {-0.00238000000000, 0.84135000000000, 0.00613000000000, 0.54045067878577},
          {-0.00241000000000, 0.84352000000000, 0.00610000000000, 0.53705771710311},
          {-0.00244000000000, 0.84565000000000, 0.00607000000000, 0.53369774123562}};

  private double[] times =
      {8.709648044E+9, 8.709649864E+9, 8.709651684E+9, 8.709653504E+9, 8.709655324E+9};

  private CKType3 segment;
  private CKType3 gapSegment;
  private CkType3RecordTable table;

  @Before
  public void setUp() throws Exception {

    table = new CkType3RecordTable(times, qdata);
    TimeListArrayWrapper intTable = new TimeListArrayWrapper(new double[] {times[0]});

    TimeListArrayWrapper gapTable = new TimeListArrayWrapper(new double[] {times[0], times[2]});

    segment = new CKType3("Segment", 50, 250, times[0], times[times.length - 1], table, intTable);
    gapSegment =
        new CKType3("GapSegment", 100, 200, times[0], times[times.length - 1], table, gapTable);

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
  public void testCaching() {

    int startCounter = table.getQueryCount();

    /*
     * Perform an attitude lookup that will likely cause a cache hit.
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
    assertFalse(segment.hasAngularVelocity());
    assertFalse(gapSegment.hasAngularVelocity());
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


class CkType3RecordTable extends AbstractGaugedRetrievable<Quaternion> {

  private double[] times;
  private double[][] qdata;
  private int queryCount = 0;

  public CkType3RecordTable(double[] times, double[][] qdata) {
    this.times = times;
    this.qdata = qdata;
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
  public Quaternion get(int index, Quaternion buffer) {
    queryCount++;
    return buffer.setTo(qdata[index][0], qdata[index][1], qdata[index][2], qdata[index][3]);
  }

  public int getQueryCount() {
    return queryCount;
  }

}
