package picante.spice.kernel.tk.sclk;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.createMockBuilder;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import com.google.common.base.Preconditions;
import picante.data.list.AbstractGaugedRetrievable;
import picante.junit.AssertTools;
import picante.math.intervals.Interval;
import picante.math.intervals.UnwritableInterval;
import picante.spice.kernel.tk.lsk.UniformTimeProvider;

public class SCLKType1Test {

  private static final double TOLERANCE = 1e-15;

  private SCLKType1 pathologicalSclk;
  private SCLKType1 sclk;
  private TestTable recordTable;
  private TestTable pathologicalRecordTable;
  private TestParallelProvider parallelConverter;
  private TestPartitionTable partitionTable;
  private SCLKType1 delegate;
  private SCLKType1TickConverter mockConverter;
  private SCLK readout;

  @Before
  public void setUp() throws Exception {

    readout = new SCLK();

    pathologicalRecordTable = new TestTable(
        new double[][] {{0.0, 0.0, 0.5}, {1000.0, 1.0, 0.5}, {2000.0, 2.5, 1.0}}, 1000.0);
    recordTable = new TestTable(
        new double[][] {{0.0, 0.0, 0.5}, {1000.0, 0.5, 1.0}, {2000.0, 1.5, 0.5}}, 1000.0);
    parallelConverter = new TestParallelProvider();

    partitionTable = new TestPartitionTable(Arrays.asList(0., 4000., 0., 4000., 0., 2000.));

    pathologicalSclk = new SCLKType1(-300, parallelConverter, partitionTable,
        Arrays.asList(10000000., 1000.), Arrays.asList(0., 0.), pathologicalRecordTable);
    sclk = new SCLKType1(-250, parallelConverter, partitionTable, Arrays.asList(10000., 1000.),
        Arrays.asList(1., 0.), recordTable);

    delegate = createMockBuilder(SCLKType1.class).addMockedMethod("getTickConverter").createMock();
    mockConverter = createMock(SCLKType1TickConverter.class);
    expect(delegate.getTickConverter()).andReturn(mockConverter).anyTimes();

  }

  @Test
  public void testGetNumberOfFieldsDelegation() {
    expect(mockConverter.getNumberOfFields()).andReturn(2).times(1);
    replay(delegate, mockConverter);
    assertSame(mockConverter, delegate.getTickConverter());
    assertEquals(2, delegate.getNumberOfFields());
    verify(delegate, mockConverter);
  }

  @Test
  public void testGetFieldOffsetDelegation() {
    expect(mockConverter.getOffset(1)).andReturn(2.0).times(1);
    replay(delegate, mockConverter);
    assertSame(mockConverter, delegate.getTickConverter());
    assertEquals(2.0, delegate.getFieldOffset(1), 0.0);
    verify(delegate, mockConverter);
  }

  @Test
  public void testGetFieldModulusDelegation() {
    expect(mockConverter.getFieldModulus(1)).andReturn(2.0).times(1);
    replay(delegate, mockConverter);
    assertSame(mockConverter, delegate.getTickConverter());
    assertEquals(2.0, delegate.getFieldModulus(1), 0.0);
    verify(delegate, mockConverter);
  }

  @Test
  public void testGetTicksPerFieldDelegation() {
    expect(mockConverter.getTicksPerField(1)).andReturn(2.0).times(1);
    replay(delegate, mockConverter);
    assertSame(mockConverter, delegate.getTickConverter());
    assertEquals(2.0, delegate.getTicksPerField(1), 0.0);
    verify(delegate, mockConverter);
  }


  @Test
  public void testComputeMaxEncodedSCLK() throws Exception {
    partitionTable =
        new TestPartitionTable(Arrays.asList(0.0, 24.0, 0.0, 12.0, 0.0, 24.0, 10.0, 24.0));

    assertEquals(74.0, SCLKKernel.computeMaxEncodedSCLK(partitionTable), 0.0);

  }

  @Test
  public void testGetID() {
    assertEquals(-250, sclk.getID());
    assertEquals(-300, pathologicalSclk.getID());
  }

  @Test
  public void testGetMaxEncodedSCLK() {
    assertEquals(10000.0, sclk.getMaxEncodedSCLK(), 0.0);
    assertEquals(10000.0, pathologicalSclk.getMaxEncodedSCLK(), 0.0);
  }

  @Test
  public void testConvertToEncodedSclkContinuousCase() {
    assertEquals(0.0, sclk.convertToEncodedSclk(0.0), 0.0);
    assertEquals(500.0, sclk.convertToEncodedSclk(0.125), 0.0);
    assertEquals(999.6, sclk.convertToEncodedSclk(0.2499), 0.0);
    assertEquals(1000.0, sclk.convertToEncodedSclk(0.25), 0.0);
    assertEquals(1500.0, sclk.convertToEncodedSclk(0.5), 0.0);
    assertEquals(2000.0, sclk.convertToEncodedSclk(0.75), 0.0);
    assertEquals(3000.0, sclk.convertToEncodedSclk(1.0), 0.0);
    assertEquals(10000.0, sclk.convertToEncodedSclk(2.75), 0.0);
  }

  @Test(expected = SCLKEvaluationException.class)
  public void testConvertToEncodedSclkPrecedesException() {
    sclk.convertToEncodedSclk(-10);
  }

  @Test(expected = SCLKEvaluationException.class)
  public void testConvertToEncodedSclkExceedsException() {
    sclk.convertToEncodedSclk(2.750001);
  }

  @Test
  public void testConvertToTDBContinuousCase() {
    assertEquals(0.0, sclk.convertToTDB(0.0), 0.0);
    assertEquals(0.125, sclk.convertToTDB(500.0), 0.0);
    assertEquals(0.2499, sclk.convertToTDB(999.6), 0.0);
    assertEquals(0.25, sclk.convertToTDB(1000.0), 0.0);
    assertEquals(0.5, sclk.convertToTDB(1500.0), 0.0);
    assertEquals(0.75, sclk.convertToTDB(2000.0), 0.0);
    assertEquals(1.0, sclk.convertToTDB(3000.0), 0.0);
    assertEquals(2.75, sclk.convertToTDB(10000.0), 0.0);
  }

  @Test(expected = SCLKEvaluationException.class)
  public void testConvertToTDBPrecedesException() {
    sclk.convertToTDB(-1.0);
  }

  @Test(expected = SCLKEvaluationException.class)
  public void testConvertToTDBExceedsException() {
    sclk.convertToTDB(10000.01);
  }

  @Test
  public void testConvertToEncodedSclkDiscontinuousCase() {
    assertEquals(0.0, pathologicalSclk.convertToEncodedSclk(0.0), 0.0);
    assertEquals(500.0, pathologicalSclk.convertToEncodedSclk(0.125), 0.0);
    assertEquals(999.6, pathologicalSclk.convertToEncodedSclk(0.2499), 0.0);
    assertEquals(1000.0, pathologicalSclk.convertToEncodedSclk(0.25), 0.0);
    assertEquals(1500.0, pathologicalSclk.convertToEncodedSclk(0.375), 0.0);
    AssertTools.assertRelativeEquality(1999.6, pathologicalSclk.convertToEncodedSclk(0.4999),
        TOLERANCE);
    assertEquals(1000.0, pathologicalSclk.convertToEncodedSclk(0.5), 0.0);
    assertEquals(1500.0, pathologicalSclk.convertToEncodedSclk(0.625), 0.0);
    assertEquals(3999.6, pathologicalSclk.convertToEncodedSclk(1.2499), 0.0);
    assertEquals(2000.0, pathologicalSclk.convertToEncodedSclk(1.25), 0.0);
    assertEquals(2500.0, pathologicalSclk.convertToEncodedSclk(1.5), 0.0);
  }

  @Test
  public void testConvertToTDBDiscontinuousCase() {
    assertEquals(0.0, pathologicalSclk.convertToTDB(0.0), 0.0);
    assertEquals(0.125, pathologicalSclk.convertToTDB(500.0), 0.0);
    assertEquals(0.2499, pathologicalSclk.convertToTDB(999.6), 0.0);
    assertEquals(0.5, pathologicalSclk.convertToTDB(1000.0), 0.0);
    assertEquals(0.625, pathologicalSclk.convertToTDB(1500.0), 0.0);
    assertEquals(1.25, pathologicalSclk.convertToTDB(2000.0), 0.0);
    assertEquals(1.5, pathologicalSclk.convertToTDB(2500.0), 0.0);
  }

  @Test
  public void testGetNumberOfPartitions() {
    assertEquals(3, pathologicalSclk.getNumberOfPartitions());
    assertEquals(3, sclk.getNumberOfPartitions());
  }

  @Test
  public void testIsValidSCLK() {
    readout.set(5, 1, 2);
    assertFalse(pathologicalSclk.isValidSCLK(readout));

    readout.set(0, 0, 0);
    assertFalse(pathologicalSclk.isValidSCLK(readout));

    readout.set(1, 1, 2, 3);
    assertFalse(pathologicalSclk.isValidSCLK(readout));

    readout.set(1, -1);
    assertFalse(pathologicalSclk.isValidSCLK(readout));

    readout.set(1, 4);
    assertTrue(pathologicalSclk.isValidSCLK(readout));

    readout.set(1, 4, 1);
    assertFalse(pathologicalSclk.isValidSCLK(readout));

    readout.set(3, 2, 0);
    assertTrue(pathologicalSclk.isValidSCLK(readout));

    readout.set(3, 2, 1);
    assertFalse(pathologicalSclk.isValidSCLK(readout));

    readout.set(5, 1, 2);
    assertFalse(sclk.isValidSCLK(readout));

    readout.set(0, 0, 0);
    assertFalse(sclk.isValidSCLK(readout));

    readout.set(1, 1, 2, 3);
    assertFalse(sclk.isValidSCLK(readout));

    readout.set(1, -1);
    assertFalse(sclk.isValidSCLK(readout));

    readout.set(1, 4);
    assertTrue(sclk.isValidSCLK(readout));

    readout.set(1, 4, 1);
    assertTrue(sclk.isValidSCLK(readout));

    readout.set(1, 5, 0);
    assertTrue(sclk.isValidSCLK(readout));

    readout.set(1, 5, 1);
    assertFalse(sclk.isValidSCLK(readout));

    readout.set(3, 3, 0);
    assertTrue(sclk.isValidSCLK(readout));

    readout.set(3, 3, 1);
    assertFalse(sclk.isValidSCLK(readout));
  }

  @Test
  public void testConvertToEncodedSclkSclk() {

    /*
     * Verify that the encodedSCLK values on the partition boundaries are exactly the same.
     */
    readout.set(1, 5, 0);
    double actual = sclk.convertToEncodedSclk(readout);
    double expected = 4000.0;
    assertEquals(expected, actual, 0.0);

    readout.set(2, 1, 0);
    actual = sclk.convertToEncodedSclk(readout);
    expected = 4000.0;
    assertEquals(expected, actual, 0.0);

    readout.set(2, 5, 0);
    actual = sclk.convertToEncodedSclk(readout);
    expected = 8000.0;
    assertEquals(expected, actual, 0.0);

    readout.set(3, 1, 0);
    actual = sclk.convertToEncodedSclk(readout);
    expected = 8000.0;
    assertEquals(expected, actual, 0.0);

    /*
     * And now that promotion from the minor field is automatic.
     */
    readout.set(1, 1, 4000);
    actual = sclk.convertToEncodedSclk(readout);
    expected = 4000.0;
    assertEquals(expected, actual, 0.0);

    /*
     * Lastly some intermediate values for simple sanity check.
     */
    readout.set(1, 1, 2000);
    actual = sclk.convertToEncodedSclk(readout);
    expected = 2000.0;
    assertEquals(expected, actual, 0.0);

    readout.set(1, 2, 2000);
    actual = sclk.convertToEncodedSclk(readout);
    expected = 3000.0;
    assertEquals(expected, actual, 0.0);

    readout.set(1, 2, 2001);
    actual = sclk.convertToEncodedSclk(readout);
    expected = 3001.0;
    assertEquals(expected, actual, 0.0);

    /*
     * Verify that the encodedSCLK values on the partition boundaries are exactly the same.
     */
    readout.set(1, 4, 0);
    actual = pathologicalSclk.convertToEncodedSclk(readout);
    expected = 4000.0;
    assertEquals(expected, actual, 0.0);

    readout.set(2, 0, 0);
    actual = pathologicalSclk.convertToEncodedSclk(readout);
    expected = 4000.0;
    assertEquals(expected, actual, 0.0);

    readout.set(2, 4, 0);
    actual = pathologicalSclk.convertToEncodedSclk(readout);
    expected = 8000.0;
    assertEquals(expected, actual, 0.0);

    readout.set(3, 0, 0);
    actual = pathologicalSclk.convertToEncodedSclk(readout);
    expected = 8000.0;
    assertEquals(expected, actual, 0.0);

    /*
     * And now that promotion from the minor field is automatic.
     */
    readout.set(1, 0, 4000);
    actual = pathologicalSclk.convertToEncodedSclk(readout);
    expected = 4000.0;
    assertEquals(expected, actual, 0.0);

    /*
     * Lastly some intermediate values for simple sanity check.
     */
    readout.set(1, 0, 2000);
    actual = pathologicalSclk.convertToEncodedSclk(readout);
    expected = 2000.0;
    assertEquals(expected, actual, 0.0);

    readout.set(1, 1, 2000);
    actual = pathologicalSclk.convertToEncodedSclk(readout);
    expected = 3000.0;
    assertEquals(expected, actual, 0.0);

    readout.set(1, 1, 2001);
    actual = pathologicalSclk.convertToEncodedSclk(readout);
    expected = 3001.0;
    assertEquals(expected, actual, 0.0);

  }

  @Test(expected = SCLKEvaluationException.class)
  public void testConvertToEncodedSclkSclkPartitionException() {
    readout.set(4, 1, 0);
    sclk.convertToEncodedSclk(readout);
  }

  @Test(expected = SCLKEvaluationException.class)
  public void testConvertToEncodedSclkSclkBadSclkException() {
    readout.set(4, 1, 0, 0);
    sclk.convertToEncodedSclk(readout);
  }

  @Test(expected = SCLKEvaluationException.class)
  public void testConvertToEncodedSclkSclkOutOfBoundsException() {
    readout.set(3, 4, 200);
    sclk.convertToEncodedSclk(readout);
  }

  @Test
  public void testConvertToSclk() {

    SCLK expected = new SCLK();

    /*
     * Test the partition boundary cases. The conversion prefers the latest partition of the two.
     * Remember, partition boundaries map to equivalent encoded SCLKs.
     */
    SCLK result = sclk.convertToSclk(0.0, readout);
    assertSame(result, readout);
    expected.set(1, 1, 0);
    assertEquals(expected, readout);

    result = sclk.convertToSclk(4000.0, readout);
    assertSame(result, readout);
    expected.set(2, 1, 0);
    assertEquals(expected, readout);

    result = sclk.convertToSclk(8000.0, readout);
    assertSame(result, readout);
    expected.set(3, 1, 0);
    assertEquals(expected, readout);

    result = sclk.convertToSclk(10000.0, readout);
    assertSame(result, readout);
    expected.set(3, 3, 0);
    assertEquals(expected, readout);

    /*
     * And now some in between values.
     */
    result = sclk.convertToSclk(1000.0, readout);
    assertSame(result, readout);
    expected.set(1, 2, 0);
    assertEquals(expected, readout);

    result = sclk.convertToSclk(1001.0, readout);
    assertSame(result, readout);
    expected.set(1, 2, 1);
    assertEquals(expected, readout);

    result = sclk.convertToSclk(1001.5, readout);
    assertSame(result, readout);
    expected.set(1, 2, 2);
    assertEquals(expected, readout);

    result = sclk.convertToSclk(1001.5 - Math.ulp(1001.5), readout);
    assertSame(result, readout);
    expected.set(1, 2, 1);
    assertEquals(expected, readout);

  }

  @Test(expected = SCLKEvaluationException.class)
  public void testConvertToSclkEncodedSclkTooSmallException() {
    sclk.convertToSclk(-1.0, readout);
  }

  @Test(expected = SCLKEvaluationException.class)
  public void testConvertToSclkEncodedSclkTooLargeException() {
    sclk.convertToSclk(10000.5, readout);
  }

}


class TestParallelProvider implements UniformTimeProvider {

  @Override
  public double convertToUniformTime(double tdb) {
    return 2.0 * tdb;
  }

  @Override
  public double convertToTDB(double parallelTime) {
    return parallelTime / 2.0;
  }

}


class TestTable extends AbstractGaugedRetrievable<SCLKType1Record> {

  private final double[][] coeffs;
  private final double ticksPerMostSignificantCount;

  public TestTable(double[][] coeffs, double ticksPerMostSignificantCount) {
    this.coeffs = coeffs;
    this.ticksPerMostSignificantCount = ticksPerMostSignificantCount;
  }

  @Override
  public double getGauge(int index) {
    return coeffs[index][0];
  }

  @Override
  public int size() {
    return coeffs.length;
  }

  @Override
  public SCLKType1Record get(int index, SCLKType1Record buffer) {
    buffer.setRecord(coeffs[index][0], coeffs[index][1], coeffs[index][2],
        ticksPerMostSignificantCount);
    return buffer;
  }

}


class TestPartitionTable extends AbstractGaugedRetrievable<Interval> {

  private final List<Double> encodedSCLKs;
  private final List<UnwritableInterval> intervals;

  public TestPartitionTable(List<Double> intervals) {
    Preconditions.checkArgument(intervals.size() % 2 == 0);
    this.encodedSCLKs = new ArrayList<Double>();
    this.intervals = new ArrayList<UnwritableInterval>(intervals.size() / 2);
    double ticks = 0;
    for (int i = 0; i < intervals.size() / 2; i++) {
      UnwritableInterval interval =
          new UnwritableInterval(intervals.get(2 * i), intervals.get(2 * i + 1));
      this.intervals.add(interval);
      this.encodedSCLKs.add(ticks);
      ticks += interval.getLength();
    }
  }

  @Override
  public double getGauge(int index) {
    return encodedSCLKs.get(index);
  }

  @Override
  public Interval get(int index, Interval buffer) {
    return buffer.setTo(intervals.get(index));
  }

  @Override
  public int size() {
    return intervals.size();
  }

}
