package picante.spice.kernel.tk.sclk;

import static org.easymock.EasyMock.capture;
import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.createMockBuilder;
import static org.easymock.EasyMock.eq;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.reset;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static picante.spice.kernel.tk.sclk.AssertionUtilities.assertFieldEquality;
import org.junit.Before;
import org.junit.Test;
import picante.data.list.GaugedRetrievableLLE;
import picante.junit.CaptureAndAnswer;
import picante.math.intervals.Interval;
import picante.math.intervals.UnwritableInterval;

public class SCLKKernelTest {

  private SCLKKernel mockedKernel;
  private TickConverter mockedTickConverter;
  private GaugedRetrievableLLE<Interval> mockedPartitionTable;
  private UnwritableInterval validEncodedSclkRange;

  @Before
  public void setUp() throws Exception {
    mockedTickConverter = createMock(TickConverter.class);
    mockedPartitionTable = createMock(GaugedRetrievableLLE.class);
    validEncodedSclkRange = new UnwritableInterval(0, 1000000);
    expect(mockedPartitionTable.size()).andReturn(2).anyTimes();
    expect(mockedPartitionTable.get(1, new Interval())).andReturn(new Interval(0, 500000));
    expect(mockedPartitionTable.getGauge(1)).andReturn(500000.0);
    replay(mockedPartitionTable);
    replay(mockedTickConverter);
    mockedKernel = createMockBuilder(SCLKKernel.class)
        .withConstructor(-100, mockedTickConverter, mockedPartitionTable, validEncodedSclkRange)
        .createMock();
    verify(mockedPartitionTable);
    verify(mockedTickConverter);
    reset(mockedPartitionTable);
    reset(mockedTickConverter);

  }

  @Test
  public void testComputeMaxEncodedSCLK() {
    GaugedRetrievableLLE<Interval> table = createMock(GaugedRetrievableLLE.class);

    expect(table.size()).andReturn(3).anyTimes();
    expect(table.get(2, new Interval())).andReturn(new Interval(100, 5000));
    expect(table.getGauge(2)).andReturn(5000.0);

    replay(table);
    assertEquals(9900.0, SCLKKernel.computeMaxEncodedSCLK(table), 0.0);
    verify(table);
  }

  @Test
  public void testGetID() {
    assertEquals(-100, mockedKernel.getID());
  }

  @Test
  public void testGetPartitionTable() {
    assertSame(mockedPartitionTable, mockedKernel.getPartitionTable());
  }

  @Test
  public void testGetNumberOfPartitions() {
    expect(mockedPartitionTable.size()).andReturn(2);
    replay(mockedPartitionTable);
    assertEquals(2, mockedKernel.getNumberOfPartitions());
    verify(mockedPartitionTable);
  }

  @Test
  public void testIsValidSCLKPartitionTooSmall() {

    SCLK sclk = new SCLK(-1, 0);
    expect(mockedPartitionTable.size()).andReturn(20).anyTimes();
    replay(mockedPartitionTable);
    assertFalse(mockedKernel.isValidSCLK(sclk));
    verify(mockedPartitionTable);
  }

  @Test
  public void testIsValidSCLKPartitionTooLarge() {

    SCLK sclk = new SCLK(21, 0);
    expect(mockedPartitionTable.size()).andReturn(20).anyTimes();
    replay(mockedPartitionTable);
    assertFalse(mockedKernel.isValidSCLK(sclk));
    verify(mockedPartitionTable);
  }

  @Test
  public void testIsValidSCLKPartitionWayTooLarge() {

    SCLK sclk = new SCLK(21, 0);
    expect(mockedPartitionTable.size()).andReturn(20).anyTimes();
    replay(mockedPartitionTable);
    assertFalse(mockedKernel.isValidSCLK(sclk));
    verify(mockedPartitionTable);
  }

  @Test
  public void testIsValidSCLKInvalidFields() {
    SCLK sclk = new SCLK(1, 1, 2, 3);
    expect(mockedPartitionTable.size()).andReturn(10).anyTimes();
    expect(mockedTickConverter.isValidClock(sclk)).andReturn(false);
    replay(mockedPartitionTable, mockedTickConverter);
    assertFalse(mockedKernel.isValidSCLK(sclk));
    verify(mockedPartitionTable, mockedTickConverter);
  }

  @Test
  public void testIsValidSCLKInvalidClockReadingForPartition() {
    SCLK sclk = new SCLK(1, 1, 2, 3);
    expect(mockedPartitionTable.size()).andReturn(10).anyTimes();
    expect(mockedTickConverter.isValidClock(sclk)).andReturn(true);
    expect(mockedTickConverter.convertToTicks(sclk)).andReturn(500.0);
    CaptureAndAnswer<Interval> capture = CaptureAndAnswer.create(new Interval(10000, 12000));
    expect(mockedPartitionTable.get(eq(0), capture(capture.getCapture()))).andAnswer(capture);
    replay(mockedPartitionTable, mockedTickConverter);
    assertFalse(mockedKernel.isValidSCLK(sclk));
    verify(mockedPartitionTable, mockedTickConverter);
  }

  @Test
  public void testIsValidSCLK() {
    SCLK sclk = new SCLK(1, 1, 2, 3);
    expect(mockedPartitionTable.size()).andReturn(10).anyTimes();
    expect(mockedTickConverter.isValidClock(sclk)).andReturn(true);
    expect(mockedTickConverter.convertToTicks(sclk)).andReturn(500.0);
    CaptureAndAnswer<Interval> capture = CaptureAndAnswer.create(new Interval(100.0, 12000.0));
    expect(mockedPartitionTable.get(eq(0), capture(capture.getCapture()))).andAnswer(capture);
    replay(mockedPartitionTable, mockedTickConverter);
    assertTrue(mockedKernel.isValidSCLK(sclk));
    verify(mockedPartitionTable, mockedTickConverter);
  }

  @Test(expected = SCLKEvaluationException.class)
  public void testConvertToSclkEncodedSclkOutOfBoundsException() {
    mockedKernel.convertToSclk(1000001.0, new SCLK());
  }

  @Test
  public void testConvertToSclk() {

    SCLK sclk = new SCLK();
    expect(mockedPartitionTable.indexLastLessThanOrEqualTo(10000.0)).andReturn(4);
    CaptureAndAnswer<Interval> capture = CaptureAndAnswer.create(new Interval(500, 1000));
    expect(mockedPartitionTable.get(eq(4), capture(capture.getCapture()))).andAnswer(capture);
    expect(mockedPartitionTable.getGauge(4)).andReturn(9700.0);

    CaptureAndAnswer<SCLK> sclkCapture = CaptureAndAnswer.create(new SCLK(1, 400L, 200L));

    expect(mockedTickConverter.convertToSCLK(eq(800.0), capture(sclkCapture.getCapture())))
        .andAnswer(sclkCapture);
    replay(mockedPartitionTable, mockedTickConverter);

    SCLK result = mockedKernel.convertToSclk(10000.0, sclk);
    assertSame(result, sclk);

    assertEquals(5, sclk.getPartition());
    assertFieldEquality(new SCLK(1, 400L, 200L), sclk);

    verify(mockedPartitionTable, mockedTickConverter);
  }

  @Test(expected = SCLKEvaluationException.class)
  public void testConvertToEncodedSclkInvalidPartitionException() {
    SCLK sclk = new SCLK(10, 1, 1);
    expect(mockedPartitionTable.size()).andReturn(5).anyTimes();
    replay(mockedPartitionTable);
    mockedKernel.convertToEncodedSclk(sclk);
    verify(mockedPartitionTable);
  }

  @Test(expected = SCLKEvaluationException.class)
  public void testConvertToEncodedSclkInvalidTickCountForPartitionException() {
    SCLK sclk = new SCLK(10, 1, 1);
    expect(mockedPartitionTable.size()).andReturn(10).anyTimes();

    CaptureAndAnswer<Interval> capture = CaptureAndAnswer.create(new UnwritableInterval(0, 4));
    expect(mockedPartitionTable.get(eq(9), capture(capture.getCapture()))).andAnswer(capture);
    replay(mockedPartitionTable);
    expect(mockedTickConverter.convertToTicks(sclk)).andReturn(50.0);
    replay(mockedTickConverter);
    mockedKernel.convertToEncodedSclk(sclk);
  }

  @Test
  public void testConvertToEncodedSclk() {
    SCLK sclk = new SCLK(10, 1, 1);

    CaptureAndAnswer<Interval> capture = CaptureAndAnswer.create(new Interval(1, 10000));

    expect(mockedTickConverter.convertToTicks(sclk)).andReturn(1000.0);
    expect(mockedPartitionTable.size()).andReturn(10);
    expect(mockedPartitionTable.get(eq(9), capture(capture.getCapture()))).andAnswer(capture);
    expect(mockedPartitionTable.getGauge(9)).andReturn(100000.0);

    replay(mockedTickConverter, mockedPartitionTable);

    assertEquals(100999.0, mockedKernel.convertToEncodedSclk(sclk), 0.0);
    verify(mockedTickConverter, mockedPartitionTable);
  }

  @Test
  public void testGetMaxEncodedSCLK() {
    assertEquals(validEncodedSclkRange.getEnd(), mockedKernel.getMaxEncodedSCLK(), 0.0);
  }

  @Test
  public void testConvertFromSclkToTDB() {

    SCLK value = new SCLK(2, 20, 30);

    expect(mockedPartitionTable.size()).andReturn(5);
    expect(mockedTickConverter.convertToTicks(value)).andReturn(1000.0);
    CaptureAndAnswer<Interval> capture = CaptureAndAnswer.create(new Interval(20, 4000));
    expect(mockedPartitionTable.get(eq(1), capture(capture.getCapture()))).andAnswer(capture);
    expect(mockedPartitionTable.getGauge(1)).andReturn(50.0);
    expect(mockedKernel.convertToTDB(1030.0)).andReturn(2121.0);

    replay(mockedKernel, mockedPartitionTable, mockedTickConverter);

    assertEquals(2121.0, mockedKernel.convertFromSclkToTDB(value), 0.0);

    verify(mockedKernel, mockedPartitionTable, mockedTickConverter);

  }

  @Test
  public void testConvertFromTDBToSclk() {
    SCLK buffer = new SCLK();

    expect(mockedKernel.convertToEncodedSclk(100.0)).andReturn(30.0);
    expect(mockedPartitionTable.indexLastLessThanOrEqualTo(30.0)).andReturn(1);
    CaptureAndAnswer<Interval> capture = CaptureAndAnswer.create(new Interval(10, 400));
    expect(mockedPartitionTable.get(eq(1), capture(capture.getCapture()))).andAnswer(capture);
    CaptureAndAnswer<SCLK> sclkCapture = CaptureAndAnswer.create(new SCLK(2, 50, 20));
    expect(mockedPartitionTable.getGauge(1)).andReturn(20.0);
    expect(mockedTickConverter.convertToSCLK(eq(20.0), capture(sclkCapture.getCapture())))
        .andAnswer(sclkCapture);

    replay(mockedKernel, mockedPartitionTable, mockedTickConverter);

    SCLK result = mockedKernel.convertFromTDBToSclk(100.0, buffer);
    assertSame(result, buffer);
    assertEquals(new SCLK(2, 50, 20), buffer);

    verify(mockedKernel, mockedPartitionTable, mockedTickConverter);

  }

}
