package picante.spice.kernelpool.content;

import static org.junit.Assert.assertEquals;
import java.util.Arrays;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import picante.math.intervals.Interval;
import picante.spice.kernel.tk.sclk.SCLKInstantiationException;

public class SCLKPartitionTableTest {

  private SCLKPartitionTable tableWithRound;
  private SCLKPartitionTable table;
  private SCLKPartitionTable negativeTable;

  @Before
  public void setUp() throws Exception {
    tableWithRound = new SCLKPartitionTable(-100, Arrays.asList(0., 100.1, 0.),
        Arrays.asList(1000., 1000., 1000.6));
    table = new SCLKPartitionTable(-101, Arrays.asList(0., 1000.), Arrays.asList(10000., 11000.));
    negativeTable = new SCLKPartitionTable(-102, Arrays.asList(0., -100., 100.),
        Arrays.asList(1000., 1000., 2000.));
  }

  @Test(expected = SCLKInstantiationException.class)
  public void testSCLKPartitionTableIncompatibleStartsEndsException() throws Exception {
    new SCLKPartitionTable(-103, Arrays.asList(1.0, 0.0), Arrays.asList(1000.0));
  }

  @Test(expected = SCLKInstantiationException.class)
  public void testSCLKPartitionTableStartExceedsEndException() throws Exception {
    new SCLKPartitionTable(-104, Arrays.asList(0.0, 1000.0), Arrays.asList(100.0, 100.0));
  }

  @Test
  public void testGetTime() {

    assertEquals(0.0, tableWithRound.getGauge(0), 0.0);
    assertEquals(1000.0, tableWithRound.getGauge(1), 0.0);
    assertEquals(1900.0, tableWithRound.getGauge(2), 0.0);

    assertEquals(0.0, table.getGauge(0), 0.0);
    assertEquals(10000.0, table.getGauge(1), 0.0);

    assertEquals(0.0, negativeTable.getGauge(0), 0.0);
    assertEquals(1000.0, negativeTable.getGauge(1), 0.0);
    assertEquals(2100.0, negativeTable.getGauge(2), 0.0);

  }

  @Test
  public void testGetRecord() {
    checkIntervals(
        Arrays.asList(new Interval(0, 1000), new Interval(100, 1000), new Interval(0, 1001)),
        tableWithRound);
    checkIntervals(Arrays.asList(new Interval(0, 10000), new Interval(1000, 11000)), table);
    checkIntervals(
        Arrays.asList(new Interval(0, 1000), new Interval(-100, 1000), new Interval(100, 2000)),
        negativeTable);
  }

  @Test
  public void testGetLength() {
    assertEquals(3, tableWithRound.size());
    assertEquals(2, table.size());
    assertEquals(3, negativeTable.size());
  }

  private static void checkIntervals(List<Interval> intervals, SCLKPartitionTable table) {
    assertEquals(intervals.size(), table.size());

    Interval buffer = new Interval();

    for (int i = 0; i < intervals.size(); i++) {
      assertEquals(intervals.get(i), table.get(i, buffer));
    }
  }

}
