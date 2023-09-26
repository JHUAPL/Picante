package picante.spice.kernel.utilities;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;
import picante.data.list.Retrievable;

public class DirectRecordIndexComputerTest {

  private DirectRecordIndexComputer computer;
  private static final double initialEpoch = 1234567890123456.0;
  private static final double intervalLength = 3600.0;
  private static final int listLength = 50;

  @Before
  public void setUp() throws Exception {
    Retrievable<?> list = createMock(Retrievable.class);
    expect(list.size()).andReturn(listLength).anyTimes();
    replay(list);
    computer = new DirectRecordIndexComputer(initialEpoch, intervalLength, list);
  }

  @Test
  public void testNegativeIndex() {
    assertTrue(computer.computeRecordIndexForTime(initialEpoch - (intervalLength + 1)) < 0);
  }

  @Test
  public void testZeroIndexForCloseInitialEpochTimes() {
    assertEquals(0, computer.computeRecordIndexForTime(initialEpoch - intervalLength / 2.0));
  }

  @Test
  public void testTransitionIndices() {
    for (int i = 0; i < listLength; i++) {
      assertEquals(i, computer.computeRecordIndexForTime(initialEpoch + i * intervalLength));
    }
  }

  @Test
  public void testGetInitialEpoch() {
    assertEquals(initialEpoch, computer.getInitialEpoch(), 0.0);
  }

  @Test
  public void testGetIntervalLength() {
    assertEquals(intervalLength, computer.getIntervalLength(), 0.0);
  }

  @Test
  public void testGetRecordListLength() {
    assertEquals(listLength, computer.getRecordListLength());
  }

  @Test
  public void testIntermediateTimes() {
    for (int i = 0; i < listLength; i++) {
      assertEquals(i, computer
          .computeRecordIndexForTime(initialEpoch + i * (intervalLength) + intervalLength / 2.0));
    }
  }

  /*
   * This was broken in a previous version before this class existed, because of an absent set of
   * parentheses.
   */
  @Test
  public void testIntegerOverflow() {
    Retrievable<?> list = createMock(Retrievable.class);
    expect(list.size()).andReturn(50).anyTimes();
    replay(list);
    DirectRecordIndexComputer computer = new DirectRecordIndexComputer(0, 1e16, list);

    assertEquals(49, computer.computeRecordIndexForTime(1e16 * 50 + 1000));
  }

}
