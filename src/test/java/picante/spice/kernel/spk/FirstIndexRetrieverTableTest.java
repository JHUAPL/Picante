package picante.spice.kernel.spk;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.reset;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.Lists;
import picante.data.list.GaugedRetrievableLLT;
import picante.mechanics.StateVector;

public class FirstIndexRetrieverTableTest {

  private GaugedRetrievableLLT<StateVector> mockTable;

  private FirstIndexRetrieverTable<StateVector> retriever;

  private List<Double> times = Lists.newArrayList();

  private List<Double> testCalls1 = Lists.newArrayList();
  private List<Double> testCalls2 = Lists.newArrayList();
  private List<Double> testCalls3 = Lists.newArrayList();
  private List<Double> testCalls4 = Lists.newArrayList();
  private List<Double> testCalls5 = Lists.newArrayList();
  private List<Double> testCalls6 = Lists.newArrayList();
  private List<Double> testCalls7 = Lists.newArrayList();

  @Before
  public void setUp() throws Exception {
    mockTable = createMock(GaugedRetrievableLLT.class);
    retriever = new FirstIndexRetrieverTable<StateVector>(mockTable);


    testCalls1.add(-1.0);
    testCalls1.add(0.0);
    testCalls1.add(7.9);
    testCalls1.add(8.0);

    testCalls2.add(27.1);
    testCalls2.add(28.0);
    testCalls2.add(331212342.20109);

    testCalls3.add(8.1);
    testCalls3.add(8.5);
    testCalls3.add(8.6);
    testCalls3.add(9.0);

    testCalls4.add(10.2);
    testCalls4.add(12.3);

    testCalls5.add(25.0);
    testCalls5.add(27.0);

    testCalls6.add(14.1);
    testCalls6.add(15.9);

    testCalls7.add(16.0);
    testCalls7.add(18.0);

  }

  // the mock can be primed with a differing time
  private void configureMock(double time) {

    times.clear();

    times.add(8.0);
    times.add(9.0);
    times.add(10.1);
    times.add(12.3);
    times.add(14.0);
    times.add(18.0);
    times.add(20.0);
    times.add(23.0);
    times.add(27.0);

    int lastLessThan = -1;

    expect(mockTable.size()).andReturn(times.size()).anyTimes();

    for (int i = 0; i < times.size(); i++) {
      if (times.get(i) < time) {
        lastLessThan = i;
      }
      expect(mockTable.getGauge(i)).andReturn(times.get(i)).anyTimes();
    }

    expect(mockTable.indexLastLessThan(time)).andReturn(lastLessThan).anyTimes();
  }

  @Test
  /**
   * We want to perform a variety of checks, example, when the et is before the first index, when it
   * is equal to the first index, when it is after the last index, when it is equal to the last
   * index. When it is exactly between two indices.
   * 
   * And then we want to test a variety of different degrees.
   * 
   * <pre>
   *           et
   *            |
   *     8.0    9.0    10.1    12.3     14.0      18.0    20.0   23.0    27.0
   * ----|------|-------|-------|--------|---------|-------|------|-------|-recordTable->
   *    /|\                                                              /|\
   *     |                                                                |
   *   first                                                             last 
   *   index                                                             index
   * </pre>
   */
  public void testGetIndex() {

    // tests testCalls where the method must shift the first index from -1
    // to zero
    for (Double d : testCalls1) {
      reset(mockTable);
      configureMock(d);
      replay(mockTable);

      // ensures the mock is working properly
      assertEquals(-1, mockTable.indexLastLessThan(d));

      int start = retriever.getFirstIndex(d, 1);
      assertEquals(0, start);

      start = retriever.getFirstIndex(d, 2);
      assertEquals(0, start);

      start = retriever.getFirstIndex(d, 3);
      assertEquals(0, start);

      start = retriever.getFirstIndex(d, 4);
      assertEquals(0, start);

      start = retriever.getFirstIndex(d, 5);
      assertEquals(0, start);

      start = retriever.getFirstIndex(d, 6);
      assertEquals(0, start);

      verify(mockTable);
    }

    // does the opposite of the test 1 calls and looks at the other end.
    // Unfortunately, SPICE did not make this, so these tests will fail
    // for (Double d : testCalls2) {
    // reset(mockTable);
    // configureMock(d);
    // replay(mockTable);
    //
    // // ensures the mock is working properly
    // assertEquals(8, mockTable.getIndexLastLessThan(d));
    //
    // System.out.println(d);
    //
    // int start = segment.getIndex(d, mockTable, 1);
    // assertEquals(8, start);
    //
    // start = segment.getIndex(d, mockTable, 2);
    // assertEquals(7, start);
    //
    // start = segment.getIndex(d, mockTable, 4);
    // assertEquals(6, start);
    //
    // start = segment.getIndex(d, mockTable, 5);
    // assertEquals(5, start);
    //
    // start = segment.getIndex(d, mockTable, 6);
    // assertEquals(4, start);
    //
    // verify(mockTable);
    // }

    // tests testCalls that should set the first index to the first index
    for (Double d : testCalls3) {
      reset(mockTable);
      configureMock(d);
      replay(mockTable);

      // ensures the mock is working properly
      assertEquals(0, mockTable.indexLastLessThan(d));

      int start = retriever.getFirstIndex(d, 2);
      assertEquals(0, start);

      start = retriever.getFirstIndex(d, 3);
      assertEquals(0, start);

      start = retriever.getFirstIndex(d, 4);
      assertEquals(0, start);

      start = retriever.getFirstIndex(d, 5);
      assertEquals(0, start);

      start = retriever.getFirstIndex(d, 6);
      assertEquals(0, start);

      verify(mockTable);
    }

    // place d near the start epoch and expand the degree until it moves
    reset(mockTable);
    configureMock(testCalls4.get(0));
    replay(mockTable);

    double d4a = testCalls4.get(0);

    // ensures the mock is working properly
    assertEquals(2, mockTable.indexLastLessThan(d4a));

    int start4a = retriever.getFirstIndex(d4a, 1);
    assertEquals(2, start4a);

    start4a = retriever.getFirstIndex(d4a, 2);
    assertEquals(2, start4a);

    start4a = retriever.getFirstIndex(d4a, 3);
    assertEquals(1, start4a);

    start4a = retriever.getFirstIndex(d4a, 4);
    assertEquals(1, start4a);

    start4a = retriever.getFirstIndex(d4a, 5);
    assertEquals(0, start4a);

    start4a = retriever.getFirstIndex(d4a, 6);
    assertEquals(0, start4a);

    start4a = retriever.getFirstIndex(d4a, 7);
    assertEquals(0, start4a);

    start4a = retriever.getFirstIndex(d4a, 8);
    assertEquals(0, start4a);

    start4a = retriever.getFirstIndex(d4a, 9);
    assertEquals(0, start4a);

    // another assumption is that this will not happen, the
    // start4a = segment.getIndex(d4a, mockTable, 10);
    // assertEquals(0, start4a);
    //
    // start4a = segment.getIndex(d4a, mockTable, 11);
    // assertEquals(0, start4a);

    verify(mockTable);

    // place d near the start epoch and expand the degree until it moves
    reset(mockTable);
    configureMock(testCalls4.get(1));
    replay(mockTable);

    double d4b = testCalls4.get(1);

    // ensures the mock is working properly
    assertEquals(2, mockTable.indexLastLessThan(d4b));

    int start4b = retriever.getFirstIndex(d4b, 1);
    assertEquals(3, start4b);

    start4b = retriever.getFirstIndex(d4b, 2);
    assertEquals(2, start4b);

    start4b = retriever.getFirstIndex(d4b, 3);
    assertEquals(2, start4b);

    start4b = retriever.getFirstIndex(d4b, 4);
    assertEquals(1, start4b);

    start4b = retriever.getFirstIndex(d4b, 5);
    assertEquals(1, start4b);

    start4b = retriever.getFirstIndex(d4b, 6);
    assertEquals(0, start4b);

    start4b = retriever.getFirstIndex(d4b, 7);
    assertEquals(0, start4b);

    start4b = retriever.getFirstIndex(d4b, 8);
    assertEquals(0, start4b);

    start4b = retriever.getFirstIndex(d4b, 9);
    assertEquals(0, start4b);

    // another assumption is that this will not happen, the
    // start4a = segment.getIndex(d4a, mockTable, 10);
    // assertEquals(0, start4a);
    //
    // start4a = segment.getIndex(d4a, mockTable, 11);
    // assertEquals(0, start4a);

    verify(mockTable);

    // tests testCalls that should set the first index to the first index
    for (Double d : testCalls5) {
      reset(mockTable);
      configureMock(d);
      replay(mockTable);

      // ensures the mock is working properly
      assertEquals(7, mockTable.indexLastLessThan(d));

      int start = retriever.getFirstIndex(d, 1);
      assertEquals(8, start);

      start = retriever.getFirstIndex(d, 2);
      assertEquals(7, start);

      start = retriever.getFirstIndex(d, 3);
      assertEquals(6, start);

      start = retriever.getFirstIndex(d, 4);
      assertEquals(5, start);

      start = retriever.getFirstIndex(d, 5);
      assertEquals(4, start);

      start = retriever.getFirstIndex(d, 6);
      assertEquals(3, start);

      verify(mockTable);
    }

    // now lets jump to the middle of the table and see what happens
    for (Double d : testCalls6) {
      reset(mockTable);
      configureMock(d);
      replay(mockTable);

      // ensures the mock is working properly
      assertEquals(4, mockTable.indexLastLessThan(d));

      int start = retriever.getFirstIndex(d, 1);
      assertEquals(4, start);

      start = retriever.getFirstIndex(d, 2);
      assertEquals(4, start);

      start = retriever.getFirstIndex(d, 3);
      assertEquals(3, start);

      start = retriever.getFirstIndex(d, 4);
      assertEquals(3, start);

      start = retriever.getFirstIndex(d, 5);
      assertEquals(2, start);

      verify(mockTable);
    }

    // now lets jump to the middle of the table and see what happens
    for (Double d : testCalls7) {
      reset(mockTable);
      configureMock(d);
      replay(mockTable);

      // ensures the mock is working properly
      assertEquals(4, mockTable.indexLastLessThan(d));

      int start = retriever.getFirstIndex(d, 1);
      assertEquals(5, start);

      start = retriever.getFirstIndex(d, 2);
      assertEquals(4, start);

      start = retriever.getFirstIndex(d, 3);
      assertEquals(4, start);

      start = retriever.getFirstIndex(d, 4);
      assertEquals(3, start);

      start = retriever.getFirstIndex(d, 5);
      assertEquals(3, start);

      verify(mockTable);
    }

  }

}
