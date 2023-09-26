package picante.spice.kernel.spk;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.reset;
import static org.junit.Assert.assertEquals;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.Lists;
import picante.data.list.Retrievable;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.mechanics.StateVector;

public class StateVectorDirectLookupRecordTableTest {

  private List<StateVector> svs = Lists.newArrayList();

  private StateVectorDirectLookupRecordTable table;

  private Retrievable<StateVector> mockList;

  private final static double INITIAL_EPOCH = 30.0;
  private final static double INTERVAL_LENGTH = 5.5;

  @Before
  public void setUp() throws Exception {
    svs.add(new StateVector(new UnwritableVectorIJK(0.0, 0.0, 0.0),
        new UnwritableVectorIJK(0.0, 0.0, 0.0)));
    svs.add(new StateVector(new UnwritableVectorIJK(1.0, 0.0, 0.0),
        new UnwritableVectorIJK(0.0, 0.0, 0.0)));
    svs.add(new StateVector(new UnwritableVectorIJK(2.0, 0.0, 0.0),
        new UnwritableVectorIJK(0.0, 0.0, 0.0)));
    svs.add(new StateVector(new UnwritableVectorIJK(3.0, 0.0, 0.0),
        new UnwritableVectorIJK(0.0, 0.0, 0.0)));
    svs.add(new StateVector(new UnwritableVectorIJK(4.0, 0.0, 0.0),
        new UnwritableVectorIJK(0.0, 0.0, 0.0)));
    svs.add(new StateVector(new UnwritableVectorIJK(5.0, 0.0, 0.0),
        new UnwritableVectorIJK(0.0, 0.0, 0.0)));
    svs.add(new StateVector(new UnwritableVectorIJK(6.0, 0.0, 0.0),
        new UnwritableVectorIJK(0.0, 0.0, 0.0)));
    svs.add(new StateVector(new UnwritableVectorIJK(7.0, 0.0, 0.0),
        new UnwritableVectorIJK(0.0, 0.0, 0.0)));
    svs.add(new StateVector(new UnwritableVectorIJK(8.0, 0.0, 0.0),
        new UnwritableVectorIJK(0.0, 0.0, 0.0)));
    svs.add(new StateVector(new UnwritableVectorIJK(9.0, 0.0, 0.0),
        new UnwritableVectorIJK(0.0, 0.0, 0.0)));

    mockList = createMock(Retrievable.class);
    configureMock();
    replay(mockList);
    table = new StateVectorDirectLookupRecordTable(INITIAL_EPOCH, INTERVAL_LENGTH, mockList);
  }

  private void configureMock() {
    expect(mockList.size()).andReturn(svs.size()).anyTimes();
    for (int i = 0; i < svs.size(); i++) {
      expect(mockList.get(i, new StateVector())).andReturn(svs.get(i)).anyTimes();
    }
  }

  @Test
  public void testGetTime() {
    reset(mockList);
    configureMock();
    replay(mockList);

    assertEquals(INITIAL_EPOCH + INTERVAL_LENGTH * 0, table.getGauge(0), 0.0);
    assertEquals(INITIAL_EPOCH + INTERVAL_LENGTH * 1, table.getGauge(1), 0.0);
    assertEquals(INITIAL_EPOCH + INTERVAL_LENGTH * 2, table.getGauge(2), 0.0);
    assertEquals(INITIAL_EPOCH + INTERVAL_LENGTH * 3, table.getGauge(3), 0.0);
    assertEquals(INITIAL_EPOCH + INTERVAL_LENGTH * 4, table.getGauge(4), 0.0);
    assertEquals(INITIAL_EPOCH + INTERVAL_LENGTH * 5, table.getGauge(5), 0.0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetTimeOutOfBounds1() {
    reset(mockList);
    configureMock();
    replay(mockList);

    assertEquals(INITIAL_EPOCH + INTERVAL_LENGTH * 10, table.getGauge(10), 0.0);

  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetTimeOutOfBounds2() {
    reset(mockList);
    configureMock();
    replay(mockList);

    assertEquals(INITIAL_EPOCH + INTERVAL_LENGTH * 6, table.getGauge(-1), 0.0);

  }

  @Test
  public void testGetRecord() {
    reset(mockList);
    configureMock();
    replay(mockList);

    for (int i = 0; i < svs.size(); i++) {
      assertEquals(svs.get(i), table.get(i, new StateVector()));
    }
  }

  @Test
  public void testGetLength() {

    reset(mockList);
    configureMock();
    replay(mockList);

    assertEquals(svs.size(), table.size());
  }

  @Test
  public void testGetIndexLastLessThan() {
    assertEquals(-1, table.indexLastLessThan(-100.0));
    assertEquals(-1, table.indexLastLessThan(30.0));
    assertEquals(0, table.indexLastLessThan(30.000000001));
    assertEquals(0, table.indexLastLessThan(35.5));
    assertEquals(1, table.indexLastLessThan(40.0));
    assertEquals(2, table.indexLastLessThan(42.0));
    assertEquals(4, table.indexLastLessThan(57.5));
    assertEquals(5, table.indexLastLessThan(57.500000001));
    assertEquals(9, table.indexLastLessThan(100.0));
  }


}
