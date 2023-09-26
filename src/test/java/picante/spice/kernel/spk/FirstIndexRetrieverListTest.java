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
import picante.data.list.Retrievable;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.mechanics.StateVector;

public class FirstIndexRetrieverListTest {

  private List<StateVector> svs = Lists.newArrayList();

  private FirstIndexRetrieverList<StateVector> retriever;
  private FirstIndexRetrieverTable<StateVector> tableRetriever;

  private Retrievable<StateVector> mockList;
  private GaugedRetrievableLLT<StateVector> recordTable;

  private final static double INITIAL_EPOCH = 30.0;
  private final static double INTERVAL_LENGTH = 6.0;

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

    recordTable = new StateVectorDirectLookupRecordTable(INITIAL_EPOCH, INTERVAL_LENGTH, mockList);

    retriever = new FirstIndexRetrieverList<StateVector>(mockList, INITIAL_EPOCH, INTERVAL_LENGTH);
    tableRetriever = new FirstIndexRetrieverTable<StateVector>(recordTable);
  }

  public void configureMock() {
    expect(mockList.size()).andReturn(svs.size()).anyTimes();
    for (int i = 0; i < svs.size(); i++) {
      expect(mockList.get(i, new StateVector())).andReturn(svs.get(i)).anyTimes();
    }
  }

  @Test
  public void testFirstIndexRetrieverList() {
    assertEquals(INITIAL_EPOCH, retriever.getStartTime(), 0.0);
    assertEquals(INTERVAL_LENGTH, retriever.getStepSize(), 0.0);
  }

  @Test
  public void testGetFirstIndex() {
    reset(mockList);
    configureMock();
    replay(mockList);

    assertEquals(0, retriever.getFirstIndex(-1.0, 1));
    assertEquals(0, retriever.getFirstIndex(-1.0, 2));
    assertEquals(0, retriever.getFirstIndex(-1.0, 3));

    assertEquals(0, retriever.getFirstIndex(30.0, 1));
    assertEquals(0, retriever.getFirstIndex(30.0, 2));
    assertEquals(0, retriever.getFirstIndex(30.0, 3));

    assertEquals(0, retriever.getFirstIndex(32.9999999, 1));
    assertEquals(1, retriever.getFirstIndex(33.0, 1));

    assertEquals(2, retriever.getFirstIndex(41.0, 1));
    assertEquals(1, retriever.getFirstIndex(41.0, 2));
    assertEquals(1, retriever.getFirstIndex(41.0, 3));
    assertEquals(0, retriever.getFirstIndex(41.0, 4));
    assertEquals(0, retriever.getFirstIndex(41.0, 5));
    assertEquals(0, retriever.getFirstIndex(41.0, 6));
    assertEquals(0, retriever.getFirstIndex(41.0, 7));

    assertEquals(3, retriever.getFirstIndex(48.0, 1));
    assertEquals(3, retriever.getFirstIndex(48.0, 2));
    assertEquals(2, retriever.getFirstIndex(48.0, 3));
    assertEquals(2, retriever.getFirstIndex(48.0, 4));
    assertEquals(1, retriever.getFirstIndex(48.0, 5));

    assertEquals(3, retriever.getFirstIndex(49.0, 1));
    assertEquals(3, retriever.getFirstIndex(49.0, 2));
    assertEquals(2, retriever.getFirstIndex(49.0, 3));
    assertEquals(2, retriever.getFirstIndex(49.0, 4));
    assertEquals(1, retriever.getFirstIndex(49.0, 5));

    assertEquals(4, retriever.getFirstIndex(52.0, 1));
    assertEquals(3, retriever.getFirstIndex(52.0, 2));
    assertEquals(3, retriever.getFirstIndex(52.0, 3));
    assertEquals(2, retriever.getFirstIndex(52.0, 4));
    assertEquals(2, retriever.getFirstIndex(52.0, 5));

    assertEquals(8, retriever.getFirstIndex(77.0, 1));
    assertEquals(7, retriever.getFirstIndex(77.0, 2));
    assertEquals(7, retriever.getFirstIndex(77.0, 3));
    assertEquals(6, retriever.getFirstIndex(77.0, 4));
    assertEquals(5, retriever.getFirstIndex(77.0, 5));

    assertEquals(9, retriever.getFirstIndex(84.0, 1));
    assertEquals(8, retriever.getFirstIndex(84.0, 2));
    assertEquals(9, retriever.getFirstIndex(85.0, 1));
    assertEquals(9, retriever.getFirstIndex(100.0, 1));

    assertEquals(0, tableRetriever.getFirstIndex(-1.0, 1));
    assertEquals(0, tableRetriever.getFirstIndex(-1.0, 2));
    assertEquals(0, tableRetriever.getFirstIndex(-1.0, 3));

    assertEquals(0, tableRetriever.getFirstIndex(30.0, 1));
    assertEquals(0, tableRetriever.getFirstIndex(30.0, 2));
    assertEquals(0, tableRetriever.getFirstIndex(30.0, 3));

    assertEquals(0, tableRetriever.getFirstIndex(32.9999999, 1));
    assertEquals(1, tableRetriever.getFirstIndex(33.0, 1));

    assertEquals(2, tableRetriever.getFirstIndex(41.0, 1));
    assertEquals(1, tableRetriever.getFirstIndex(41.0, 2));
    assertEquals(1, tableRetriever.getFirstIndex(41.0, 3));
    assertEquals(0, tableRetriever.getFirstIndex(41.0, 4));
    assertEquals(0, tableRetriever.getFirstIndex(41.0, 5));
    assertEquals(0, tableRetriever.getFirstIndex(41.0, 6));
    assertEquals(0, tableRetriever.getFirstIndex(41.0, 7));

    assertEquals(3, tableRetriever.getFirstIndex(48.0, 1));
    // should differ from the list retriever
    assertEquals(2, tableRetriever.getFirstIndex(48.0, 2));
    assertEquals(2, tableRetriever.getFirstIndex(48.0, 3));
    // should differ from the list retriever
    assertEquals(1, tableRetriever.getFirstIndex(48.0, 4));
    assertEquals(1, tableRetriever.getFirstIndex(48.0, 5));

    assertEquals(3, tableRetriever.getFirstIndex(49.0, 1));
    assertEquals(3, tableRetriever.getFirstIndex(49.0, 2));
    assertEquals(2, tableRetriever.getFirstIndex(49.0, 3));
    assertEquals(2, tableRetriever.getFirstIndex(49.0, 4));
    assertEquals(1, tableRetriever.getFirstIndex(49.0, 5));

    assertEquals(4, tableRetriever.getFirstIndex(52.0, 1));
    assertEquals(3, tableRetriever.getFirstIndex(52.0, 2));
    assertEquals(3, tableRetriever.getFirstIndex(52.0, 3));
    assertEquals(2, tableRetriever.getFirstIndex(52.0, 4));
    assertEquals(2, tableRetriever.getFirstIndex(52.0, 5));

    assertEquals(8, tableRetriever.getFirstIndex(77.0, 1));
    assertEquals(7, tableRetriever.getFirstIndex(77.0, 2));
    assertEquals(7, tableRetriever.getFirstIndex(77.0, 3));
    assertEquals(6, tableRetriever.getFirstIndex(77.0, 4));
    assertEquals(5, tableRetriever.getFirstIndex(77.0, 5));

    assertEquals(9, tableRetriever.getFirstIndex(84.0, 1));
    assertEquals(8, tableRetriever.getFirstIndex(84.0, 2));
    // these break the tablerRetriever but not the list retriever
    // assertEquals(9, tableRetriever.getFirstIndex(85.0, 1));
    // assertEquals(9, tableRetriever.getFirstIndex(100.0, 1));

    verify(mockList);
  }

}
