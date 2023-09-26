package picante.spice.kernel.spk;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.reset;
import static org.junit.Assert.assertEquals;
import static picante.junit.AssertTools.assertComponentEquals;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.Lists;
import picante.data.list.GaugedRetrievableLLT;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;

public class AbstractSPKTypeTest {
  ;
  private ConcreteSPKType segment;

  private GaugedRetrievableLLT<StateVector> mockTable;

  private List<Double> times = Lists.newArrayList();
  private List<StateVector> sv = Lists.newArrayList();

  private List<Double> testCalls = Lists.newArrayList();

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
    segment = new ConcreteSPKType("sample", 10, 11, 11, 8.0, 30.0, mockTable, 3);

    testCalls1.add(-1.0);
    testCalls1.add(0.0);
    testCalls1.add(7.9);
    testCalls1.add(8.0);

    // testCalls2.add(27.1);
    // testCalls2.add(28.0);
    // testCalls2.add(331212342.20109);

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

    testCalls.addAll(testCalls1);
    testCalls.addAll(testCalls2);
    testCalls.addAll(testCalls3);
    testCalls.addAll(testCalls4);
    testCalls.addAll(testCalls5);
    testCalls.addAll(testCalls6);
    testCalls.addAll(testCalls7);

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

    sv.add(new StateVector(new UnwritableVectorIJK(0.0, 0.0, 0.0),
        new UnwritableVectorIJK(0.0, 0.0, 0.0)));
    sv.add(new StateVector(new UnwritableVectorIJK(0.1, 0.0, 0.0),
        new UnwritableVectorIJK(0.0, 0.0, 0.0)));
    sv.add(new StateVector(new UnwritableVectorIJK(0.2, 0.0, 0.0),
        new UnwritableVectorIJK(0.0, 0.0, 0.0)));
    sv.add(new StateVector(new UnwritableVectorIJK(0.3, 0.0, 0.0),
        new UnwritableVectorIJK(0.0, 0.0, 0.0)));
    sv.add(new StateVector(new UnwritableVectorIJK(0.4, 0.0, 0.0),
        new UnwritableVectorIJK(0.0, 0.0, 0.0)));
    sv.add(new StateVector(new UnwritableVectorIJK(0.5, 0.0, 0.0),
        new UnwritableVectorIJK(0.0, 0.0, 0.0)));
    sv.add(new StateVector(new UnwritableVectorIJK(0.6, 0.0, 0.0),
        new UnwritableVectorIJK(0.0, 0.0, 0.0)));
    sv.add(new StateVector(new UnwritableVectorIJK(0.7, 0.0, 0.0),
        new UnwritableVectorIJK(0.0, 0.0, 0.0)));
    sv.add(new StateVector(new UnwritableVectorIJK(0.8, 0.0, 0.0),
        new UnwritableVectorIJK(0.0, 0.0, 0.0)));

    int lastLessThan = -1;

    expect(mockTable.size()).andReturn(times.size()).anyTimes();

    for (int i = 0; i < times.size(); i++) {
      if (times.get(i) < time) {
        lastLessThan = i;
      }

      expect(mockTable.getGauge(i)).andReturn(times.get(i)).anyTimes();
      expect(mockTable.get(i, new StateVector())).andReturn(sv.get(i)).anyTimes();

    }

    expect(mockTable.indexLastLessThan(time)).andReturn(lastLessThan).anyTimes();
  }

  @Test
  public void testGetStateVectorRecord() {

    for (double d : testCalls) {

      // reset the bufferSV so that the mock
      for (int i = 0; i < segment.recordSize; i++) {
        segment.bufferSV[i] = new StateVector();
      }

      reset(mockTable);
      configureMock(d);
      replay(mockTable);

      int firstIndex = segment.firstIndexRetriever.getFirstIndex(d, segment.recordSize);

      StateVector[] buffer = segment.getStateVectorRecord(firstIndex);

      for (int i = 0; i < segment.recordSize; i++) {

        assertEquals(sv.get(firstIndex + i), buffer[i]);
        assertComponentEquals(sv.get(firstIndex + i).getPosition(), buffer[i].getPosition(), 1E-15);
        assertComponentEquals(sv.get(firstIndex + i).getVelocity(), buffer[i].getVelocity(), 1E-15);

        assertEquals(sv.get(firstIndex + i), segment.bufferSV[i]);
        assertComponentEquals(sv.get(firstIndex + i).getPosition(),
            segment.bufferSV[i].getPosition(), 1E-15);
        assertComponentEquals(sv.get(firstIndex + i).getVelocity(),
            segment.bufferSV[i].getVelocity(), 1E-15);

      }
    }
  }

  @Test
  public void testFillTimeBuffer() {
    for (double d : testCalls) {
      reset(mockTable);
      configureMock(d);
      replay(mockTable);

      int firstIndex = segment.firstIndexRetriever.getFirstIndex(d, segment.recordSize);
      segment.fillTimeBuffer(firstIndex);
      for (int i = 0; i < segment.recordSize; i++) {
        assertEquals(times.get(firstIndex + i), segment.bufferTime[i], 1E-15);
      }
    }
  }

  @Test
  public void testFillStateVectorBuffers() {
    for (double d : testCalls) {

      // reset the bufferSV so that the mock
      for (int i = 0; i < segment.recordSize; i++) {
        segment.bufferSV[i] = new StateVector();
      }

      reset(mockTable);
      configureMock(d);
      replay(mockTable);

      int firstIndex = segment.firstIndexRetriever.getFirstIndex(d, segment.recordSize);

      StateVector[] buffer = segment.getStateVectorRecord(firstIndex);

      segment.fillStateVectorBuffers(buffer);

      for (int i = 0; i < segment.recordSize; i++) {
        assertEquals(buffer[i].getPosition().getI(), segment.bufferX[i], 1E-15);
        assertEquals(buffer[i].getPosition().getJ(), segment.bufferY[i], 1E-15);
        assertEquals(buffer[i].getPosition().getK(), segment.bufferZ[i], 1E-15);
        assertEquals(buffer[i].getVelocity().getI(), segment.bufferdX[i], 1E-15);
        assertEquals(buffer[i].getVelocity().getJ(), segment.bufferdY[i], 1E-15);
        assertEquals(buffer[i].getVelocity().getK(), segment.bufferdZ[i], 1E-15);

        assertEquals(sv.get(firstIndex + i).getPosition().getI(), segment.bufferX[i], 1E-15);
        assertEquals(sv.get(firstIndex + i).getPosition().getJ(), segment.bufferY[i], 1E-15);
        assertEquals(sv.get(firstIndex + i).getPosition().getK(), segment.bufferZ[i], 1E-15);
        assertEquals(sv.get(firstIndex + i).getVelocity().getI(), segment.bufferdX[i], 1E-15);
        assertEquals(sv.get(firstIndex + i).getVelocity().getJ(), segment.bufferdY[i], 1E-15);
        assertEquals(sv.get(firstIndex + i).getVelocity().getK(), segment.bufferdZ[i], 1E-15);
      }
    }
  }

  @Test
  public void testPrepareBuffers() {
    for (double d : testCalls) {

      // reset the bufferSV so that the mock
      for (int i = 0; i < segment.recordSize; i++) {
        segment.bufferSV[i] = new StateVector();
      }

      reset(mockTable);
      configureMock(d);
      replay(mockTable);

      int firstIndex = segment.firstIndexRetriever.getFirstIndex(d, segment.recordSize);
      segment.prepareBuffers(d);

      for (int i = 0; i < segment.recordSize; i++) {

        assertEquals(times.get(firstIndex + i), segment.bufferTime[i], 1E-15);

        assertEquals(sv.get(firstIndex + i).getPosition().getI(), segment.bufferX[i], 1E-15);
        assertEquals(sv.get(firstIndex + i).getPosition().getJ(), segment.bufferY[i], 1E-15);
        assertEquals(sv.get(firstIndex + i).getPosition().getK(), segment.bufferZ[i], 1E-15);
        assertEquals(sv.get(firstIndex + i).getVelocity().getI(), segment.bufferdX[i], 1E-15);
        assertEquals(sv.get(firstIndex + i).getVelocity().getJ(), segment.bufferdY[i], 1E-15);
        assertEquals(sv.get(firstIndex + i).getVelocity().getK(), segment.bufferdZ[i], 1E-15);
      }
    }
  }

  class ConcreteSPKType extends AbstractSPKType {

    public ConcreteSPKType(String name, int targetID, int observerID, int frameID, double startET,
        double finalET, GaugedRetrievableLLT<StateVector> table, int degree) {
      super(name, targetID, observerID, frameID, startET, finalET, table,
          new FirstIndexRetrieverTable<StateVector>(table), degree);
    }

    @Override
    VectorIJK prepareUnivariatePosition(@SuppressWarnings("unused") double time, VectorIJK buffer) {
      return buffer;
    }

    @Override
    StateVector prepareUnivariateState(@SuppressWarnings("unused") double time,
        StateVector buffer) {
      return buffer;
    }

    @Override
    public int getType() {
      return 0;
    }
  }
}
