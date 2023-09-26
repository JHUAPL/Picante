package picante.spice.kernel.spk;

import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import picante.data.list.GaugedRetrievableLLT;
import picante.math.intervals.Interval;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;
import picante.spice.daf.DAF;
import picante.spice.daf.DAFFactory;
import picante.spice.daf.content.DAFBackedSPKContent;
import picante.spice.daf.content.SPKSegmentFactory;

public class SPKType1Test {

  private static DAF daf;
  private static SPK spk;
  private SPKType1 segment;

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {

    daf = DAFFactory.createDAF(SPKType1Test.class.getResourceAsStream("type1.bsp"));

    spk = new SPK("type1.bsp", new DAFBackedSPKContent(daf, SPKSegmentFactory.VALIDATING));

  }

  @Before
  public void setUp() throws Exception {
    segment = (SPKType1) spk.getSegment(0);
  }

  @Test
  public void testSPKType1() {
    SPKType1 sample =
        new SPKType1("SEGMENTNAME", 10, 20, 30, 40, 50, new GaugedRetrievableLLT<SPKType1Record>() {

          @Override
          public SPKType1Record get(@SuppressWarnings("unused") int index,
              @SuppressWarnings("unused") SPKType1Record buffer) {
            throw new UnsupportedOperationException();
          }

          @Override
          public int size() {
            throw new UnsupportedOperationException();
          }

          @Override
          public double getGauge(@SuppressWarnings("unused") int index) {
            throw new UnsupportedOperationException();
          }

          @Override
          public int indexLastLessThan(@SuppressWarnings("unused") double time) {
            throw new UnsupportedOperationException();
          }
        });

    assertEquals("SEGMENTNAME", sample.getName());
    assertEquals(10, sample.getTargetID());
    assertEquals(20, sample.getObserverID());
    assertEquals(30, sample.getFrameID());
    assertEquals(40.0, sample.getCoverage().getBoundingInterval(new Interval()).getBegin(), 0.0);
    assertEquals(50.0, sample.getCoverage().getBoundingInterval(new Interval()).getEnd(), 0.0);
  }

  @Test
  public void testGetType() {
    assertEquals(1, segment.getType());
  }

  @Test
  public void testGetPosition() {

    double[] value = new double[1];
    daf.getSegment(0).get(570415, value, 0, 1);
    double et = value[0] + 50.0;
    VectorIJK vector = new VectorIJK();

    segment.getPosition(et, vector);

    assertEquals(445673.6497338726, vector.getI(), 0.0);
    assertEquals(1060277.962575679, vector.getJ(), 0.0);
    assertEquals(1184686.2828979813, vector.getK(), 0.0);

  }

  @Test
  public void testGetState() {

    double[] value = new double[1];
    daf.getSegment(0).get(570415, value, 0, 1);
    double et = value[0] + 5000.0;
    StateVector state = new StateVector();

    segment.getState(et, state);

    VectorIJK v;
    v = state.getPosition();
    assertEquals(427453.0712460731, v.getI(), 0.0);
    assertEquals(1056587.197371757, v.getJ(), 0.0);
    assertEquals(1179846.2681781596, v.getK(), 0.0);

    v = state.getVelocity();
    assertEquals(-3.690082654295314, v.getI(), 0.0);
    assertEquals(-0.767961629390993, v.getJ(), 0.0);
    assertEquals(-1.0027501147026363, v.getK(), 0.0);

  }

}
