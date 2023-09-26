package picante.spice.kernel.spk;

import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import picante.data.list.Retrievable;
import picante.math.intervals.Interval;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;
import picante.spice.daf.DAF;
import picante.spice.daf.DAFFactory;
import picante.spice.daf.content.DAFBackedSPKContent;
import picante.spice.daf.content.SPKSegmentFactory;

public class SPKType2Test {

  private static DAF daf;
  private static SPK spk;
  private SPKType2 segment;

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {

    daf = DAFFactory.createDAF(SPKType2Test.class.getResourceAsStream("type2.bsp"));

    spk = new SPK("type2.bsp", new DAFBackedSPKContent(daf, SPKSegmentFactory.VALIDATING));

  }

  @Before
  public void setUp() throws Exception {
    segment = (SPKType2) spk.getSegment(0);
  }

  @Test
  public void testSPKType2() {
    SPKType2 sample =
        new SPKType2("SEGMENTNAME", 10, 20, 30, 40, 50, 60, 70, new Retrievable<SPKType2Record>() {

          @Override
          public SPKType2Record get(@SuppressWarnings("unused") int index,
              @SuppressWarnings("unused") SPKType2Record buffer) {
            throw new UnsupportedOperationException();
          }

          @Override
          public int size() {
            throw new UnsupportedOperationException();
          }
        });

    assertEquals("SEGMENTNAME", sample.getName());
    assertEquals(10, sample.getTargetID());
    assertEquals(20, sample.getObserverID());
    assertEquals(30, sample.getFrameID());
    assertEquals(40, sample.getCoverage().getBoundingInterval(new Interval()).getBegin(), 0.0);
    assertEquals(50, sample.getCoverage().getBoundingInterval(new Interval()).getEnd(), 0.0);
    assertEquals(60, sample.getInitialEpoch(), 0.0);
    assertEquals(70, sample.getIntervalLength(), 0.0);
  }

  @Test
  public void testGetType() {
    assertEquals(2, segment.getType());
  }

  @Test
  public void testGetPosition() {

    VectorIJK buffer = segment.getPosition(2.2960800000000E+08 - 2000.0, new VectorIJK());
    assertEquals(-2736.775696772357, buffer.getI(), 0.0);
    assertEquals(3272.9527121203737, buffer.getJ(), 0.0);
    assertEquals(1704.4661091804126, buffer.getK(), 0.0);

  }

  @Test
  public void testGetState() {

    StateVector buffer = segment.getState(2.2960800000000E+08 - 2000.0, new StateVector());

    VectorIJK v;
    v = buffer.getPosition();
    assertEquals(-2736.775696772357, v.getI(), 0.0);
    assertEquals(3272.9527121203737, v.getJ(), 0.0);
    assertEquals(1704.4661091804126, v.getK(), 0.0);

    v = buffer.getVelocity();
    assertEquals(-0.009583873270934148, v.getI(), 0.0);
    assertEquals(-0.0070718052308111155, v.getJ(), 0.0);
    assertEquals(-0.0040839630765958325, v.getK(), 0.0);

  }

}
