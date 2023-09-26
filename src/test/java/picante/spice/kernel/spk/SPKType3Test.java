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

public class SPKType3Test {

  private static DAF daf;
  private static SPK spk;
  private SPKType3 segment;

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    daf = DAFFactory.createDAF(SPKType3Test.class.getResourceAsStream("type3.bsp"));

    spk = new SPK("type3.bsp", new DAFBackedSPKContent(daf, SPKSegmentFactory.VALIDATING));

  }

  @Before
  public void setUp() throws Exception {
    segment = (SPKType3) spk.getSegment(0);
  }

  @Test
  public void testSPKType3() {
    SPKType3 sample =
        new SPKType3("SEGMENTNAME", 10, 20, 30, 40, 50, 60, 70, new Retrievable<SPKType3Record>() {

          @Override
          public SPKType3Record get(@SuppressWarnings("unused") int index,
              @SuppressWarnings("unused") SPKType3Record buffer) {
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
    assertEquals(3, segment.getType());
  }

  @Test
  public void testGetPosition() {

    VectorIJK buffer = segment.getPosition(2.2960800000000E+08 + 10000.0, new VectorIJK());
    assertEquals(-271.62740505705574, buffer.getI(), 0.0);
    assertEquals(-106.05983303902637, buffer.getJ(), 0.0);
    assertEquals(30.30683119626158, buffer.getK(), 0.0);

  }

  @Test
  public void testGetState() {

    StateVector buffer = segment.getState(2.2960800000000E+08 + 10000.0, new StateVector());

    VectorIJK v;

    v = buffer.getPosition();
    assertEquals(-271.62740505705574, v.getI(), 0.0);
    assertEquals(-106.05983303902637, v.getJ(), 0.0);
    assertEquals(30.30683119626158, v.getK(), 0.0);

    v = buffer.getVelocity();
    assertEquals(0.0004173912280652296, v.getI(), 0.0);
    assertEquals(-0.0012876997284353752, v.getJ(), 0.0);
    assertEquals(0.00004728906346822818, v.getK(), 0.0);

  }

}
