package picante.spice.kernel.spk;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import org.easymock.EasyMock;
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

public class SPKType5Test {

  private static DAF daf;
  private static SPK spk;
  private SPKType5 segment;

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    daf = DAFFactory.createDAF(SPKType5Test.class.getResourceAsStream("type5.bsp"));
    spk = new SPK("type5.bsp", new DAFBackedSPKContent(daf, SPKSegmentFactory.VALIDATING));
  }

  @Before
  public void setUp() throws Exception {
    segment = (SPKType5) spk.getSegment(0);
  }


  @Test
  public void testMetaData() {

    GaugedRetrievableLLT<StateVector> recordList = EasyMock.createMock(GaugedRetrievableLLT.class);
    SPKType5 sample = new SPKType5("SEGMENTNAME", 10, 20, 30, 40, 50, recordList, 10);

    assertEquals("SEGMENTNAME", sample.getName());
    assertEquals(10, sample.getTargetID());
    assertEquals(20, sample.getObserverID());
    assertEquals(30, sample.getFrameID());
    assertEquals(40.0, sample.getCoverage().getBoundingInterval(new Interval()).getBegin(), 0.0);
    assertEquals(50.0, sample.getCoverage().getBoundingInterval(new Interval()).getEnd(), 0.0);

  }

  @Test
  public void testGetType() {
    assertEquals(5, segment.getType());
  }

  @Test
  public void testGetPosition() {

    double et = 157809664.184e0;
    VectorIJK buffer = new VectorIJK();
    VectorIJK result = segment.getPosition(et, buffer);
    assertSame(buffer, result);
    assertEquals(new VectorIJK(-426840288.42963773, -6983869059.6378145, -1182661940.0834372),
        result);

    et = 1.5780975959014E+08;
    result = segment.getPosition(et, buffer);
    assertSame(buffer, result);
    assertEquals(new VectorIJK(-426839912.06490028, -6983869075.1549330, -1182661873.0852516),
        result);

    et = et + 40000.0;
    result = segment.getPosition(et, buffer);
    assertSame(buffer, result);
    assertEquals(new VectorIJK(-426682117.17208743, -6983875578.7831049, -1182633783.0580902),
        result);

  }

  @Test
  public void testGetState() {

    double et = 157809664.184e0;
    StateVector buffer = new StateVector();
    StateVector result = segment.getState(et, buffer);
    assertSame(buffer, result);
    assertEquals(new StateVector(-426840288.42963773, -6983869059.6378145, -1182661940.0834372,
        3.9448691306989945, -0.16264289815530858, 0.70224184051269500), result);

    et = 1.5780975959014E+08;
    result = segment.getState(et, buffer);
    assertSame(buffer, result);
    assertEquals(new StateVector(-426839912.06490028, -6983869075.1549330, -1182661873.0852516,
        3.9448691458221421, -0.16264265033329145, 0.70224188248673880), result);

    et = et + 40000.0;
    result = segment.getState(et, buffer);
    assertSame(buffer, result);
    assertEquals(new StateVector(-426682117.17208743, -6983875578.7831049, -1182633783.0580902,
        3.9448754981498282, -0.16253876220747715, 0.70225947359099528), result);

  }

}
