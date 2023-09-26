package picante.spice.kernel.spk;

import static org.easymock.EasyMock.createMock;
import static org.junit.Assert.assertEquals;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertComponentRelativeEquality;
import java.io.InputStream;
import java.util.List;
import java.util.Scanner;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import com.google.common.collect.Lists;
import picante.data.list.GaugedRetrievableLLT;
import picante.math.intervals.Interval;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;
import picante.spice.daf.DAF;
import picante.spice.daf.DAFFactory;
import picante.spice.daf.content.DAFBackedSPKContent;
import picante.spice.daf.content.SPKSegmentFactory;

public class SPKType13Test {

  private static DAF daf5d;

  private final double TOL = 1E-14;

  private static SPK spk5d;

  private SPKType13 segment;
  private SPKType13 realSegment5d;

  private GaugedRetrievableLLT<StateVector> mockTable;

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    daf5d = DAFFactory.createDAF(SPKType13Test.class.getResourceAsStream("type13_5d.bsp"));

    spk5d = new SPK("type13_5d.bsp", new DAFBackedSPKContent(daf5d, SPKSegmentFactory.VALIDATING));

  }

  @Before
  public void setUp() throws Exception {
    // constructs a fake segment
    segment = new SPKType13("SEGMENTNAME", 10, 20, 30, 40.0, 50.0, mockTable, 2);
    // constructs a segment from a real spk file
    realSegment5d = (SPKType13) spk5d.getSegment(0);

    mockTable = createMock(GaugedRetrievableLLT.class);

  }

  @Test
  public void testSPKType13() {
    assertEquals("SEGMENTNAME", segment.getName());
    assertEquals(10, segment.getTargetID());
    assertEquals(20, segment.getObserverID());
    assertEquals(30, segment.getFrameID());
    assertEquals(40.0, segment.getCoverage().getBoundingInterval(new Interval()).getBegin(), 0.0);
    assertEquals(50.0, segment.getCoverage().getBoundingInterval(new Interval()).getEnd(), 0.0);
    assertEquals(3, segment.getDegree());
    assertEquals(5, realSegment5d.getDegree());
  }

  @Test
  public void testGetType() {
    assertEquals(13, segment.getType());
    assertEquals(13, realSegment5d.getType());
  }

  @Test
  public void testGetPosition() {

    List<StateVector> spiceStateVectors =
        readSpiceTest(SPKType13Test.class.getResourceAsStream("type13_5d_spice.txt"));

    int count = 0;
    for (double et = 0.0; et < 29.0; et = et + 0.1) {

      VectorIJK buffer = realSegment5d.getPosition(et, new VectorIJK());

      assertComponentEquals(spiceStateVectors.get(count).getPosition(), buffer, TOL);

      count++;
    }

  }

  @Test
  public void testGetState() {

    List<StateVector> spiceStateVectors =
        readSpiceTest(SPKType13Test.class.getResourceAsStream("type13_5d_spice.txt"));

    int count = 0;
    for (double et = 0.0; et < 29.0; et = et + 0.1) {

      StateVector buffer = realSegment5d.getState(et, new StateVector());

      assertComponentRelativeEquality(spiceStateVectors.get(count).getPosition(),
          buffer.getPosition(), TOL);

      assertComponentRelativeEquality(spiceStateVectors.get(count).getVelocity(),
          buffer.getVelocity(), TOL);

      count++;
    }
  }

  private List<StateVector> readSpiceTest(InputStream file) {

    Scanner scan = null;

    scan = new Scanner(file);

    List<StateVector> svs = Lists.newArrayList();

    while (scan.hasNextLine()) {

      String[] s = scan.nextLine().split(",");
      svs.add(new StateVector(
          new UnwritableVectorIJK(Double.parseDouble(s[1].trim()), Double.parseDouble(s[2].trim()),
              Double.parseDouble(s[3].trim())),
          new UnwritableVectorIJK(Double.parseDouble(s[4].trim()), Double.parseDouble(s[5].trim()),
              Double.parseDouble(s[6].trim()))));
    }

    scan.close();

    return svs;

  }

}
