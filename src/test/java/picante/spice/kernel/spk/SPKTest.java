package picante.spice.kernel.spk;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import picante.spice.daf.DAF;
import picante.spice.daf.DAFFactory;
import picante.spice.daf.content.DAFBackedSPKContent;
import picante.spice.daf.content.SPKSegmentFactory;
import picante.spice.kernel.KernelType;

public class SPKTest {

  private static DAF daf;
  private SPK spk;
  private static DAF commentDAF;
  private SPK commentSPK;

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    daf = DAFFactory.createDAF(SPKTest.class.getResourceAsStream("combined.bsp"));
    commentDAF = DAFFactory.createDAF(SPKTest.class.getResourceAsStream("type1.bsp"));
  }

  @Before
  public void setUp() throws Exception {
    spk = new SPK("combined.bsp", new DAFBackedSPKContent(daf, SPKSegmentFactory.VALIDATING));
    commentSPK =
        new SPK("type1.bsp", new DAFBackedSPKContent(commentDAF, SPKSegmentFactory.VALIDATING));
  }

  @Test(expected = SPKInstantiationException.class)
  public void testIdWordException() throws Exception {
    new SPK("invalidID.bsp",
        new DAFBackedSPKContent(
            DAFFactory.createDAF(SPKTest.class.getResourceAsStream("invalidID.bsp")),
            SPKSegmentFactory.VALIDATING));
  }

  @Test
  public void testGetSegment() {
    assertEquals(2, spk.getSegment(0).getType());
    assertEquals(3, spk.getSegment(1).getType());
    assertEquals(2, spk.getSegment(2).getType());
    assertEquals(3, spk.getSegment(3).getType());
  }

  @Test
  public void testGetSize() {
    assertEquals(4, spk.getSize());
  }

  @Test
  public void testGetSegments() {
    List<SPKSegment> segments = new ArrayList<SPKSegment>();
    List<SPKSegment> result = spk.getSegments(segments);

    assertSame(result, segments);

    assertEquals(4, segments.size());
    assertEquals(2, segments.get(0).getType());
    assertEquals(3, segments.get(1).getType());
    assertEquals(2, segments.get(2).getType());
    assertEquals(3, segments.get(3).getType());
  }

  @Test
  public void testGetType() {
    assertEquals(KernelType.SPK, spk.getType());
  }

  @Test
  public void testGetName() {
    assertEquals("combined.bsp", spk.getName());
  }

  @Test
  public void testGetInternalName() {
    assertEquals(" < DAFCAT: SPK CONCATENATION >", spk.getInternalName());
  }

  @Test
  public void testGetComments() {
    assertEquals(0, spk.getComments().size());

    List<String> expected = new ArrayList<String>();

    expected.add("; type1.bsp LOG FILE");
    expected.add("");
    expected.add("; Created 2008-06-02/17:15:13.00.");
    expected.add(";");
    expected.add("; BEGIN SPKMERGE COMMANDS");
    expected.add("");
    expected.add("LEAPSECONDS_KERNEL   = /Users/turnefs1/Kernels/cassini/lsk/naif0008.tls");
    expected.add("");
    expected.add("SPK_KERNEL           = type1.bsp");
    expected.add("  SOURCE_SPK_KERNEL  = native.bsp");
    expected.add("    INCLUDE_COMMENTS = NO");
    expected.add("    BODIES           = -82");
    expected.add("    BEGIN_TIME       = 2007 APR 04 11:58:55.814");
    expected.add("    END_TIME         = 2007 APR 16 11:58:54.814");
    expected.add("");
    expected.add("; END SPKMERGE COMMANDS");

    assertEquals(expected, commentSPK.getComments());
  }

}
