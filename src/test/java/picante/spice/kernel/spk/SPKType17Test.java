package picante.spice.kernel.spk;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static picante.junit.AssertTools.assertComponentRelativeEquality;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;
import picante.spice.daf.DAF;
import picante.spice.daf.DAFFactory;
import picante.spice.daf.content.DAFBackedSPKContent;
import picante.spice.daf.content.SPKSegmentFactory;

public class SPKType17Test {

  private static final double TOLERANCE = 1e-14;

  private static DAF daf;
  private static SPK spk;
  private SPKType17 segment;

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    daf = DAFFactory.createDAF(SPKType17Test.class.getResourceAsStream("type17.bsp"));
    spk = new SPK("type17.bsp", new DAFBackedSPKContent(daf, SPKSegmentFactory.VALIDATING));
  }

  @Before
  public void setUp() throws Exception {
    segment = (SPKType17) spk.getSegment(0);
  }

  @Test
  public void testGetType() {
    assertEquals(17, segment.getType());
  }

  @Test
  public void testGetPosition() {

    VectorIJK buffer = new VectorIJK();
    VectorIJK result = segment.getPosition(12345678.0, buffer);

    assertSame(result, buffer);
    assertComponentRelativeEquality(
        new VectorIJK(-2092.0783461542701, 133219.68262158972, -9636.6807155293664), result,
        TOLERANCE);

  }

  @Test
  public void testGetState() {

    StateVector buffer = new StateVector();
    StateVector result = segment.getState(12345678.0, buffer);

    assertSame(result, buffer);
    assertComponentRelativeEquality(new StateVector(-2092.0783461542701, 133219.68262158972,
        -9636.6807155293664, -16.829479334655790, -0.15865788059351996, 1.4595677116985091), result,
        TOLERANCE);

  }

}
