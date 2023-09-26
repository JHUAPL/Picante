package picante.spice.daf.content;

import static org.junit.Assert.assertEquals;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import picante.spice.daf.DAF;
import picante.spice.daf.DAFFactory;
import picante.spice.kernel.spk.SPKInstantiationException;
import picante.spice.kernel.spk.SPKSegment;
import picante.spice.kernel.spk.SPKType12Test;
import picante.spice.kernel.spk.SPKType13Test;
import picante.spice.kernel.spk.SPKType1Test;
import picante.spice.kernel.spk.SPKType2Test;
import picante.spice.kernel.spk.SPKType3Test;
import picante.spice.kernel.spk.SPKType8Test;
import picante.spice.kernel.spk.SPKType9Test;

public class SPKSegmentFactoryTest {

  private static DAF invalid;
  private static DAF type1;
  private static DAF type2;
  private static DAF type3;
  private static DAF type8;
  private static DAF type9;
  private static DAF type12;
  private static DAF type13;


  private List<SPKSegment> list;

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {

    invalid = DAFFactory.createDAF(SPKSegmentFactoryTest.class.getResourceAsStream("invalid.bsp"));

    type1 = DAFFactory.createDAF(SPKType1Test.class.getResourceAsStream("type1.bsp"));

    type2 = DAFFactory.createDAF(SPKType2Test.class.getResourceAsStream("type2.bsp"));

    type3 = DAFFactory.createDAF(SPKType3Test.class.getResourceAsStream("type3.bsp"));

    type8 = DAFFactory.createDAF(SPKType8Test.class.getResourceAsStream("type8_5d.bsp"));

    type9 = DAFFactory.createDAF(SPKType9Test.class.getResourceAsStream("type9_5d.bsp"));

    type12 = DAFFactory.createDAF(SPKType12Test.class.getResourceAsStream("type12_5d.bsp"));

    type13 = DAFFactory.createDAF(SPKType13Test.class.getResourceAsStream("type13_5d.bsp"));

  }

  @Before
  public void setUp() throws Exception {
    list = new ArrayList<SPKSegment>();
  }

  @Test(expected = SPKInstantiationException.class)
  public void testValidatingFactoryException() throws Exception {
    SPKSegmentFactory.VALIDATING.createAndAdd(invalid.getSegment(0), list);
  }

  @Test
  public void testValidatingFactory() throws Exception {
    SPKSegmentFactory.VALIDATING.createAndAdd(type1.getSegment(0), list);
    SPKSegmentFactory.VALIDATING.createAndAdd(type2.getSegment(0), list);
    SPKSegmentFactory.VALIDATING.createAndAdd(type3.getSegment(0), list);
    SPKSegmentFactory.VALIDATING.createAndAdd(type8.getSegment(0), list);
    SPKSegmentFactory.VALIDATING.createAndAdd(type9.getSegment(0), list);
    SPKSegmentFactory.VALIDATING.createAndAdd(type12.getSegment(0), list);
    SPKSegmentFactory.VALIDATING.createAndAdd(type13.getSegment(0), list);

    assertEquals(7, list.size());
    assertEquals(1, list.get(0).getType());
    assertEquals(2, list.get(1).getType());
    assertEquals(3, list.get(2).getType());
    assertEquals(8, list.get(3).getType());
    assertEquals(9, list.get(4).getType());
    assertEquals(12, list.get(5).getType());
    assertEquals(13, list.get(6).getType());
  }

  @Test
  public void testDisregardingFactory() throws Exception {
    SPKSegmentFactory.DISREGARDING.createAndAdd(invalid.getSegment(0), list);
    SPKSegmentFactory.DISREGARDING.createAndAdd(type1.getSegment(0), list);
    SPKSegmentFactory.DISREGARDING.createAndAdd(type2.getSegment(0), list);
    SPKSegmentFactory.DISREGARDING.createAndAdd(type3.getSegment(0), list);
    SPKSegmentFactory.DISREGARDING.createAndAdd(type8.getSegment(0), list);
    SPKSegmentFactory.DISREGARDING.createAndAdd(type9.getSegment(0), list);
    SPKSegmentFactory.DISREGARDING.createAndAdd(type12.getSegment(0), list);
    SPKSegmentFactory.DISREGARDING.createAndAdd(type13.getSegment(0), list);

    assertEquals(7, list.size());
    assertEquals(1, list.get(0).getType());
    assertEquals(2, list.get(1).getType());
    assertEquals(3, list.get(2).getType());
    assertEquals(8, list.get(3).getType());
    assertEquals(9, list.get(4).getType());
    assertEquals(12, list.get(5).getType());
    assertEquals(13, list.get(6).getType());

  }
}
