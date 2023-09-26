package picante.spice.daf.content.recordtables;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import picante.spice.daf.DAF;
import picante.spice.daf.DAFFactory;
import picante.spice.daf.DAFSegment;
import picante.spice.kernel.spk.SPKType1Record;
import picante.spice.kernel.spk.SPKType1Test;

public class DAFType1SPKRecordTableTest {

  private static DAF daf;

  private DAFType1SPKRecordTable table;
  private DAFTimeListTableTestWrapper timeTableWrapper;

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    daf = DAFFactory.createDAF(SPKType1Test.class.getResourceAsStream("type1.bsp"));
  }

  @Before
  public void setUp() throws Exception {
    double[] values = new double[1];
    DAFSegment segment = daf.getSegment(0);
    segment.get(segment.getLength() - 1, values, 0, 1);
    int records = (int) values[0];
    timeTableWrapper = new DAFTimeListTableTestWrapper(records, 71 * records, segment);
    table = new DAFType1SPKRecordTable(segment, timeTableWrapper, 0);
  }

  @Test
  public void testObtainTime() {
    /*
     * We're not testing the DAFTimeListTable API, so just check to make sure that it invokes the
     * getTime method of the recordtable once and only once.
     */
    int counter = timeTableWrapper.getGetTimeInvokedCounter();
    table.obtainTime(0);
    int newCounter = timeTableWrapper.getGetTimeInvokedCounter();

    assertEquals(counter + 1, newCounter);
  }

  @Test
  public void testObtainRecordIntSPKType1Record() {
    SPKType1Record record = new SPKType1Record();
    SPKType1Record result = table.get(8033, record);
    assertSame(record, result);

    /*
     * Check the contents of the record were assigned properly.
     */
    assertEquals(2.2999710080134743E+8, record.getTl(), 0.0);

    double[] g = record.getG(new double[15]);

    assertEquals(171.96593269076357, g[0], 0.0);
    assertEquals(343.93186538152713, g[1], 0.0);
    assertEquals(687.8637307630543, g[2], 0.0);
    assertEquals(859.8296634538178, g[3], 0.0);
    assertEquals(1031.7955961445814, g[4], 0.0);
    assertEquals(1203.761528835345, g[5], 0.0);
    assertEquals(1596.8265178428046, g[6], 0.0);
    assertEquals(1628.4120973166187, g[7], 0.0);
    assertEquals(1554.8994139574054, g[8], 0.0);
    assertEquals(1385.2385978845211, g[9], 0.0);
    assertEquals(1325.897751078276, g[10], 0.0);
    assertEquals(707.3898701794653, g[11], 0.0);
    assertEquals(1243.4065054980683, g[12], 0.0);
    assertEquals(1389.2464923208004, g[13], 0.0);
    assertEquals(1493.1153889429163, g[14], 0.0);

    double[] refpos = record.getRefpos(new double[3]);

    assertEquals(1689688.7226758278, refpos[0], 0.0);
    assertEquals(894342.9694968559, refpos[1], 0.0);
    assertEquals(855548.7436215796, refpos[2], 0.0);

    double[] refvel = record.getRefvel(new double[3]);

    assertEquals(-1.1828001356848592, refvel[0], 0.0);
    assertEquals(1.4509658069393678, refvel[1], 0.0);
    assertEquals(1.2908925904435702, refvel[2], 0.0);

    double[][] dt = record.getDt(new double[3][15]);

    assertEquals(-0.000006976292685782994, dt[0][0], 0.0);
    assertEquals(1.171157213007038E-9, dt[0][1], 0.0);
    assertEquals(-1.0772547640596745E-12, dt[0][2], 0.0);
    assertEquals(7.464539813999443E-16, dt[0][3], 0.0);
    assertEquals(-1.6068528411604562E-15, dt[0][4], 0.0);
    assertEquals(0.0, dt[0][5], 0.0);
    assertEquals(0.0, dt[0][6], 0.0);
    assertEquals(-5.960192011533915E-15, dt[0][7], 0.0);
    assertEquals(0.0, dt[0][8], 0.0);
    assertEquals(0.0, dt[0][9], 0.0);
    assertEquals(0.0, dt[0][10], 0.0);
    assertEquals(0.0, dt[0][11], 0.0);
    assertEquals(0.0, dt[0][12], 0.0);
    assertEquals(0.0, dt[0][13], 0.0);
    assertEquals(0.0, dt[0][14], 0.0);
    assertEquals(-0.0000036931744048113546, dt[1][0], 0.0);
    assertEquals(-8.55374056617487E-10, dt[1][1], 0.0);
    assertEquals(-4.2895300419450225E-13, dt[1][2], 0.0);
    assertEquals(-9.106965666483781E-16, dt[1][3], 0.0);
    assertEquals(-6.806303601508776E-16, dt[1][4], 0.0);
    assertEquals(0.0, dt[1][5], 0.0);
    assertEquals(0.0, dt[1][6], 0.0);
    assertEquals(-2.7039051189251355E-15, dt[1][7], 0.0);
    assertEquals(0.0, dt[1][8], 0.0);
    assertEquals(0.0, dt[1][9], 0.0);
    assertEquals(0.0, dt[1][10], 0.0);
    assertEquals(0.0, dt[1][11], 0.0);
    assertEquals(0.0, dt[1][12], 0.0);
    assertEquals(0.0, dt[1][13], 0.0);
    assertEquals(0.0, dt[1][14], 0.0);
    assertEquals(-0.0000035326900204876533, dt[2][0], 0.0);
    assertEquals(-7.491733012164379E-10, dt[2][1], 0.0);
    assertEquals(-4.1768741383113745E-13, dt[2][2], 0.0);
    assertEquals(-8.077138041408961E-16, dt[2][3], 0.0);
    assertEquals(-8.089135000125414E-16, dt[2][4], 0.0);
    assertEquals(0.0, dt[2][5], 0.0);
    assertEquals(0.0, dt[2][6], 0.0);
    assertEquals(2.7636402074568415E-15, dt[2][7], 0.0);
    assertEquals(0.0, dt[2][8], 0.0);
    assertEquals(0.0, dt[2][9], 0.0);
    assertEquals(0.0, dt[2][10], 0.0);
    assertEquals(0.0, dt[2][11], 0.0);
    assertEquals(0.0, dt[2][12], 0.0);
    assertEquals(0.0, dt[2][13], 0.0);
    assertEquals(0.0, dt[2][14], 0.0);

    assertEquals(5.0, record.getKqmax1(), 0.0);

    int[] kq = record.getKq(new int[3]);

    assertEquals(4.0, kq[0], 0.0);
    assertEquals(4.0, kq[1], 0.0);
    assertEquals(4.0, kq[2], 0.0);

    /*
     * Now just check that the index arithmetic is appropriate, and we didn't just hit pay dirt
     * accidentally.
     */
    table.get(2, record);

    assertEquals(2.289601801010434E+8, record.getTl(), 0.0);

    record.getKq(kq);

    assertEquals(4, kq[0], 0.0);
    assertEquals(6, kq[1], 0.0);
    assertEquals(6, kq[2], 0.0);
  }

}


class DAFTimeListTableTestWrapper extends DAFTimeListTable {

  private int getTimeInvokedCounter = 0;

  public DAFTimeListTableTestWrapper(int length, int startIndex, DAFSegment segment) {
    super(segment, length, startIndex);
  }

  @Override
  public double getGauge(int index) {
    getTimeInvokedCounter++;
    return super.getGauge(index);
  }

  public int getGetTimeInvokedCounter() {
    return getTimeInvokedCounter;
  }

}
