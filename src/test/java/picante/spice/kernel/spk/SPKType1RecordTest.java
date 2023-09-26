package picante.spice.kernel.spk;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;

public class SPKType1RecordTest {

  private SPKType1Record record;
  private double[] data = new double[71];

  @Before
  public void setUp() throws Exception {
    record = new SPKType1Record();

    data[0] = 2.2999710080134743E+8;
    data[1] = 171.96593269076357;
    data[2] = 343.93186538152713;
    data[3] = 687.8637307630543;
    data[4] = 859.8296634538178;
    data[5] = 1031.7955961445814;
    data[6] = 1203.761528835345;
    data[7] = 1596.8265178428046;
    data[8] = 1628.4120973166187;
    data[9] = 1554.8994139574054;
    data[10] = 1385.2385978845211;
    data[11] = 1325.897751078276;
    data[12] = 707.3898701794653;
    data[13] = 1243.4065054980683;
    data[14] = 1389.2464923208004;
    data[15] = 1493.1153889429163;
    data[16] = 1689688.7226758278;
    data[17] = -1.1828001356848592;
    data[18] = 894342.9694968559;
    data[19] = 1.4509658069393678;
    data[20] = 855548.7436215796;
    data[21] = 1.2908925904435702;
    data[22] = -0.000006976292685782994;
    data[23] = 1.171157213007038E-9;
    data[24] = -1.0772547640596745E-12;
    data[25] = 7.464539813999443E-16;
    data[26] = -1.6068528411604562E-15;
    data[27] = 0.0;
    data[28] = 0.0;
    data[29] = -5.960192011533915E-15;
    data[30] = 0.0;
    data[31] = 0.0;
    data[32] = 0.0;
    data[33] = 0.0;
    data[34] = 0.0;
    data[35] = 0.0;
    data[36] = 0.0;
    data[37] = -0.0000036931744048113546;
    data[38] = -8.55374056617487E-10;
    data[39] = -4.2895300419450225E-13;
    data[40] = -9.106965666483781E-16;
    data[41] = -6.806303601508776E-16;
    data[42] = 0.0;
    data[43] = 0.0;
    data[44] = -2.7039051189251355E-15;
    data[45] = 0.0;
    data[46] = 0.0;
    data[47] = 0.0;
    data[48] = 0.0;
    data[49] = 0.0;
    data[50] = 0.0;
    data[51] = 0.0;
    data[52] = -0.0000035326900204876533;
    data[53] = -7.491733012164379E-10;
    data[54] = -4.1768741383113745E-13;
    data[55] = -8.077138041408961E-16;
    data[56] = -8.089135000125414E-16;
    data[57] = 0.0;
    data[58] = 0.0;
    data[59] = 2.7636402074568415E-15;
    data[60] = 0.0;
    data[61] = 0.0;
    data[62] = 0.0;
    data[63] = 0.0;
    data[64] = 0.0;
    data[65] = 0.0;
    data[66] = 0.0;
    data[67] = 5.0;
    data[68] = 4.0;
    data[69] = 4.0;
    data[70] = 4.0;

    record.setRecord(data);

  }

  @Ignore
  @Test
  public void testSetRecord() {
    /*
     * This is tested implicitly by the getters...
     */
    fail("Not yet implemented");
  }

  @Test
  public void testEvaluate() {
    StateVector state = record.evaluate(2.299967000004828E8, new StateVector());

    VectorIJK v;
    v = state.getPosition();
    assertEquals(1690162.2295793374, v.getI(), 0.0);
    assertEquals(893761.1245620202, v.getJ(), 0.0);
    assertEquals(855031.0690539064, v.getK(), 0.0);

    v = state.getVelocity();
    assertEquals(-1.180003484389045, v.getI(), 0.0);
    assertEquals(1.4524456349673072, v.getJ(), 0.0);
    assertEquals(1.2923081457935746, v.getK(), 0.0);

  }

  @Test
  public void testGetTl() {
    assertEquals(2.2999710080134743E+8, record.getTl(), 0.0);
  }

  @Test
  public void testGetG() {
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
  }

  @Test
  public void testGetRefpos() {
    double[] refpos = record.getRefpos(new double[3]);

    assertEquals(1689688.7226758278, refpos[0], 0.0);
    assertEquals(894342.9694968559, refpos[1], 0.0);
    assertEquals(855548.7436215796, refpos[2], 0.0);
  }

  @Test
  public void testGetRefvel() {
    double[] refvel = record.getRefvel(new double[3]);

    assertEquals(-1.1828001356848592, refvel[0], 0.0);
    assertEquals(1.4509658069393678, refvel[1], 0.0);
    assertEquals(1.2908925904435702, refvel[2], 0.0);
  }

  @Test
  public void testGetDt() {
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
  }

  @Test
  public void testGetKqmax1() {
    assertEquals(5.0, record.getKqmax1(), 0.0);
  }

  @Test
  public void testGetKq() {
    int[] kq = record.getKq(new int[3]);

    assertEquals(4.0, kq[0], 0.0);
    assertEquals(4.0, kq[1], 0.0);
    assertEquals(4.0, kq[2], 0.0);
  }

}
