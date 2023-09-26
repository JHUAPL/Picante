package picante.spice.kernelpool.content;

import java.util.List;

import org.junit.BeforeClass;
import org.junit.Test;

import com.google.common.collect.Lists;
import picante.junit.AssertTools;

public class LargeAngles {
  static double tol = 1E-15;

  List<Double> rpdXval = Lists.newArrayList(3.4906585039886591E-002, 0.34906585039886590,
      3.4906585039886591, 34.906585039886593, 349.06585039886590, 3490.6585039886590,
      34906.585039886588, 349065.85039886594, 3490658.5039886590, 34906585.039886594,
      349065850.39886594, 3490658503.9886589);

  List<Double> valXrpd = Lists.newArrayList(3.4906585039886591E-002, 0.34906585039886590,
      3.4906585039886591, 34.906585039886593, 349.06585039886590, 3490.6585039886590,
      34906.585039886588, 349065.85039886594, 3490658.5039886590, 34906585.039886594,
      349065850.39886594, 3490658503.9886589);


  List<Double> cos = Lists.newArrayList(1.0000000000000000, 1.0000000000000000, 1.0000000000000000,
      1.0000000000000000, 1.0000000000000000, 1.0000000000000000, 1.0000000000000000,
      1.0000000000000000, 0.99999999999999689, 0.99999999999999778, 0.99999999998997047,
      0.99999999956403485);

  List<Double> sin = Lists.newArrayList(0.90929742682568171, 0.91294525072762767,
      -0.87329729721399463, 0.93003950441613703, 0.58198476199429494, -7.1451895212519906E-002,
      -0.65571431556347004, -0.76310111747215914, -0.67708746222703275, 0.91471045864904121,
      -0.85130215335420245, 0.68880798718504987);

  List<Double> tan = Lists.newArrayList(-2.1850398632615189, 2.2371609442247422,
      -1.7925274837903817, -2.5309983280933408, 0.71567263086205468, -7.1634990890478131E-002,
      -0.86848531796085437, -1.1807609264644945, 0.92007760925990489, 2.2635192929011994,
      -1.6225300134811245, -0.95015359617113115);

  static List<Double> rpdXvalJava = Lists.newArrayList();
  static List<Double> valXrpdJava = Lists.newArrayList();
  static List<Double> cosJava = Lists.newArrayList();
  static List<Double> sinJava = Lists.newArrayList();
  static List<Double> tanJava = Lists.newArrayList();

  @BeforeClass
  public static void setup() {
    Double rpd = Math.toRadians(1);
    rpdXvalJava.add(rpd * 2E0);
    rpdXvalJava.add(rpd * 2E1);
    rpdXvalJava.add(rpd * 2E2);
    rpdXvalJava.add(rpd * 2E3);
    rpdXvalJava.add(rpd * 2E4);
    rpdXvalJava.add(rpd * 2E5);
    rpdXvalJava.add(rpd * 2E6);
    rpdXvalJava.add(rpd * 2E7);
    rpdXvalJava.add(rpd * 2E8);
    rpdXvalJava.add(rpd * 2E9);
    rpdXvalJava.add(rpd * 2E10);
    rpdXvalJava.add(rpd * 2E11);

    valXrpdJava.add(2E0 * rpd);
    valXrpdJava.add(2E1 * rpd);
    valXrpdJava.add(2E2 * rpd);
    valXrpdJava.add(2E3 * rpd);
    valXrpdJava.add(2E4 * rpd);
    valXrpdJava.add(2E5 * rpd);
    valXrpdJava.add(2E6 * rpd);
    valXrpdJava.add(2E7 * rpd);
    valXrpdJava.add(2E8 * rpd);
    valXrpdJava.add(2E9 * rpd);
    valXrpdJava.add(2E10 * rpd);
    valXrpdJava.add(2E11 * rpd);

    cosJava.add(Math.cos(2E0 * Math.PI));
    cosJava.add(Math.cos(2E1 * Math.PI));
    cosJava.add(Math.cos(2E2 * Math.PI));
    cosJava.add(Math.cos(2E3 * Math.PI));
    cosJava.add(Math.cos(2E4 * Math.PI));
    cosJava.add(Math.cos(2E5 * Math.PI));
    cosJava.add(Math.cos(2E6 * Math.PI));
    cosJava.add(Math.cos(2E7 * Math.PI));
    cosJava.add(Math.cos(2E8 * Math.PI));
    cosJava.add(Math.cos(2E9 * Math.PI));
    cosJava.add(Math.cos(2E10 * Math.PI));
    cosJava.add(Math.cos(2E11 * Math.PI));

    sinJava.add(Math.sin(2E0));
    sinJava.add(Math.sin(2E1));
    sinJava.add(Math.sin(2E2));
    sinJava.add(Math.sin(2E3));
    sinJava.add(Math.sin(2E4));
    sinJava.add(Math.sin(2E5));
    sinJava.add(Math.sin(2E6));
    sinJava.add(Math.sin(2E7));
    sinJava.add(Math.sin(2E8));
    sinJava.add(Math.sin(2E9));
    sinJava.add(Math.sin(2E10));
    sinJava.add(Math.sin(2E11));

    tanJava.add(Math.tan(2E0));
    tanJava.add(Math.tan(2E1));
    tanJava.add(Math.tan(2E2));
    tanJava.add(Math.tan(2E3));
    tanJava.add(Math.tan(2E4));
    tanJava.add(Math.tan(2E5));
    tanJava.add(Math.tan(2E6));
    tanJava.add(Math.tan(2E7));
    tanJava.add(Math.tan(2E8));
    tanJava.add(Math.tan(2E9));
    tanJava.add(Math.tan(2E10));
    tanJava.add(Math.tan(2E11));
  }

  @Test
  public void testRPDxVAL() {
    for (int i = 0; i < 10; i++) {
      AssertTools.assertRelativeEquality(rpdXval.get(i), rpdXvalJava.get(i), tol);
    }
  }

  @Test
  public void testVALxRPD() {
    for (int i = 0; i < 10; i++) {
      AssertTools.assertRelativeEquality(valXrpd.get(i), valXrpdJava.get(i), tol);
    }
  }

  @Test
  public void testCOS() {
    for (int i = 0; i < 10; i++) {
      AssertTools.assertRelativeEquality(cos.get(i), cosJava.get(i), tol);
    }
  }

  @Test
  public void testSIN() {
    for (int i = 0; i < 10; i++) {
      AssertTools.assertRelativeEquality(sin.get(i), sinJava.get(i), tol);
    }
  }

  @Test
  public void testTAN() {
    for (int i = 0; i < 10; i++) {
      AssertTools.assertRelativeEquality(tan.get(i), tanJava.get(i), tol);
    }
  }

}
