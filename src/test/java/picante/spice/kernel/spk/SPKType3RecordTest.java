package picante.spice.kernel.spk;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static picante.junit.AssertTools.assertEqualStateVector;
import static picante.junit.AssertTools.assertEqualVector;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import picante.math.functions.ChebyshevPolynomial;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;

public class SPKType3RecordTest {

  private ChebyshevPolynomial polynomial;
  private double[] data = new double[2 + 6 * 3];
  private SPKType3Record record;
  private double[] x2s;
  private double[] coeffs;
  private SPKType3Record actualRecord;
  private double[] actualCoeffs;
  private double[] actualX2S;

  @Before
  public void setUp() throws Exception {

    polynomial = new ChebyshevPolynomial();
    for (int i = 0; i < data.length; i++) {
      data[i] = i + 1;
    }
    record = new SPKType3Record();
    record.setCoefficients(2, data, 2, data, 5, data, 8, data, 11, data, 14, data, 17, data, 0);
    x2s = new double[2];
    coeffs = new double[3];

    actualRecord = new SPKType3Record();

    actualCoeffs = new double[6 * 11];

    actualCoeffs[0] = -261.619057618816;
    actualCoeffs[1] = 15.774978560157965;
    actualCoeffs[2] = 24.241002183541415;
    actualCoeffs[3] = 0.3983173179038324;
    actualCoeffs[4] = 0.15494328146037795;
    actualCoeffs[5] = 0.009184865293350969;
    actualCoeffs[6] = -0.03773225517701739;
    actualCoeffs[7] = -0.02112280248656495;
    actualCoeffs[8] = -0.008243596213934445;
    actualCoeffs[9] = 0.00559280135403907;
    actualCoeffs[10] = 0.0033640678823215406;

    actualCoeffs[11] = -35.670049065808826;
    actualCoeffs[12] = -165.80554414083076;
    actualCoeffs[13] = 0.9993410674230212;
    actualCoeffs[14] = 1.8879748688632505;
    actualCoeffs[15] = 0.1373617290014364;
    actualCoeffs[16] = 0.12984414740502892;
    actualCoeffs[17] = 0.03128064474529202;
    actualCoeffs[18] = -0.0008291565420865954;
    actualCoeffs[19] = -0.012871161371774797;
    actualCoeffs[20] = -0.005140190470141359;
    actualCoeffs[21] = 0.0032481899600552834;

    actualCoeffs[22] = 24.826138353152537;
    actualCoeffs[23] = 9.450899963573965;
    actualCoeffs[24] = -2.1976757120001413;
    actualCoeffs[25] = -0.16057596295359206;
    actualCoeffs[26] = -0.018501774149230266;
    actualCoeffs[27] = -0.009711369230285233;
    actualCoeffs[28] = -0.000630371184381473;
    actualCoeffs[29] = 0.001677804077628367;
    actualCoeffs[30] = 0.0020895993297281867;
    actualCoeffs[31] = -0.00002981200982820481;
    actualCoeffs[32] = -0.0006359001150008097;

    actualCoeffs[33] = 0.0001305426731104677;
    actualCoeffs[34] = 0.0007537511708483639;
    actualCoeffs[35] = 0.000017644319058003846;
    actualCoeffs[36] = 0.000005572091109431285;
    actualCoeffs[37] = -7.962975116180275E-7;
    actualCoeffs[38] = -0.000003992308980715502;
    actualCoeffs[39] = -0.0000015050062533889356;
    actualCoeffs[40] = -4.985816495101876E-7;
    actualCoeffs[41] = 7.767779658387598E-7;
    actualCoeffs[42] = 5.191462781360403E-7;
    actualCoeffs[43] = 0.;

    actualCoeffs[44] = -0.0012310529676484703;
    actualCoeffs[45] = 0.000041131566789350225;
    actualCoeffs[46] = 0.00009662159774057127;
    actualCoeffs[47] = 0.000010287706683701423;
    actualCoeffs[48] = 0.00000921535381171707;
    actualCoeffs[49] = 0.0000018085876095386818;
    actualCoeffs[50] = -8.034847226215803E-7;
    actualCoeffs[51] = -0.0000010877683853957647;
    actualCoeffs[52] = -7.139153430751888E-7;
    actualCoeffs[53] = 5.012638827245808E-7;
    actualCoeffs[54] = 0.;

    actualCoeffs[55] = 0.0000689204594831536;
    actualCoeffs[56] = -0.00006887010731814314;
    actualCoeffs[57] = -0.00000800630269378484;
    actualCoeffs[58] = -0.0000010406100341881559;
    actualCoeffs[59] = -5.722303348222445E-7;
    actualCoeffs[60] = 1.0147478983840367E-7;
    actualCoeffs[61] = 1.7710371072445566E-7;
    actualCoeffs[62] = 1.5984249209594747E-7;
    actualCoeffs[63] = -4.1405569205840015E-9;
    actualCoeffs[64] = -9.813273379642124E-8;
    actualCoeffs[65] = 0.;

    actualX2S = new double[2];

    actualX2S[0] = 2.295648E+8;
    actualX2S[1] = 129600.0;

    actualRecord.setCoefficients(10, actualCoeffs, 0, actualCoeffs, 11, actualCoeffs, 22,
        actualCoeffs, 33, actualCoeffs, 44, actualCoeffs, 55, actualX2S, 0);

  }

  @Test
  public void testSPKType3RecordInt() {
    SPKType3Record nonDefault = new SPKType3Record(5);
    nonDefault.getXPolynomial(polynomial);
    assertEquals(5, polynomial.getDegree());

    nonDefault.getYPolynomial(polynomial);
    assertEquals(5, polynomial.getDegree());

    nonDefault.getZPolynomial(polynomial);
    assertEquals(5, polynomial.getDegree());

    nonDefault.getDXPolynomial(polynomial);
    assertEquals(5, polynomial.getDegree());

    nonDefault.getDYPolynomial(polynomial);
    assertEquals(5, polynomial.getDegree());

    nonDefault.getDZPolynomial(polynomial);
    assertEquals(5, polynomial.getDegree());
  }

  @Test
  public void testEvaluateDoubleVectorIJK() {
    VectorIJK position = new VectorIJK();

    VectorIJK actual = actualRecord.evaluate(2.2960800000000E+08 + 10000.0, position);
    assertSame(position, actual);

    VectorIJK expected = new VectorIJK(-271.62740505705574, -106.05983303902637, 30.30683119626158);

    assertEqualVector(expected, actual);
  }

  @Test
  public void testEvaluateDoubleStateVector() {

    StateVector state = new StateVector();

    StateVector actual = actualRecord.evaluate(2.2960800000000E+08 + 10000.0, state);

    assertSame(actual, state);

    StateVector expected =
        new StateVector(new VectorIJK(-271.62740505705574, -106.05983303902637, 30.30683119626158),
            new VectorIJK(0.0004173912280652296, -0.0012876997284353752, 0.00004728906346822818));

    assertEqualStateVector(expected, actual);

  }

  @Ignore
  @Test
  public void testSetCoefficients() {
    /*
     * Implicitly tested in setUp, along with the three get tests.
     */
    fail("Not yet implemented");
  }

  @Test
  public void testGetXPolynomial() {
    record.getXPolynomial(polynomial);
    assertEquals(2, polynomial.getDegree());

    polynomial.getCoefficients(coeffs, x2s);

    double[] array = new double[3];
    System.arraycopy(data, 2, array, 0, 3);
    assertTrue(Arrays.equals(array, coeffs));

    array = new double[2];
    System.arraycopy(data, 0, array, 0, 2);
    assertTrue(Arrays.equals(array, x2s));
  }

  @Test
  public void testGetYPolynomial() {
    record.getYPolynomial(polynomial);
    assertEquals(2, polynomial.getDegree());

    polynomial.getCoefficients(coeffs, x2s);

    double[] array = new double[3];
    System.arraycopy(data, 5, array, 0, 3);
    assertTrue(Arrays.equals(array, coeffs));

    array = new double[2];
    System.arraycopy(data, 0, array, 0, 2);
    assertTrue(Arrays.equals(array, x2s));
  }

  @Test
  public void testGetZPolynomial() {
    record.getZPolynomial(polynomial);
    assertEquals(2, polynomial.getDegree());

    polynomial.getCoefficients(coeffs, x2s);

    double[] array = new double[3];
    System.arraycopy(data, 8, array, 0, 3);
    assertTrue(Arrays.equals(array, coeffs));

    array = new double[2];
    System.arraycopy(data, 0, array, 0, 2);
    assertTrue(Arrays.equals(array, x2s));

  }

  @Test
  public void testGetDXPolynomial() {
    record.getDXPolynomial(polynomial);
    assertEquals(2, polynomial.getDegree());

    polynomial.getCoefficients(coeffs, x2s);

    double[] array = new double[3];
    System.arraycopy(data, 11, array, 0, 3);
    assertTrue(Arrays.equals(array, coeffs));

    array = new double[2];
    System.arraycopy(data, 0, array, 0, 2);
    assertTrue(Arrays.equals(array, x2s));

  }

  @Test
  public void testGetDYPolynomial() {
    record.getDYPolynomial(polynomial);
    assertEquals(2, polynomial.getDegree());

    polynomial.getCoefficients(coeffs, x2s);

    double[] array = new double[3];
    System.arraycopy(data, 14, array, 0, 3);
    assertTrue(Arrays.equals(array, coeffs));

    array = new double[2];
    System.arraycopy(data, 0, array, 0, 2);
    assertTrue(Arrays.equals(array, x2s));

  }

  @Test
  public void testGetDZPolynomial() {
    record.getDZPolynomial(polynomial);
    assertEquals(2, polynomial.getDegree());

    polynomial.getCoefficients(coeffs, x2s);

    double[] array = new double[3];
    System.arraycopy(data, 17, array, 0, 3);
    assertTrue(Arrays.equals(array, coeffs));

    array = new double[2];
    System.arraycopy(data, 0, array, 0, 2);
    assertTrue(Arrays.equals(array, x2s));

  }

}
