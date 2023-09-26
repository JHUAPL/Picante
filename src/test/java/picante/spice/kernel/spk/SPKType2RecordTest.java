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

public class SPKType2RecordTest {

  private ChebyshevPolynomial polynomial;
  private double[] data = new double[2 + 3 * 3];
  private SPKType2Record record;
  private double[] x2s;
  private double[] coeffs;
  private SPKType2Record actualRecord;
  private double[] actualCoeffs;
  private double[] actualX2S;

  @Before
  public void setUp() throws Exception {
    polynomial = new ChebyshevPolynomial();
    for (int i = 0; i < data.length; i++) {
      data[i] = i + 1;
    }
    record = new SPKType2Record();
    record.setCoefficients(2, data, 2, data, 5, data, 8, data, 0);
    x2s = new double[2];
    coeffs = new double[3];

    actualRecord = new SPKType2Record();

    actualCoeffs = new double[3 * 13];

    actualCoeffs[0] = -2603.3011653580106;
    actualCoeffs[1] = -1596.9160392353244;
    actualCoeffs[2] = 152.2790331353133;
    actualCoeffs[3] = 17.250822907681624;
    actualCoeffs[4] = -0.3258010333923448;
    actualCoeffs[5] = -0.06880293398790646;
    actualCoeffs[6] = -0.003450790285756092;
    actualCoeffs[7] = 0.00003770565412376096;
    actualCoeffs[8] = 0.000026104657500417755;
    actualCoeffs[9] = 0.0000019030982702571786;
    actualCoeffs[10] = -1.4400063989825116E-8;
    actualCoeffs[11] = -1.3985820087230936E-8;
    actualCoeffs[12] = -1.1213782345230748E-9;

    actualCoeffs[13] = 3078.863816207362;
    actualCoeffs[14] = -1204.813953695642;
    actualCoeffs[15] = -178.8325372168099;
    actualCoeffs[16] = 8.58450526023314;
    actualCoeffs[17] = 1.0609636881502644;
    actualCoeffs[18] = 0.01337019932765798;
    actualCoeffs[19] = -0.0026346814289264432;
    actualCoeffs[20] = -0.00031202537357911313;
    actualCoeffs[21] = -0.00001093596889399905;
    actualCoeffs[22] = 0.0000011511073156955534;
    actualCoeffs[23] = 1.6571862680091748E-7;
    actualCoeffs[24] = 6.3518900052864675E-9;
    actualCoeffs[25] = -6.064508618380796E-10;

    actualCoeffs[26] = 1602.7075356380412;
    actualCoeffs[27] = -694.7961065742646;
    actualCoeffs[28] = -92.99685101271504;
    actualCoeffs[29] = 5.119956440412413;
    actualCoeffs[30] = 0.5666918789050632;
    actualCoeffs[31] = 0.005367354351802166;
    actualCoeffs[32] = -0.0015204053488947617;
    actualCoeffs[33] = -0.00016804914732018576;
    actualCoeffs[34] = -0.000005231425247181703;
    actualCoeffs[35] = 6.745454573614899E-7;
    actualCoeffs[36] = 8.943275980795523E-8;
    actualCoeffs[37] = 3.0705709468265158E-9;
    actualCoeffs[38] = -3.5841358653730886E-10;

    actualX2S = new double[2];

    actualX2S[0] = 2.2960800000000E+08;
    actualX2S[1] = 1.7280000000000E+05;

    actualRecord.setCoefficients(12, actualCoeffs, 0, actualCoeffs, 13, actualCoeffs, 26, actualX2S,
        0);

  }

  @Test
  public void testSPKType2RecordInt() {
    SPKType2Record nonDefault = new SPKType2Record(10);
    nonDefault.getXPolynomial(polynomial);
    assertEquals(10, polynomial.getDegree());

    nonDefault.getYPolynomial(polynomial);
    assertEquals(10, polynomial.getDegree());

    nonDefault.getZPolynomial(polynomial);
    assertEquals(10, polynomial.getDegree());
  }

  @Test
  public void testEvaluateDoubleVectorIJK() {

    VectorIJK position = new VectorIJK();

    VectorIJK actual = actualRecord.evaluate(2.2960800000000E+08 - 2000.0, position);
    assertSame(position, actual);

    VectorIJK expected = new VectorIJK(-2736.775696772357, 3272.9527121203737, 1704.4661091804126);

    assertEqualVector(expected, actual);

  }

  @Test
  public void testEvaluateDoubleStateVector() {

    StateVector state = new StateVector();

    StateVector actual = actualRecord.evaluate(2.2960800000000E+08 - 2000.0, state);

    assertSame(actual, state);

    StateVector expected =
        new StateVector(new VectorIJK(-2736.775696772357, 3272.9527121203737, 1704.4661091804126),
            new VectorIJK(-0.009583873270934148, -0.0070718052308111155, -0.0040839630765958325));

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

}
