package picante.spice.kernel.pck;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import picante.junit.AssertTools;
import picante.math.functions.ChebyshevPolynomial;
import picante.mechanics.rotations.EulerAngles;

public class PCKType2RecordTest {

  private static final double TOLERANCE = 1e-14;

  private ChebyshevPolynomial polynomial;
  private PCKType2Record record;
  private PCKType2Record actualRecord;
  private double[] x2s;
  private double[] coeffs;
  private double[] actualCoeffs;
  private double[] actualX2S;
  private double[] data;

  @Before
  public void setUp() throws Exception {
    polynomial = new ChebyshevPolynomial();

    data = new double[2 + 3 * 3];
    for (int i = 0; i < data.length; i++) {
      data[i] = (double) i + 1;
    }

    record = new PCKType2Record();
    record.setCoefficients(2, data, 2, data, 5, data, 8, data, 0);

    coeffs = new double[3];
    x2s = new double[2];

    actualX2S = new double[] {302400.00000000000, 345600.00000000000};
    actualCoeffs = new double[] {-5.42421658946042676E-002, -5.18026404279307897E-005,
        8.97765545795517445E-005, -1.53049817633425698E-005, 1.32139493194244644E-006,
        5.98394217685485710E-007, -6.59630101830294498E-008, -9.90919880123506016E-009,
        4.92715875384412191E-010, 1.16123461129340697E-010, 0.42498915872431681,
        1.40035149979810604E-004, -1.88981844663768974E-005, -2.19813973158239655E-006,
        1.42376062148040280E-006, -1.69896143704177179E-007, -3.48300902224302006E-008,
        2.92066301735029393E-009, 4.42195222535900828E-010, -3.91919489001621353E-012,
        2565.0633501887387, 0.92003729484821883, -8.05264900251033612E-005,
        1.19756966537932548E-005, -1.22334393349781505E-006, -5.36670677569200021E-007,
        6.08391076616600886E-008, 9.02182072896612341E-009, -4.64563395583544875E-010,
        -1.04469683995618839E-010,};

    actualRecord = new PCKType2Record();
    actualRecord.setCoefficients(9, actualCoeffs, 0, actualCoeffs, 10, actualCoeffs, 20, actualX2S,
        0);
  }

  @Test
  public void testEvaluate() {

    EulerAngles.KIK angles = new EulerAngles.KIK();

    EulerAngles.KIK result = actualRecord.evaluate(0.0, angles);
    assertSame(angles, result);

    AssertTools.assertRelativeEquality(0.71866883439673757, angles.getLeftAngle(), TOLERANCE);
    AssertTools.assertRelativeEquality(0.42485598665803781, angles.getCenterAngle(), TOLERANCE);
    AssertTools.assertRelativeEquality(-5.41483383638381374E-002, angles.getRightAngle(),
        TOLERANCE);

  }

  @Test
  public void testPCKType2RecordInt() {
    PCKType2Record nonDefault = new PCKType2Record(10);
    nonDefault.getAPolynomial(polynomial);
    assertEquals(10, polynomial.getDegree());
    nonDefault.getBPolynomial(polynomial);
    assertEquals(10, polynomial.getDegree());
    nonDefault.getCPolynomial(polynomial);
    assertEquals(10, polynomial.getDegree());
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
  public void testGetAPolynomial() {
    record.getAPolynomial(polynomial);

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
  public void testGetBPolynomial() {
    record.getBPolynomial(polynomial);

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
  public void testGetCPolynomial() {
    record.getCPolynomial(polynomial);

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
