package picante.spice.daf.content.recordtables;

import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import picante.math.functions.ChebyshevPolynomial;
import picante.spice.daf.DAF;
import picante.spice.daf.DAFFactory;
import picante.spice.daf.DAFSegment;
import picante.spice.kernel.spk.SPKType3Record;
import picante.spice.kernel.spk.SPKType3Test;

public class DAFType3SPKRecordListTest {

  private static DAF daf;
  private DAFType3SPKRecordList list;
  private SPKType3Record record;

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    daf = DAFFactory.createDAF(SPKType3Test.class.getResourceAsStream("type3.bsp"));
  }

  @Before
  public void setUp() throws Exception {
    double[] values = new double[2];
    DAFSegment segment = daf.getSegment(0);
    segment.get(segment.getLength() - 2, values, 0, 2);
    int recordSize = (int) values[0];
    int records = (int) values[1];
    list = new DAFType3SPKRecordList(segment, records, 0, recordSize);
    record = new SPKType3Record();
  }

  @Test
  public void testGetLegnth() {
    assertEquals(5, list.size());
  }

  @Test
  public void testDAFType3SPKRecordList() {

    double[] coeffs = new double[11];
    double[] x2s = new double[2];
    ChebyshevPolynomial expected = new ChebyshevPolynomial();
    ChebyshevPolynomial actual = new ChebyshevPolynomial();

    list.get(2, record);

    x2s[0] = 2.295648E+8;
    x2s[1] = 129600.0;

    coeffs[0] = -261.619057618816;
    coeffs[1] = 15.774978560157965;
    coeffs[2] = 24.241002183541415;
    coeffs[3] = 0.3983173179038324;
    coeffs[4] = 0.15494328146037795;
    coeffs[5] = 0.009184865293350969;
    coeffs[6] = -0.03773225517701739;
    coeffs[7] = -0.02112280248656495;
    coeffs[8] = -0.008243596213934445;
    coeffs[9] = 0.00559280135403907;
    coeffs[10] = 0.0033640678823215406;

    expected.setCoefficients(10, coeffs, x2s);
    record.getXPolynomial(actual);

    assertEquals(expected, actual);

    coeffs[0] = -35.670049065808826;
    coeffs[1] = -165.80554414083076;
    coeffs[2] = 0.9993410674230212;
    coeffs[3] = 1.8879748688632505;
    coeffs[4] = 0.1373617290014364;
    coeffs[5] = 0.12984414740502892;
    coeffs[6] = 0.03128064474529202;
    coeffs[7] = -0.0008291565420865954;
    coeffs[8] = -0.012871161371774797;
    coeffs[9] = -0.005140190470141359;
    coeffs[10] = 0.0032481899600552834;

    expected.setCoefficients(10, coeffs, x2s);
    record.getYPolynomial(actual);

    assertEquals(expected, actual);

    coeffs[0] = 24.826138353152537;
    coeffs[1] = 9.450899963573965;
    coeffs[2] = -2.1976757120001413;
    coeffs[3] = -0.16057596295359206;
    coeffs[4] = -0.018501774149230266;
    coeffs[5] = -0.009711369230285233;
    coeffs[6] = -0.000630371184381473;
    coeffs[7] = 0.001677804077628367;
    coeffs[8] = 0.0020895993297281867;
    coeffs[9] = -0.00002981200982820481;
    coeffs[10] = -0.0006359001150008097;

    expected.setCoefficients(10, coeffs, x2s);
    record.getZPolynomial(actual);

    assertEquals(expected, actual);

    coeffs[0] = 0.0001305426731104677;
    coeffs[1] = 0.0007537511708483639;
    coeffs[2] = 0.000017644319058003846;
    coeffs[3] = 0.000005572091109431285;
    coeffs[4] = -7.962975116180275E-7;
    coeffs[5] = -0.000003992308980715502;
    coeffs[6] = -0.0000015050062533889356;
    coeffs[7] = -4.985816495101876E-7;
    coeffs[8] = 7.767779658387598E-7;
    coeffs[9] = 5.191462781360403E-7;
    coeffs[10] = 0.;

    expected.setCoefficients(10, coeffs, x2s);
    record.getDXPolynomial(actual);

    assertEquals(expected, actual);

    coeffs[0] = -0.0012310529676484703;
    coeffs[1] = 0.000041131566789350225;
    coeffs[2] = 0.00009662159774057127;
    coeffs[3] = 0.000010287706683701423;
    coeffs[4] = 0.00000921535381171707;
    coeffs[5] = 0.0000018085876095386818;
    coeffs[6] = -8.034847226215803E-7;
    coeffs[7] = -0.0000010877683853957647;
    coeffs[8] = -7.139153430751888E-7;
    coeffs[9] = 5.012638827245808E-7;
    coeffs[10] = 0.;

    expected.setCoefficients(10, coeffs, x2s);
    record.getDYPolynomial(actual);

    assertEquals(expected, actual);

    coeffs[0] = 0.0000689204594831536;
    coeffs[1] = -0.00006887010731814314;
    coeffs[2] = -0.00000800630269378484;
    coeffs[3] = -0.0000010406100341881559;
    coeffs[4] = -5.722303348222445E-7;
    coeffs[5] = 1.0147478983840367E-7;
    coeffs[6] = 1.7710371072445566E-7;
    coeffs[7] = 1.5984249209594747E-7;
    coeffs[8] = -4.1405569205840015E-9;
    coeffs[9] = -9.813273379642124E-8;
    coeffs[10] = 0.;

    expected.setCoefficients(10, coeffs, x2s);
    record.getDZPolynomial(actual);

    assertEquals(expected, actual);

  }

}
