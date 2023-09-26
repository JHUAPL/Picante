package picante.spice.daf.content.recordtables;

import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import picante.math.functions.ChebyshevPolynomial;
import picante.spice.daf.DAF;
import picante.spice.daf.DAFFactory;
import picante.spice.daf.DAFSegment;
import picante.spice.kernel.spk.SPKType2Record;
import picante.spice.kernel.spk.SPKType2Test;

public class DAFChebyshevTripletRecordListTest {

  private static DAF daf;
  private DAFChebyshevTripletRecordList<SPKType2Record> list;
  private SPKType2Record record;

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    daf = DAFFactory.createDAF(SPKType2Test.class.getResourceAsStream("type2.bsp"));
  }

  @Before
  public void setUp() throws Exception {
    double[] values = new double[2];
    DAFSegment segment = daf.getSegment(0);
    segment.get(segment.getLength() - 2, values, 0, 2);
    int recordSize = (int) values[0];
    int records = (int) values[1];
    list = new DAFChebyshevTripletRecordList<SPKType2Record>(segment, records, 0, recordSize);
    record = new SPKType2Record();
  }

  @Test
  public void testGetLength() {
    assertEquals(4, list.size());
  }

  @Test
  public void testObtainRecordIntSPKType2Record() {

    double[] coeffs = new double[13];
    double[] x2s = new double[2];
    ChebyshevPolynomial expected = new ChebyshevPolynomial();
    ChebyshevPolynomial actual = new ChebyshevPolynomial();

    list.get(2, record);

    x2s[0] = 2.2960800000000E+08;
    x2s[1] = 1.7280000000000E+05;

    coeffs[0] = -2603.3011653580106;
    coeffs[1] = -1596.9160392353244;
    coeffs[2] = 152.2790331353133;
    coeffs[3] = 17.250822907681624;
    coeffs[4] = -0.3258010333923448;
    coeffs[5] = -0.06880293398790646;
    coeffs[6] = -0.003450790285756092;
    coeffs[7] = 0.00003770565412376096;
    coeffs[8] = 0.000026104657500417755;
    coeffs[9] = 0.0000019030982702571786;
    coeffs[10] = -1.4400063989825116E-8;
    coeffs[11] = -1.3985820087230936E-8;
    coeffs[12] = -1.1213782345230748E-9;

    expected.setCoefficients(12, coeffs, x2s);
    record.getXPolynomial(actual);

    assertEquals(expected, actual);

    coeffs[0] = 3078.863816207362;
    coeffs[1] = -1204.813953695642;
    coeffs[2] = -178.8325372168099;
    coeffs[3] = 8.58450526023314;
    coeffs[4] = 1.0609636881502644;
    coeffs[5] = 0.01337019932765798;
    coeffs[6] = -0.0026346814289264432;
    coeffs[7] = -0.00031202537357911313;
    coeffs[8] = -0.00001093596889399905;
    coeffs[9] = 0.0000011511073156955534;
    coeffs[10] = 1.6571862680091748E-7;
    coeffs[11] = 6.3518900052864675E-9;
    coeffs[12] = -6.064508618380796E-10;

    expected.setCoefficients(12, coeffs, x2s);
    record.getYPolynomial(actual);

    assertEquals(expected, actual);

    coeffs[0] = 1602.7075356380412;
    coeffs[1] = -694.7961065742646;
    coeffs[2] = -92.99685101271504;
    coeffs[3] = 5.119956440412413;
    coeffs[4] = 0.5666918789050632;
    coeffs[5] = 0.005367354351802166;
    coeffs[6] = -0.0015204053488947617;
    coeffs[7] = -0.00016804914732018576;
    coeffs[8] = -0.000005231425247181703;
    coeffs[9] = 6.745454573614899E-7;
    coeffs[10] = 8.943275980795523E-8;
    coeffs[11] = 3.0705709468265158E-9;
    coeffs[12] = -3.5841358653730886E-10;

    expected.setCoefficients(12, coeffs, x2s);
    record.getZPolynomial(actual);

    assertEquals(expected, actual);
  }

}
