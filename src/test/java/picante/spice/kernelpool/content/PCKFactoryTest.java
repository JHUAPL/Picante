package picante.spice.kernelpool.content;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static picante.junit.AssertTools.assertRotationAngleEquals;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.spice.kernel.tk.pck.JEDOffsetConverter;
import picante.spice.kernel.tk.pck.PCKFrameFunction;
import picante.spice.kernel.tk.pck.PCKWithNutationFrameFunction;
import picante.spice.kernel.tk.pck.TimeConverter;
import picante.spice.kernelpool.BasicKernelPool;
import picante.units.FundamentalPhysicalConstants;

// TODO: Add tests for missing NUT_PREC arrays, as these should be equivalent to empty arrays.
public class PCKFactoryTest {

  private final static double MAT_TOLERANCE = 1E-12;

  private PCKFactory factory;
  private KernelPoolValidatingRetriever pool;
  private KernelPoolValidatingRetriever exceptionPool;
  private BasicKernelPool data;
  private BasicKernelPool exceptionData;

  @Before
  public void setUp() throws Exception {
    factory = new PCKFactory();

    /*
     * To assemble the string output array use a simple replace/find with this regular expression:
     */
    // Find: ^\s*//\s*(.*)$
    // Replace with: "$1",

    /*
     * Text content of kernel:
     */
    // BODY3_NUT_PREC_ANGLES = ( 125.045 -1935.5364525000
    // 250.089 -3871.0729050000
    // 260.008 475263.3328725000
    // 176.625 487269.6299850000
    // 357.529 35999.0509575000
    // 311.589 964468.4993100000
    // 134.963 477198.8693250000
    // 276.617 12006.3007650000
    // 34.226 63863.5132425000
    // 15.134 -5806.6093575000
    // 119.743 131.8406400000
    // 239.961 6003.1503825000
    // 25.053 473327.7964200000 )
    // BODY399_POLE_RA = ( 269.9949 0.0031 0. )
    // BODY399_POLE_DEC = ( 66.5392 0.0130 0. )
    // BODY399_PM = ( 38.3213 13.17635815 -1.4D-12 )
    // BODY301_POLE_RA = ( 269.9949 0.0031 0. )
    // BODY301_POLE_DEC = ( 66.5392 0.0130 0. )
    // BODY301_PM = ( 38.3213 13.17635815 -1.4D-12 )
    // BODY301_NUT_PREC_RA = ( -3.8787 -0.1204 0.0700 -0.0172
    // 0.0 0.0072 0.0 0.0
    // 0.0 -0.0052 0.0 0.0
    // 0.0043 )
    // BODY301_NUT_PREC_DEC = ( 1.5419 0.0239 -0.0278 0.0068
    // 0.0 -0.0029 0.0009 0.0
    // 0.0 0.0008 0.0 0.0
    // -0.0009 )
    // BODY301_NUT_PREC_PM = ( 3.5610 0.1208 -0.0642 0.0158
    // 0.0252 -0.0066 -0.0047 -0.0046
    // 0.0028 0.0052 0.0040 0.0019
    // -0.0044 )
    // BODY4_CONSTANTS_REF_FRAME = 17
    // BODY4_CONSTANTS_JED_EPOCH = -36525
    // BODY4_NUT_PREC_ANGLES = ( 125.045 -1935.5364525000
    // 250.089 -3871.0729050000
    // 260.008 475263.3328725000
    // 176.625 487269.6299850000
    // 357.529 35999.0509575000
    // 311.589 964468.4993100000
    // 134.963 477198.8693250000
    // 276.617 12006.3007650000
    // 34.226 63863.5132425000
    // 15.134 -5806.6093575000
    // 119.743 131.8406400000
    // 239.961 6003.1503825000
    // 25.053 473327.7964200000 )
    // BODY499_POLE_RA = ( 269.9949 0.0031 0. )
    // BODY499_POLE_DEC = ( 66.5392 0.0130 0. )
    // BODY499_PM = ( 38.3213 13.17635815 -1.4D-12 )
    // BODY5_CONSTANTS_REF_FRAME = 19
    // BODY599_POLE_RA = ( 269.9949 0.0031 0. )
    // BODY599_POLE_DEC = ( 66.5392 0.0130 0. )
    // BODY599_PM = ( 38.3213 13.17635815 -1.4D-12 )
    // BODY701_POLE_RA = ( 269.9949 0.0031 0. )
    // BODY701_POLE_DEC = ( 66.5392 0.0130 0. )
    // BODY701_PM = ( 38.3213 13.17635815 -1.4D-12 )

    data = KernelPoolBuilder.createPool(new String[] {
        "BODY3_NUT_PREC_ANGLES = ( 125.045 -1935.5364525000", "250.089 -3871.0729050000",
        "260.008 475263.3328725000", "176.625 487269.6299850000", "357.529 35999.0509575000",
        "311.589 964468.4993100000", "134.963 477198.8693250000", "276.617 12006.3007650000",
        "34.226 63863.5132425000", "15.134 -5806.6093575000", "119.743 131.8406400000",
        "239.961 6003.1503825000", "25.053 473327.7964200000 )",
        "BODY399_POLE_RA = ( 269.9949 0.0031 0. )", "BODY399_POLE_DEC = ( 66.5392 0.0130 0. )",
        "BODY399_PM = ( 38.3213 13.17635815 -1.4D-12 )", "BODY301_POLE_RA = ( 269.9949 0.0031 0. )",
        "BODY301_POLE_DEC = ( 66.5392 0.0130 0. )", "BODY301_PM = ( 38.3213 13.17635815 -1.4D-12 )",
        "BODY301_NUT_PREC_RA = ( -3.8787 -0.1204 0.0700 -0.0172", "0.0 0.0072 0.0 0.0",
        "0.0 -0.0052 0.0 0.0", "0.0043 )", "BODY301_NUT_PREC_DEC = ( 1.5419 0.0239 -0.0278 0.0068",
        "0.0 -0.0029 0.0009 0.0", "0.0 0.0008 0.0 0.0", "-0.0009 )",
        "BODY301_NUT_PREC_PM = ( 3.5610 0.1208 -0.0642 0.0158", "0.0252 -0.0066 -0.0047 -0.0046",
        "0.0028 0.0052 0.0040 0.0019", "-0.0044 )", "BODY4_CONSTANTS_REF_FRAME = 17",
        "BODY4_CONSTANTS_JED_EPOCH = -36525", "BODY4_NUT_PREC_ANGLES = ( 125.045 -1935.5364525000",
        "250.089 -3871.0729050000", "260.008 475263.3328725000", "176.625 487269.6299850000",
        "357.529 35999.0509575000", "311.589 964468.4993100000", "134.963 477198.8693250000",
        "276.617 12006.3007650000", "34.226 63863.5132425000", "15.134 -5806.6093575000",
        "119.743 131.8406400000", "239.961 6003.1503825000", "25.053 473327.7964200000 )",
        "BODY499_POLE_RA = ( 269.9949 0.0031 0. )", "BODY499_POLE_DEC = ( 66.5392 0.0130 0. )",
        "BODY499_PM = ( 38.3213 13.17635815 -1.4D-12 )", "BODY5_CONSTANTS_REF_FRAME = 19",
        "BODY599_POLE_RA = ( 269.9949 0.0031 0. )", "BODY599_POLE_DEC = ( 66.5392 0.0130 0. )",
        "BODY599_PM = ( 38.3213 13.17635815 -1.4D-12 )", "BODY701_POLE_RA = ( 269.9949 0.0031 0. )",
        "BODY701_POLE_DEC = ( 66.5392 0.0130 0. )", "BODY701_PM = ( 38.3213 13.17635815 -1.4D-12 )",
        "BODY399_RADII = ( 123.0, 456.0, 789.0 )", "BODY-399_RADII = ( -123, -456, -789)"});
    pool = new KernelPoolValidatingRetriever(data);

    // BODY6_CONSTANTS_REF_FRAME = 'TestFailed'
    // BODY6_CONSTANTS_JED_EPOCH = 'TestFailed'
    // BODY7_NUT_PREC_ANGLES = ( 125.045 -1935.5364525000
    // 250.089 -3871.0729050000
    // 260.008 475263.3328725000
    // 176.625 487269.6299850000
    // 357.529 35999.0509575000
    // 311.589 964468.4993100000
    // 134.963 477198.8693250000
    // 276.617 12006.3007650000
    // 34.226 63863.5132425000
    // 15.134 -5806.6093575000
    // 119.743 131.8406400000
    // 239.961 6003.1503825000
    // 25.053 473327.7964200000 )
    // BODY701_POLE_RA = ( 269.9949 0.0031 0. )
    // BODY701_POLE_DEC = ( 66.5392 0.0130 0. )
    // BODY701_PM = ( 38.3213 13.17635815 -1.4D-12 )
    // BODY701_NUT_PREC_RA = ( -3.8787 -0.1204 0.0700 -0.0172
    // 0.0 0.0072 0.0 0.0
    // 0.0 -0.0052 0.0 0.0
    // 0.0043 )
    // BODY701_NUT_PREC_DEC = ( 1.5419 0.0239 -0.0278 0.0068
    // 0.0 -0.0029 0.0009 0.0
    // 0.0 0.0008 0.0 0.0
    // -0.0009 )
    // BODY701_NUT_PREC_PM = ( 3.5610 0.1208 -0.0642 0.0158
    // 0.0252 -0.0066 -0.0047 -0.0046
    // 0.0028 0.0052 0.0040 0.0019
    // -0.0044 )

    exceptionData = KernelPoolBuilder.createPool(new String[] {
        "BODY6_CONSTANTS_REF_FRAME = 'TestFailed'", "BODY6_CONSTANTS_JED_EPOCH = 'TestFailed'",
        "BODY7_NUT_PREC_ANGLES = ( 125.045 -1935.5364525000", "250.089 -3871.0729050000",
        "260.008 475263.3328725000", "176.625 487269.6299850000", "357.529 35999.0509575000",
        "311.589 964468.4993100000", "134.963 477198.8693250000", "276.617 12006.3007650000",
        "34.226 63863.5132425000", "15.134 -5806.6093575000", "119.743 131.8406400000",
        "239.961 6003.1503825000", "25.053 473327.7964200000 )",
        "BODY701_POLE_RA = ( 269.9949 0.0031 0. )", "BODY701_POLE_DEC = ( 66.5392 0.0130 0. )",
        "BODY701_PM = ( 38.3213 13.17635815 -1.4D-12 )",
        "BODY701_NUT_PREC_RA = ( -3.8787 -0.1204 0.0700 -0.0172", "0.0 0.0072 0.0 0.0",
        "0.0 -0.0052 0.0 0.0", "0.0043 )", "BODY701_NUT_PREC_DEC = ( 1.5419 0.0239 -0.0278 0.0068",
        "0.0 -0.0029 0.0009 0.0", "0.0 0.0008 0.0 0.0", "-0.0009 )"

    });
    exceptionPool = new KernelPoolValidatingRetriever(exceptionData);

  }

  @Test
  public void testCreateTextPCK() throws Exception {
    List<PCKFrameFunction> list = factory.createTextPCK(data).getFrameFunctions();

    Map<Integer, Integer> codeMap = new HashMap<Integer, Integer>();
    codeMap.put(701, 1);
    codeMap.put(399, 1);
    codeMap.put(301, 1);
    codeMap.put(499, 17);
    codeMap.put(599, 19);

    Set<Integer> bodyCodes = new HashSet<Integer>();

    for (PCKFrameFunction function : list) {
      bodyCodes.add(function.getBodyCode());
      assertEquals((int) codeMap.get(function.getBodyCode()), function.getReferenceCode());
    }

    assertTrue(codeMap.keySet().containsAll(bodyCodes));
    assertTrue(bodyCodes.containsAll(codeMap.keySet()));
  }

  @Test
  public void testCreateFrameFunction() throws Exception {
    PCKFrameFunction function = factory.createFrameFunction(701, pool);
    assertEquals(PCKFrameFunction.class, function.getClass());

    RotationMatrixIJK expected1200 = new RotationMatrixIJK(0.98496948835712850,
        -0.17272841630541738, -3.47292216906218391E-005, 0.15843631397773189, 0.90355000449932810,
        -0.39811471183870339, 6.87971032722016484E-002, 0.39212534165734136, 0.91733563923542583);

    RotationMatrixIJK matrix = new RotationMatrixIJK();

    function.getTransform(FundamentalPhysicalConstants.SECONDS_PER_DAY * 1200.0, matrix);

    assertRotationAngleEquals(expected1200, matrix, MAT_TOLERANCE);

    RotationMatrixIJK expectedMinus12000 = new RotationMatrixIJK(0.78820075599655703,
        0.61541820450678597, -4.25217589651319001E-005, -0.56453830661706661, 0.72300849255016053,
        -0.39818992963443889, -0.24502258795545981, 0.31387760872988107, 0.91730299145354399);

    function.getTransform(FundamentalPhysicalConstants.SECONDS_PER_DAY * -12000.0, matrix);

    assertRotationAngleEquals(expectedMinus12000, matrix, MAT_TOLERANCE);

  }

  @Test
  public void testCreateFrameFunctionWithNutation() throws Exception {

    PCKFrameFunction function = factory.createFrameFunction(301, pool);
    assertEquals(PCKWithNutationFrameFunction.class, function.getClass());

    RotationMatrixIJK expected0 = new RotationMatrixIJK(0.78422705209191690, -0.62006191525085586,
        -2.26086714041824934E-002, 0.55784711246016394, 0.72055666546681307, -0.41183090094261288,
        0.27165148607559469, 0.31035675134719964, 0.91097977859342927);

    RotationMatrixIJK actual = new RotationMatrixIJK();

    function.getTransform(0.0, actual);

    assertRotationAngleEquals(expected0, actual, MAT_TOLERANCE);

    RotationMatrixIJK expected20000 = new RotationMatrixIJK(0.70130621049221653,
        -0.71268841660683824, -1.56467236022299026E-002, 0.64307643209032972, 0.64197246069382063,
        -0.41752133143193121, 0.30760738225052159, 0.28274826355816690, 0.90853239779317874);

    function.getTransform(20000.0 * FundamentalPhysicalConstants.SECONDS_PER_DAY, actual);

    assertRotationAngleEquals(expected20000, actual, MAT_TOLERANCE);

  }

  @Test
  public void testCreateKeyword() {
    assertEquals("BODY301_NUT_PREC_PM", factory.createKeyword(301, "NUT_PREC_PM"));
  }

  @Test
  public void testComputeBarycenter() {

    /*
     * Standard planet barycenters
     */
    assertEquals(1, factory.computeBarycenter(1));
    assertEquals(10, factory.computeBarycenter(10));

    /*
     * Standard moon/satellite barycenters
     */
    assertEquals(1, factory.computeBarycenter(100));
    assertEquals(1, factory.computeBarycenter(101));
    assertEquals(9, factory.computeBarycenter(999));

    /*
     * Extended moon/satellite barycenters
     */
    assertEquals(1, factory.computeBarycenter(10000));
    assertEquals(1, factory.computeBarycenter(19999));
    assertEquals(9, factory.computeBarycenter(99999));

    assertEquals(-10000, factory.computeBarycenter(-10000));
    assertEquals(100000, factory.computeBarycenter(100000));
  }

  @Test
  public void testGetReferenceFrame() throws Exception {
    assertEquals(1, factory.getReferenceFrame(3, pool));
    assertEquals(17, factory.getReferenceFrame(4, pool));
    assertEquals(19, factory.getReferenceFrame(5, pool));
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetReferenceFrameException() throws Exception {
    factory.getReferenceFrame(6, exceptionPool);
  }

  @Test
  public void testCreateConverter() throws Exception {
    assertSame(TimeConverter.NO_CONVERSION, factory.createConverter(3, pool));
    TimeConverter converter = factory.createConverter(4, pool);
    assertEquals(JEDOffsetConverter.class, converter.getClass());
    assertEquals(
        (FundamentalPhysicalConstants.JULIAN_DATE_OF_J2000 + 36525.0)
            * FundamentalPhysicalConstants.SECONDS_PER_DAY,
        converter.computeEvaluationTime(0), 0.0);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testCreateConverterException() throws Exception {
    factory.createConverter(6, exceptionPool);
  }

  @Test
  public void testIdentifyBodyCodesForFrames() {
    Set<Integer> result = new HashSet<Integer>();
    result.add(301);
    result.add(399);
    result.add(499);
    result.add(599);
    result.add(701);
    assertEquals(result, factory.identifyBodyCodesForFrames(data.getKeywords()));
  }

  @Test
  public void testIdentifyBodyCodesForEllipsoidalShapes() {
    Set<Integer> result = new HashSet<>();
    result.add(399);
    result.add(-399);
    assertEquals(result, factory.identifyBodyCodesForEllipsoidalShapes(data.getKeywords()));
  }

  @Test
  public void testCreateArray() throws Exception {
    double[] expected = new double[] {38.3213, 13.17635815, -1.4E-12};
    double[] result = factory.createArray(pool.getDoubles("BODY399_PM"));

    for (int i = 0; i < expected.length; i++) {
      assertEquals(expected[i], result[i], 0.0);
    }
  }

  @Test
  public void testCreate2DArray() throws Exception {
    double[][] expected = new double[][] {{0, 2, 4}, {1, 3, 5}};
    List<Double> values = new ArrayList<Double>();
    values.add(0.0);
    values.add(1.0);
    values.add(2.0);
    values.add(3.0);
    values.add(4.0);
    values.add(5.0);
    values.add(6.0);
    double[][] result = factory.create2DArray(values);

    for (int i = 0; i < expected[0].length; i++) {
      assertEquals(expected[0][i], result[0][i], 0.0);
      assertEquals(expected[1][i], result[1][i], 0.0);
    }
  }

  @Test
  public void testFetchAndCreateAbsentCase() throws Exception {
    assertEquals(0, factory.fetchAndCreate("NOTPRESENT", 9, pool).length);
  }

  @Test
  public void testFetchAndCreate() throws Exception {
    double[] expected = new double[] {38.3213, 13.17635815, -1.4E-12};
    double[] result = factory.fetchAndCreate("BODY399_PM", 5, pool);

    for (int i = 0; i < expected.length; i++) {
      assertEquals(expected[i], result[i], 0.0);
    }
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testFetchAndCreateException() throws Exception {
    factory.fetchAndCreate("BODY399_PM", 1, pool);
  }

}
