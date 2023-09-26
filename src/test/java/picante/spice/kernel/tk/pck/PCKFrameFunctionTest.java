package picante.spice.kernel.tk.pck;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expectLastCall;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertRotationAngleEquals;
import org.junit.Before;
import org.junit.Test;
import picante.junit.AssertTools;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.StateTransform;
import picante.units.FundamentalPhysicalConstants;

/**
 * TODO: Add explicit derivative tests.
 * 
 */
public class PCKFrameFunctionTest {

  private static final double TOLERANCE = 1.0E-15;
  private static final double MAT_TOLERANCE = 1.0E-12;

  private PCKFrameFunction constantTerm;
  private PCKFrameFunction linearTerm;
  private PCKFrameFunction quadraticTerm;

  private PCKFrameFunction iauMoon;

  @Before
  public void setUp() throws Exception {

    double[] constant = new double[] {1.0, 0.0, 0.0};
    constantTerm =
        new PCKFrameFunction(TimeConverter.NO_CONVERSION, 1, 100, constant, constant, constant);

    double[] linear = new double[] {0.0, 1.0, 0.0};
    linearTerm = new PCKFrameFunction(TimeConverter.NO_CONVERSION, 2, 10, linear, linear, linear);

    double[] quadratic = new double[] {0.0, 0.0, 1.0};
    quadraticTerm =
        new PCKFrameFunction(TimeConverter.NO_CONVERSION, 3, 1, quadratic, quadratic, quadratic);

    iauMoon = new PCKFrameFunction(TimeConverter.NO_CONVERSION, 1, 301,
        new double[] {269.9949, 0.0031, 0.}, new double[] {66.5392, 0.0130, 0.},
        new double[] {38.3213, 13.17635815, -1.4E-12});

  }

  @Test
  public void testGetReferenceCode() {
    assertEquals(1, constantTerm.getReferenceCode());
    assertEquals(2, linearTerm.getReferenceCode());
    assertEquals(3, quadraticTerm.getReferenceCode());
  }

  @Test
  public void testGetBodyCode() {
    assertEquals(100, constantTerm.getBodyCode());
    assertEquals(10, linearTerm.getBodyCode());
    assertEquals(1, quadraticTerm.getBodyCode());
  }

  @Test
  public void testComputeEpoch() {
    double et = 10.0;
    TimeConverter mock = createMock(TimeConverter.class);
    mock.computeEvaluationTime(et);
    expectLastCall().andReturn(20.0);
    replay(mock);
    double[] values = new double[] {1.0, 2.0, 3.0};
    PCKFrameFunction f = new PCKFrameFunction(mock, -10, 10, values, values, values);
    AssertTools.assertEqualDouble(20.0, f.computeEpoch(et));
    verify(mock);
  }

  private double prepareDayTime(double time) {
    return time / FundamentalPhysicalConstants.SECONDS_PER_DAY;
  }

  private double prepareCenturyTime(double time) {
    return time / PCKFrameFunction.JULIAN_SECONDS_PER_CENTURY;
  }

  @Test
  public void testConstantRA() {
    AssertTools.assertRelativeEquality(1.0, constantTerm.computeRA(0.0), TOLERANCE);
    AssertTools.assertRelativeEquality(1.0, constantTerm.computeRA(1E6), TOLERANCE);
  }

  @Test
  public void testLinearRA() {
    AssertTools.assertRelativeEquality(prepareCenturyTime(0.0), linearTerm.computeRA(0.0),
        TOLERANCE);
    AssertTools.assertRelativeEquality(prepareCenturyTime(1E6), linearTerm.computeRA(1E6),
        TOLERANCE);
  }

  @Test
  public void testQuadraticRA() {
    AssertTools.assertRelativeEquality(prepareCenturyTime(0.0) * prepareCenturyTime(0.0),
        quadraticTerm.computeRA(0.0), TOLERANCE);
    AssertTools.assertRelativeEquality(prepareCenturyTime(1E6) * prepareCenturyTime(1E6),
        quadraticTerm.computeRA(1E6), TOLERANCE);
  }

  @Test
  public void testConstantDEC() {
    AssertTools.assertRelativeEquality(1.0, constantTerm.computeDEC(0.0), TOLERANCE);
    AssertTools.assertRelativeEquality(1.0, constantTerm.computeDEC(1E6), TOLERANCE);
  }

  @Test
  public void testLinearDEC() {
    AssertTools.assertRelativeEquality(prepareCenturyTime(0.0), linearTerm.computeDEC(0.0),
        TOLERANCE);
    AssertTools.assertRelativeEquality(prepareCenturyTime(1E6), linearTerm.computeDEC(1E6),
        TOLERANCE);
  }

  @Test
  public void testQuadraticDEC() {
    AssertTools.assertRelativeEquality(prepareCenturyTime(0.0) * prepareCenturyTime(0.0),
        quadraticTerm.computeDEC(0.0), TOLERANCE);
    AssertTools.assertRelativeEquality(prepareCenturyTime(1E6) * prepareCenturyTime(1E6),
        quadraticTerm.computeDEC(1E6), TOLERANCE);
  }

  @Test
  public void testConstantW() {
    AssertTools.assertRelativeEquality(1.0, constantTerm.computeW(0.0), TOLERANCE);
    AssertTools.assertRelativeEquality(1.0, constantTerm.computeW(1E6), TOLERANCE);
  }

  @Test
  public void testLinearW() {
    AssertTools.assertRelativeEquality(prepareDayTime(0.0), linearTerm.computeW(0.0), TOLERANCE);
    AssertTools.assertRelativeEquality(prepareDayTime(1E6), linearTerm.computeW(1E6), TOLERANCE);
  }

  @Test
  public void testQuadraticW() {
    AssertTools.assertRelativeEquality(prepareDayTime(0.0) * prepareDayTime(0.0),
        quadraticTerm.computeW(0.0), TOLERANCE);
    AssertTools.assertRelativeEquality(prepareDayTime(1E6) * prepareDayTime(1E6),
        quadraticTerm.computeW(1E6), TOLERANCE);
  }

  @Test
  public void testGetTransform() {

    RotationMatrixIJK expected1200 = new RotationMatrixIJK(0.98496948835712850,
        -0.17272841630541738, -3.47292216906218391E-005, 0.15843631397773189, 0.90355000449932810,
        -0.39811471183870339, 6.87971032722016484E-002, 0.39212534165734136, 0.91733563923542583);

    RotationMatrixIJK matrix = new RotationMatrixIJK();

    RotationMatrixIJK result =
        iauMoon.getTransform(FundamentalPhysicalConstants.SECONDS_PER_DAY * 1200.0, matrix);

    assertSame(result, matrix);
    assertRotationAngleEquals(expected1200, matrix, MAT_TOLERANCE);

    RotationMatrixIJK expectedMinus12000 = new RotationMatrixIJK(0.78820075599655703,
        0.61541820450678597, -4.25217589651319001E-005, -0.56453830661706661, 0.72300849255016053,
        -0.39818992963443889, -0.24502258795545981, 0.31387760872988107, 0.91730299145354399);

    result = iauMoon.getTransform(FundamentalPhysicalConstants.SECONDS_PER_DAY * -12000.0, matrix);

    assertSame(result, matrix);
    assertRotationAngleEquals(expectedMinus12000, matrix, MAT_TOLERANCE);

  }

  @Test
  public void testGetStateTransform() {

    StateTransform expected1200 = new StateTransform(
        new RotationMatrixIJK(0.98496948835712850, -0.17272841630541738, -3.47292216906218391E-005,
            0.15843631397773189, 0.90355000449932810, -0.39811471183870339,
            6.87971032722016484E-002, 0.39212534165734136, 0.91733563923542583),
        new MatrixIJK(-4.59751134596638475E-007, -2.62169276776547887E-006,
            6.83139258767717331E-015, 2.40497857814043961E-006, -4.21709825644747857E-007,
            6.59539934701698272E-014, 1.04371979754990881E-006, -1.83117277381989635E-007,
            2.86236479112715144E-014));

    StateTransform transform = new StateTransform();

    StateTransform result =
        iauMoon.getStateTransform(FundamentalPhysicalConstants.SECONDS_PER_DAY * 1200.0, transform);

    assertSame(result, transform);
    assertComponentEquals(expected1200, transform, MAT_TOLERANCE);

    StateTransform expectedMinus12000 = new StateTransform(
        new RotationMatrixIJK(0.78820075599655703, 0.61541820450678597, -4.25217589651319001E-005,
            -0.56453830661706661, 0.72300849255016053, -0.39818992963443889, -0.24502258795545981,
            0.31387760872988107, 0.91730299145354399),
        new MatrixIJK(1.63805831500732929E-006, -2.09795354248526465E-006, 6.83397157690209491E-015,
            1.92443131328913314E-006, 1.50263134148588192E-006, 6.59515124328452192E-014,
            8.35447903632987189E-007, 6.52176439144356497E-007, 2.86290559739678108E-014));

    result = iauMoon.getStateTransform(FundamentalPhysicalConstants.SECONDS_PER_DAY * -12000.0,
        transform);

    assertSame(result, transform);
    assertComponentEquals(expectedMinus12000, transform, MAT_TOLERANCE);

  }
}
