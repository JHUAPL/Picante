package picante.spice.kernel.tk.pck;

import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertRotationAngleEquals;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.StateTransform;
import picante.units.FundamentalPhysicalConstants;

/**
 * PCK nutation frame function test.
 * 
 * TODO: Add actual, non-NAIF validation specific tests that exercise how the polynomials are
 * evaluated.
 */
public class PCKWithNutationFrameFunctionTest {

  private final static double MAT_TOLERANCE = 1E-15;

  private PCKWithNutationFrameFunction iauMoon;

  @Before
  public void setUp() throws Exception {

    /*
     * For reference this is the J2000 to IAU_MOON conversion function from the NAIF PCK:
     * pck00009.tpc.
     */
    iauMoon = new PCKWithNutationFrameFunction(TimeConverter.NO_CONVERSION, 1, 301,
        new double[] {269.9949, 0.0031, 0.}, new double[] {66.5392, 0.0130, 0.},
        new double[] {38.3213, 13.17635815, -1.4E-12},
        new double[][] {
            {125.045, 250.089, 260.008, 176.625, 357.529, 311.589, 134.963, 276.617, 34.226, 15.134,
                119.743, 239.961, 25.053},
            {-1935.5364525000, -3871.0729050000, 475263.3328725000, 487269.6299850000,
                35999.0509575000, 964468.4993100000, 477198.8693250000, 12006.3007650000,
                63863.5132425000, -5806.6093575000, 131.8406400000, 6003.1503825000,
                473327.7964200000}},
        new double[] {-3.8787, -0.1204, 0.0700, -0.0172, 0.0, 0.0072, 0.0, 0.0, 0.0, -0.0052, 0.0,
            0.0, 0.0043},
        new double[] {1.5419, 0.0239, -0.0278, 0.0068, 0.0, -0.0029, 0.0009, 0.0, 0.0, 0.0008, 0.0,
            0.0, -0.0009},
        new double[] {3.5610, 0.1208, -0.0642, 0.0158, 0.0252, -0.0066, -0.0047, -0.0046, 0.0028,
            0.0052, 0.0040, 0.0019, -0.0044});
  }

  @Test
  public void testGetTransform() {

    RotationMatrixIJK expected0 = new RotationMatrixIJK(0.78422705209191690, -0.62006191525085586,
        -2.26086714041824934E-002, 0.55784711246016394, 0.72055666546681307, -0.41183090094261288,
        0.27165148607559469, 0.31035675134719964, 0.91097977859342927);

    RotationMatrixIJK actual = new RotationMatrixIJK();

    iauMoon.getTransform(0.0, actual);

    assertRotationAngleEquals(expected0, actual, MAT_TOLERANCE);

    RotationMatrixIJK expected20000 = new RotationMatrixIJK(0.70130621049221653,
        -0.71268841660683824, -1.56467236022299026E-002, 0.64307643209032972, 0.64197246069382063,
        -0.41752133143193121, 0.30760738225052159, 0.28274826355816690, 0.90853239779317874);

    iauMoon.getTransform(20000.0 * FundamentalPhysicalConstants.SECONDS_PER_DAY, actual);

    assertRotationAngleEquals(expected20000, actual, MAT_TOLERANCE);
  }

  @Test
  public void testGetStateTransform() {

    StateTransform expected0 = new StateTransform(
        new RotationMatrixIJK(0.78422705209191690, -0.62006191525085586, -2.26086714041824934E-002,
            0.55784711246016394, 0.72055666546681307, -0.41183090094261288, 0.27165148607559469,
            0.31035675134719964, 0.91097977859342927),
        new MatrixIJK(-1.65057825769955067E-006, -2.08758320158425800E-006,
            1.20172347550938789E-010, 1.91778757371194920E-006, -1.48539156715740753E-006,
            -1.15711141436875670E-009, 8.26779430586423243E-007, -7.22143284151260954E-007,
            -5.20118349810575381E-010));

    StateTransform actual = new StateTransform();

    iauMoon.getStateTransform(0.0, actual);

    assertComponentEquals(expected0, actual, MAT_TOLERANCE);

    StateTransform expected20000 = new StateTransform(
        new RotationMatrixIJK(0.70130621049221653, -0.71268841660683824, -1.56467236022299026E-002,
            0.64307643209032972, 0.64197246069382063, -0.41752133143193121, 0.30760738225052159,
            0.28274826355816690, 0.90853239779317874),
        new MatrixIJK(-1.89693165069554538E-006, -1.86661784060647238E-006,
            -8.33035750278296996E-010, 1.70826012351925734E-006, -1.71157403555257909E-006,
            -5.78581093243131554E-010, 7.53519374279240550E-007, -8.18868045571319940E-007,
            -2.80236818352353574E-010));

    iauMoon.getStateTransform(20000.0 * FundamentalPhysicalConstants.SECONDS_PER_DAY, actual);

    assertComponentEquals(expected20000, actual, MAT_TOLERANCE);

  }

}
