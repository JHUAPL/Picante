package picante.mechanics.providers.aberrated;

import static org.junit.Assert.assertSame;
import static picante.junit.AssertTools.assertComponentRelativeEquality;
import org.junit.Test;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;

public class InertialStellarAberratedStateVectorFunctionTest {

  private final static double TOL = 1.0E-14;

  /**
   * Hacked up some SPICE code to pull intermediate values from forced to pass through the numerical
   * derivative path in ZZSTELAB. This test confirms that the receipt case works.
   */
  @Test
  public void testNumericalDerivativeReceiptCase() {

    VectorIJK lcacc =
        new VectorIJK(1.0843819016059797E-006, -5.5545570476489559E-006, -2.4052025482390604E-006);
    VectorIJK lcvobs = new VectorIJK(-29.783863764792766, -5.0353084672836834, -2.1830502887997865);
    VectorIJK ptarg = new VectorIJK(-1450879255.0830989, -4318303610.2170620, -918262771.45009398);
    VectorIJK vtarg =
        new VectorIJK(35.037538304649466, 3.0710483803040134, -1.2837821349171108E-002);
    VectorIJK rhat =
        new VectorIJK(-0.31220839707573794, -0.92923697372392478, -0.19759697229411308);
    VectorIJK vp = new VectorIJK(-25.285211980218609, 8.3541890904047875, 0.66415042829598570);
    VectorIJK vphat =
        new VectorIJK(-0.94922089145879696, 0.31362089517039632, 2.4932575692980484E-002);
    VectorIJK scorr = new VectorIJK(-391946.00429301796, 129517.20211170538, 10298.769362323023);

    StateVector buffer = new StateVector();
    StateVector result = InertialStellarAberratedStateVectorFunction.numericalDerivative(1.0,
        new double[] {-1, 1}, lcacc, lcvobs, ptarg, vtarg, rhat, vp, vphat, scorr, buffer);

    assertSame(result, buffer);

    assertComponentRelativeEquality(
        new StateVector(-391946.00429301796, 129517.20211170538, 10298.769362323023,
            4.1239257028792053E-002, -1.2371539443847723E-002, -2.1521953895899060E-002),
        buffer, TOL);
  }

  /**
   * Same as the receipt case, but the sense of the correction is inverted.
   */
  @Test
  public void testNumericalDerivativeTransmissionCase() {

    VectorIJK lcacc =
        new VectorIJK(-1.0843819016059797E-006, 5.5545570476489559E-006, 2.4052025482390604E-006);
    VectorIJK lcvobs = new VectorIJK(29.783863764792766, 5.0353084672836834, 2.1830502887997865);
    VectorIJK ptarg = new VectorIJK(-1450716384.4473479, -4318364501.5215302, -918330845.77117467);
    VectorIJK vtarg =
        new VectorIJK(35.037120870600567, 3.0714155599723538, -1.2597470206702432E-002);
    VectorIJK rhat =
        new VectorIJK(-0.31217206070824016, -0.92924623984503929, -0.19761080498552355);
    VectorIJK vp = new VectorIJK(25.286049407935614, -8.3533883595499798, -0.66415107098072745);
    VectorIJK vphat =
        new VectorIJK(0.94923295030731236, -0.31358443344296805, -2.4932090829457354E-002);
    VectorIJK scorr = new VectorIJK(391972.05826798733, -129471.23010989907, -10291.571646775332);

    StateVector buffer = new StateVector();
    StateVector result = InertialStellarAberratedStateVectorFunction.numericalDerivative(1.0,
        new double[] {-1, 1}, lcacc, lcvobs, ptarg, vtarg, rhat, vp, vphat, scorr, buffer);

    assertSame(result, buffer);

    assertComponentRelativeEquality(
        new StateVector(391972.05826798733, -129471.23010989907, -10291.571646775332,
            -4.1239786136429757E-002, 1.2362160974589642E-002, 2.1519116644412861E-002),
        buffer, TOL);

  }

}
