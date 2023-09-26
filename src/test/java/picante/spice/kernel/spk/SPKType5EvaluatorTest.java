package picante.spice.kernel.spk;

import static org.junit.Assert.assertSame;
import static picante.junit.AssertTools.assertComponentEquals;
import org.junit.Test;
import picante.mechanics.StateVector;

public class SPKType5EvaluatorTest {

  private static final double TOL = 1.0e-16;

  @Test
  public void testUseBoth() {

    SPKType5Evaluator evaluator = new SPKType5Evaluator();

    StateVector left = new StateVector(-1.8276966996656E+09, -5.9817694199201E+09,
        -1.7044851611248E+09, 4.3758733772182E+00, -1.1068356354252E+00, -2.1045070068570E-01);
    StateVector right = new StateVector(-1.8273514451565E+09, -5.9818567389886E+09,
        -1.7045017628728E+09, 4.3759437036600E+00, -1.1066054499009E+00, -2.1038511116766E-01);
    evaluator.configure(1.3289051279402E+11, 0.0, left, 86400.0, right);
    StateVector buffer = new StateVector();

    StateVector result = evaluator.evaluate(0.0, buffer);
    assertSame(buffer, result);
    assertComponentEquals(left, result, TOL);

    result = evaluator.evaluate(86400.0, buffer);
    assertSame(buffer, result);
    assertComponentEquals(right, result, TOL);

    result = evaluator.evaluate(10000.0, buffer);
    assertSame(buffer, result);
    assertComponentEquals(new StateVector(-1827654013.9138300, -5981780216.7234869,
        -1704487213.9860592, 4.1636464421117108, -1.0531251362653662, -0.20023571842160254), result,
        TOL);

    result = evaluator.evaluate(76400.0, buffer);
    assertSame(buffer, result);
    assertComponentEquals(new StateVector(-1827394131.5078788, -5981845944.1477718,
        -1704499710.5707529, 4.1636965067363647, -1.0529612732883284, -0.20018902757810350), result,
        TOL);

  }

  @Test
  public void testUseOne() {

    SPKType5Evaluator evaluator = new SPKType5Evaluator();

    StateVector left = new StateVector(-1.8276966996656E+09, -5.9817694199201E+09,
        -1.7044851611248E+09, 4.3758733772182E+00, -1.1068356354252E+00, -2.1045070068570E-01);
    evaluator.configure(1.3289051279402E+11, 0.0, left, 0.0, left);

    StateVector buffer = new StateVector();

    StateVector result = evaluator.evaluate(0.0, buffer);
    assertSame(buffer, result);
    assertComponentEquals(left, result, TOL);

    result = evaluator.evaluate(1000.0, buffer);
    assertSame(buffer, result);
    assertComponentEquals(new StateVector(-1827692323.7917769, -5981770526.7542763,
        -1704485371.5750849, 4.3758742686748029, -1.1068327178214608, -0.21044986932431223), result,
        TOL);

  }

}
