package picante.spice.kernel.spk;

import static org.junit.Assert.assertSame;
import static picante.junit.AssertTools.assertComponentRelativeEquality;
import static picante.junit.AssertTools.assertRelativeEquality;
import org.junit.Test;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;

public class EquinoctialElementsTest {

  private static final double TOLERANCE = 1e-13;

  @Test
  public void testEvaluatePosition() {

    EquinoctialElements elements =
        new EquinoctialElements(0.0, 133584.4767617734, 6.573230396213189E-7, -3.834742374741285E-6,
            2.5585308839092624, 2.874273755616854E-6, 1.78617982824953E-6, 6.478082519544579E-7,
            1.2646197899315496E-4, -6.445237364989953E-7, 0.7083010399471831, 1.4580066155875588);

    VectorIJK buffer = new VectorIJK();
    VectorIJK result = elements.evaluate(0, buffer);
    assertSame(result, buffer);
    assertComponentRelativeEquality(
        new VectorIJK(17040.850849408249, -132233.70969392639, 8278.9304839369761), result,
        TOLERANCE);

    result = elements.evaluate(-301320000.0, buffer);
    assertSame(result, buffer);
    assertComponentRelativeEquality(
        new VectorIJK(113259.62343003204, 69258.088976055369, -14847.395210639823), result,
        TOLERANCE);

    result = elements.evaluate(569419200.0, buffer);
    assertSame(result, buffer);
    assertComponentRelativeEquality(
        new VectorIJK(-133089.95790701272, 1999.8513111718894, 11302.344109040767), result,
        TOLERANCE);

  }

  @Test
  public void testEvaluateState() {

    EquinoctialElements elements =
        new EquinoctialElements(0.0, 133584.4767617734, 6.573230396213189E-7, -3.834742374741285E-6,
            2.5585308839092624, 2.874273755616854E-6, 1.78617982824953E-6, 6.478082519544579E-7,
            1.2646197899315496E-4, -6.445237364989953E-7, 0.7083010399471831, 1.4580066155875588);

    StateVector buffer = new StateVector();
    StateVector result = elements.evaluate(0, buffer);
    assertSame(result, buffer);
    assertComponentRelativeEquality(new StateVector(17040.850849408249, -132233.70969392639,
        8278.9304839369761, 16.693054907553208, 2.0518738333261366, -1.5872193179875826), result,
        TOLERANCE);

    result = elements.evaluate(-301320000.0, buffer);
    assertSame(result, buffer);
    assertComponentRelativeEquality(new StateVector(113259.62343003204, 69258.088976055369,
        -14847.395210639823, -8.8402685758630586, 14.392504306442476, -0.30012574178285273), result,
        TOLERANCE);

    result = elements.evaluate(569419200.0, buffer);
    assertSame(result, buffer);
    assertComponentRelativeEquality(new StateVector(-133089.95790701272, 1999.8513111718894,
        11302.344109040767, -0.14663464451318756, -16.846183869801060, 1.2538802016067592), result,
        TOLERANCE);

  }

  @Test
  public void testSolveKeplersEquation() {

    assertRelativeEquality(0.0, EquinoctialElements.solveKeplersEquation(0.0, 1.0), 0.0);
    assertRelativeEquality(0.69261877956204621, EquinoctialElements.solveKeplersEquation(0.9, 0.0),
        TOLERANCE);
    assertRelativeEquality(0.63020676436025636,
        EquinoctialElements.solveKeplersEquation(Math.sqrt(1.0 / 2.0), 0.1), TOLERANCE);
    assertRelativeEquality(0.15890799174349121,
        EquinoctialElements.solveKeplersEquation(0.123415126, 0.23412412412), TOLERANCE);

  }

}
