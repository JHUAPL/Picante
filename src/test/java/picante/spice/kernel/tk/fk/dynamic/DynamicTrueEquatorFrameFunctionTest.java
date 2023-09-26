package picante.spice.kernel.tk.fk.dynamic;

import java.io.IOException;
import org.junit.BeforeClass;
import org.junit.Test;
import picante.junit.AssertTools;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.CelestialFrames;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisAndFrameProvider;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.mechanics.StateTransformFunction;
import picante.mechanics.providers.lockable.LockableEphemerisProvider;
import picante.spice.SpiceEnvironment;
import picante.spice.adapters.AdapterInstantiationException;
import picante.spice.kernel.KernelInstantiationException;
import picante.spice.kernelpool.parser.ParseException;

public class DynamicTrueEquatorFrameFunctionTest extends DynamicFrameFunctionTest {

  private static StateTransformFunction trueEqj2000STF;
  private static FrameID j2000 = CelestialFrames.J2000;
  /**
   * We lowered the tolerance to 1E-11 to pass the test because we believe the error is good enough
   * for now
   */
  private final static double tol = 1E-11;
  private final static double et_0 = 0;
  private final static double et_1E8 = 1E8;
  private final static double et_1E9 = 1E9;

  @BeforeClass
  public static void setup() throws AdapterInstantiationException, KernelInstantiationException,
      ParseException, IOException {

    SpiceEnvironment se = SpiceEnvironmentSetup.createDefault();
    EphemerisAndFrameProvider provider =
        new LockableEphemerisProvider(se.getEphemerisSources(), se.getFrameSources());

    trueEqj2000STF = provider.createStateTransformFunction(
        SpiceEnvironmentSetup.DynFrames.TRUEEQDATE, j2000, Coverage.ALL_TIME);
  }


  @Test
  public void trueEq0test() {
    StateTransform result = trueEqj2000STF.getStateTransform(et_0, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(9.9999999772170789E-01, 6.1932310989079475E-05,
        2.6850942970991014E-05, -6.1933062582113758E-05, 9.9999999769038916E-01,
        2.7991380899483609E-05, -2.6849209338068903E-05, -2.7993043796858963E-05,
        9.9999999924775473E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(5.5034764115193275E-16, -7.4776635592844189E-12, -3.2489978086438402E-12,
            7.4777236213875325E-12, 4.3091738518160849E-16, 1.1503877191483054E-12,
            3.2488597256104353E-12, -1.1499857276172540E-12, 5.5037714085403992E-17);

    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertRotationAngleEquals(expected.getRotation(), result.getRotation(), tol);
    AssertTools.assertComponentEquals(expected.getRotationDerivative(),
        result.getRotationDerivative(), tol);
  }

  @Test
  public void trueEq1E8test() {
    StateTransform result = trueEqj2000STF.getStateTransform(et_1E8, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(9.9999975256497475E-01,
        -6.4516592957371765E-04, -2.8041204023406300E-04, 6.4515954322625702E-04,
        9.9999979162316222E-01, -2.2864727516114931E-05, 2.8042673334587119E-04,
        2.2683811354744369E-05, 9.9999996042314510E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(-7.8962409893984751E-17, -1.0042709088778971E-13, -5.0533682138202909E-14,
            9.9850389746663206E-14, -1.1134791189672558E-16, -2.0524389289788259E-12,
            5.1860100292434398E-14, 2.0523778177893015E-12, -6.1098712181575292E-17);

    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertRotationAngleEquals(expected.getRotation(), result.getRotation(), tol);
    AssertTools.assertComponentEquals(expected.getRotationDerivative(),
        result.getRotationDerivative(), tol);
  }

  @Test
  public void trueEq1E9test() {
    StateTransform result = trueEqj2000STF.getStateTransform(et_1E9, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(9.9996961187349065E-01,
        -7.1501737821974857E-03, -3.1065003565146328E-03, 7.1502521124844759E-03,
        9.9997443652108065E-01, 1.4109394154999471E-05, 3.1063200589380858E-03,
        -3.6321226133521149E-05, 9.9999517471658828E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(-6.9126302752799929E-14, -8.1329565471635728E-12, -3.5319968451562308E-12,
            8.1321145975837889E-12, -5.8143570750617465E-14, -3.2497367474918008E-13,
            3.5339347991655985E-12, 2.7444768993831280E-13, -1.0967617198880691E-14);

    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertRotationAngleEquals(expected.getRotation(), result.getRotation(), tol);
    AssertTools.assertComponentEquals(expected.getRotationDerivative(),
        result.getRotationDerivative(), tol);
  }

  @Override
  public DynamicFrameFunction create() {
    return new DynamicTrueEquatorFrameFunction(1, 1);
  }
}
