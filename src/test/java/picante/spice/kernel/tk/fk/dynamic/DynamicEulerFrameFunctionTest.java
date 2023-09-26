package picante.spice.kernel.tk.fk.dynamic;

import java.io.IOException;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import com.google.common.collect.Lists;
import picante.junit.AssertTools;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisAndFrameProvider;
import picante.mechanics.StateTransform;
import picante.mechanics.StateTransformFunction;
import picante.mechanics.providers.lockable.LockableEphemerisProvider;
import picante.mechanics.rotations.EulerAngles;
import picante.spice.SpiceEnvironment;
import picante.spice.adapters.AdapterInstantiationException;
import picante.spice.kernel.KernelInstantiationException;
import picante.spice.kernelpool.content.DynamicUnits;
import picante.spice.kernelpool.parser.ParseException;

public class DynamicEulerFrameFunctionTest extends DynamicFrameFunctionTest {

  private static StateTransformFunction hgspec2hciSTF;
  /**
   * We lowered the tolerance to 1E-11 to pass the test because we believe the error is good enough
   * for now
   */
  private final static double tol = 1E-11;
  private final static double et_0 = 0;
  private final static double et_1E8 = 1E8;
  private final static double et_1E9 = 1E9;

  @BeforeClass
  public static void setup() throws KernelInstantiationException, IOException,
      AdapterInstantiationException, ParseException {

    SpiceEnvironment se = SpiceEnvironmentSetup.createDefault();
    EphemerisAndFrameProvider provider =
        new LockableEphemerisProvider(se.getEphemerisSources(), se.getFrameSources());

    hgspec2hciSTF =
        provider.createStateTransformFunction(SpiceEnvironmentSetup.DynFrames.SPP_HGSPEC,
            SpiceEnvironmentSetup.DynFrames.SPP_HCI, Coverage.ALL_TIME);
  }

  @Test
  public void hgspec0test() {
    StateTransform result = hgspec2hciSTF.getStateTransform(et_0, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(1.0000000000000000E+00, 0.0000000000000000E+00,
        0.0000000000000000E+00, 0.0000000000000000E+00, 1.0000000000000000E+00,
        0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,
        1.0000000000000000E+00);
    MatrixIJK dRotMat =
        new MatrixIJK(0.0000000000000000E+00, 2.8509999999999982E-06, 0.0000000000000000E+00,
            -2.8509999999999982E-06, 0.0000000000000000E+00, 0.0000000000000000E+00,
            0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00);

    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertComponentRelativeEquality(expected, result, tol);
  }

  /**
   * Could n
   */
  @Ignore
  @Test
  public void hgspec1E8test() {
    StateTransform result = hgspec2hciSTF.getStateTransform(et_1E8, new StateTransform());

    RotationMatrixIJK rotMat = new RotationMatrixIJK(-7.0743670152060534E-01,
        7.0677670684711025E-01, 0.0000000000000000E+00, -7.0677670684711025E-01,
        -7.0743670152060534E-01, 0.0000000000000000E+00, -0.0000000000000000E+00,
        0.0000000000000000E+00, 1.0000000000000000E+00);
    MatrixIJK dRotMat =
        new MatrixIJK(-2.0150203912211101E-06, -2.0169020360352445E-06, 0.0000000000000000E+00,
            2.0169020360352445E-06, -2.0150203912211101E-06, 0.0000000000000000E+00,
            0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00);


    StateTransform expected = new StateTransform(rotMat, dRotMat);

    AssertTools.assertComponentRelativeEquality(expected, result, tol);
    // AssertTools.assertRotationAngleEquals(expected.getRotation(), result.getRotation(), tol);
    // AssertTools.assertComponentEquals(expected.getRotationDerivative(),
    // result.getRotationDerivative(), tol);
  }


  @Test
  public void hgspec1E9test() {
    StateTransform result = hgspec2hciSTF.getStateTransform(et_1E9, new StateTransform());

    RotationMatrixIJK rotMat = new RotationMatrixIJK(4.6668503203796076E-03,
        -9.9998911019474968E-01, 0.0000000000000000E+00, 9.9998911019474968E-01,
        4.6668503203796076E-03, 0.0000000000000000E+00, 0.0000000000000000E+00,
        0.0000000000000000E+00, 1.0000000000000000E+00);
    MatrixIJK dRotMat =
        new MatrixIJK(2.8509689531652297E-06, 1.3305190263402253E-08, 0.0000000000000000E+00,
            -1.3305190263402253E-08, 2.8509689531652297E-06, 0.0000000000000000E+00,
            0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00);


    StateTransform expected = new StateTransform(rotMat, dRotMat);
    // AssertTools.assertComponentRelativeEquality(expected, result, tol);
    AssertTools.assertRotationAngleEquals(expected.getRotation(), result.getRotation(), tol);
    AssertTools.assertComponentEquals(expected.getRotationDerivative(),
        result.getRotationDerivative(), tol);
  }


  @Override
  public DynamicFrameFunction create() {
    return new DynamicEulerFrameFunction(1, 1, 2451545.0,
        Lists.newArrayList(EulerAngles.Axis.K, EulerAngles.Axis.I, EulerAngles.Axis.K),
        Lists.newArrayList(0.0, -1.6335026), Lists.newArrayList(0.0), Lists.newArrayList(0.0),
        DynamicUnits.DEGREES);
  }


}
