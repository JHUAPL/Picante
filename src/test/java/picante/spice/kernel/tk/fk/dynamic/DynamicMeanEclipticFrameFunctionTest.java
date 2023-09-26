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

public class DynamicMeanEclipticFrameFunctionTest extends DynamicFrameFunctionTest {

  private static StateTransformFunction eclipj2000STF;
  private static FrameID j2000 = CelestialFrames.J2000;
  private final static double tol = 1E-15;
  private final static double et_0 = 0;
  private final static double et_1E8 = 1E8;
  private final static double et_1E9 = 1E9;

  @BeforeClass
  public static void setup() throws AdapterInstantiationException, KernelInstantiationException,
      ParseException, IOException {

    SpiceEnvironment se = SpiceEnvironmentSetup.createDefault();
    EphemerisAndFrameProvider provider =
        new LockableEphemerisProvider(se.getEphemerisSources(), se.getFrameSources());

    eclipj2000STF = provider.createStateTransformFunction(
        SpiceEnvironmentSetup.DynFrames.MEANECLIPTIC, j2000, Coverage.ALL_TIME);
  }

  @Test
  public void eclip0test() {
    StateTransform result = eclipj2000STF.getStateTransform(et_0, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(1.0000000000000000E+00, 0.0000000000000000E+00,
        0.0000000000000000E+00, 0.0000000000000000E+00, 9.1748206206918181E-01,
        3.9777715593191371E-01, 0.0000000000000000E+00, -3.9777715593191371E-01,
        9.1748206206918181E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(0.0000000000000000E+00, -7.0860020185466574E-12, -3.0791864575156765E-12,
            7.7261097754572220E-12, 2.8608544678329320E-14, -6.5986259323460641E-14,
            6.4486106716927941E-15, 6.5986259323460641E-14, 2.8608544678329320E-14);
    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertComponentRelativeEquality(expected, result, tol);
  }

  @Test
  public void eclip1E8test() {
    StateTransform result = eclipj2000STF.getStateTransform(et_1E8, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(9.9999970153176743E-01,
        -7.0860693005184595E-04, -3.0791653809533937E-04, 7.7261630754370898E-04,
        9.1748464916051231E-01, 3.9777043834208270E-01, 6.4580774616180506E-07,
        -3.9777055752158175E-01, 9.1748492280176064E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(-5.9694061317928805E-15, -7.0861358773442795E-12, -3.0791439979298719E-12,
            7.7262156066866082E-12, 2.3133238037207056E-14, -6.8365540363388332E-14,
            6.4675439748425412E-15, 6.5981946823866428E-14, 2.8606106700425386E-14);

    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertRotationAngleEquals(expected.getRotation(), result.getRotation(), tol);
    AssertTools.assertComponentEquals(expected.getRotationDerivative(),
        result.getRotationDerivative(), tol);
  }

  @Test
  public void eclip1E9test() {
    StateTransform result = eclipj2000STF.getStateTransform(et_1E9, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(9.9997014956344754E-01,
        -7.0866113627837558E-03, -3.0789481076163765E-03, 7.7265735771015302E-03,
        9.1748329045958577E-01, 3.9769927305466490E-01, 6.5432535301594534E-06,
        -3.9771119127684118E-01, 9.1751065840694279E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(-5.9704754309153784E-14, -7.0871501651716712E-12, -3.0786791055246035E-12,
            7.7269604782970106E-12, -2.6155908653990950E-14, -8.9779695225104686E-14,
            6.6378684107422939E-15, 6.5943001677266436E-14, 2.8584110800104024E-14);

    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertRotationAngleEquals(expected.getRotation(), result.getRotation(), tol);
    AssertTools.assertComponentEquals(expected.getRotationDerivative(),
        result.getRotationDerivative(), tol);
  }



  @Override
  public DynamicFrameFunction create() {
    return new DynamicMeanEclipticFrameFunction(1, 1);
  }

}
