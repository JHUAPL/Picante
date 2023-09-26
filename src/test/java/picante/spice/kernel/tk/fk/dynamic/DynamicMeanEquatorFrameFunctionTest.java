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

public class DynamicMeanEquatorFrameFunctionTest extends DynamicFrameFunctionTest {

  private static StateTransformFunction equatj2000STF;
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

    equatj2000STF = provider.createStateTransformFunction(SpiceEnvironmentSetup.DynFrames.EQUATDATE,
        j2000, Coverage.ALL_TIME);
  }

  @Test
  public void meanEq0test() {
    StateTransform result = equatj2000STF.getStateTransform(et_0, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(1.0000000000000000E+00,
        -0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,
        1.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,
        0.0000000000000000E+00, 1.0000000000000000E+00);
    MatrixIJK dRotMat =
        new MatrixIJK(0.0000000000000000E+00, -7.0860020185466574E-12, -3.0791864575156765E-12,
            7.0860020185466574E-12, 0.0000000000000000E+00, 0.0000000000000000E+00,
            3.0791864575156765E-12, 0.0000000000000000E+00, 0.0000000000000000E+00);
    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertComponentRelativeEquality(expected, result, tol);
  }

  @Test
  public void meanEq1E8test() {
    StateTransform result = equatj2000STF.getStateTransform(et_1E8, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(9.9999970153176743E-01,
        -7.0860693005184595E-04, -3.0791653809533937E-04, 7.0860693005166293E-04,
        9.9999974893807175E-01, -1.0909650687302154E-07, 3.0791653809576048E-04,
        -1.0909531846069443E-07, 9.9999995259369567E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(-5.9694061317928805E-15, -7.0861358773442795E-12, -3.0791439979298719E-12,
            7.0861358773369609E-12, -5.0212864886644915E-15, -2.1819390791241935E-15,
            3.0791439979467140E-12, -2.1819034267395326E-15, -9.4811964312838902E-16);
    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertComponentRelativeEquality(expected, result, tol);
  }

  @Test
  public void meanEq1E9test() {
    StateTransform result = equatj2000STF.getStateTransform(et_1E9, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(9.9997014956344754E-01,
        -7.0866113627837558E-03, -3.0789481076163765E-03, 7.0866113609541924E-03,
        9.9997488959492464E-01, -1.0910411367933706E-05, 3.0789481118273605E-03,
        -1.0909222951831218E-05, 9.9999525996852279E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(-5.9704754309153784E-14, -7.0871501651716712E-12, -3.0786791055246035E-12,
            7.0871501578535349E-12, -5.0225378087861589E-14, -2.1821618961495205E-14,
            3.0786791223689645E-12, -2.1818053719646403E-14, -9.4793762212943138E-15);
    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertComponentRelativeEquality(expected, result, tol);
  }

  @Override
  public DynamicFrameFunction create() {
    return new DynamicMeanEquatorFrameFunction(1, 1);
  }

}
