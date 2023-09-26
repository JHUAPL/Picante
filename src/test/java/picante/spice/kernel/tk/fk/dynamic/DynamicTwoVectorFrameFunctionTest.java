package picante.spice.kernel.tk.fk.dynamic;

import static org.easymock.EasyMock.expect;
import java.io.IOException;
import org.easymock.EasyMock;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import picante.junit.AssertTools;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.CelestialFrames;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisAndFrameProvider;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.mechanics.StateTransformFunction;
import picante.mechanics.StateVector;
import picante.mechanics.providers.lockable.LockableEphemerisProvider;
import picante.spice.SpiceEnvironment;
import picante.spice.adapters.AdapterInstantiationException;
import picante.spice.kernel.KernelInstantiationException;
import picante.spice.kernel.tk.fk.FKInstantiationException;
import picante.spice.kernelpool.parser.ParseException;

public class DynamicTwoVectorFrameFunctionTest extends DynamicFrameFunctionTest {
  private static StateTransformFunction hci2j2000STF;
  private static StateTransformFunction hgi2j2000STF;
  private static StateTransformFunction heeq2j2000STF;
  private static StateTransformFunction mso2j2000STF;
  private static FrameID j2000 = CelestialFrames.J2000;
  private final static double tol = 1E-12;
  private final static double et_0 = 0;
  private final static double et_1E8 = 1E8;
  private final static double et_1E9 = 1E9;

  @BeforeClass
  public static void setup() throws AdapterInstantiationException, KernelInstantiationException,
      ParseException, IOException {

    SpiceEnvironment se = SpiceEnvironmentSetup.createDefault();
    EphemerisAndFrameProvider provider =
        new LockableEphemerisProvider(se.getEphemerisSources(), se.getFrameSources());

    hci2j2000STF = provider.createStateTransformFunction(SpiceEnvironmentSetup.DynFrames.SPP_HCI,
        j2000, Coverage.ALL_TIME);
    hgi2j2000STF = provider.createStateTransformFunction(SpiceEnvironmentSetup.DynFrames.SPP_HGI,
        j2000, Coverage.ALL_TIME);
    heeq2j2000STF = provider.createStateTransformFunction(SpiceEnvironmentSetup.DynFrames.SPP_HEEQ,
        j2000, Coverage.ALL_TIME);
    mso2j2000STF = provider.createStateTransformFunction(SpiceEnvironmentSetup.DynFrames.SPP_MSO,
        j2000, Coverage.ALL_TIME);
  }


  @Test
  public void hgi0Test() {
    StateTransform result = hgi2j2000STF.getStateTransform(et_0, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(2.4417086305312133E-01, 8.8962237270918409E-01,
        3.8594380887789309E-01, -9.6198243865000510E-01, 1.7199431971237683E-01,
        2.1215028097003727E-01, 1.2235349347232782E-01, -4.2307208364764332E-01,
        8.9779710106079025E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,
            0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,
            0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00);

    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertRotationAngleEquals(expected.getRotation(), result.getRotation(), tol);
    AssertTools.assertComponentEquals(expected.getRotationDerivative(),
        result.getRotationDerivative(), tol);
  }

  @Test
  public void hgi1E8Test() {
    StateTransform result = hgi2j2000STF.getStateTransform(et_1E8, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(2.4417086305312133E-01, 8.8962237270918409E-01,
        3.8594380887789309E-01, -9.6198243865000510E-01, 1.7199431971237683E-01,
        2.1215028097003727E-01, 1.2235349347232782E-01, -4.2307208364764332E-01,
        8.9779710106079025E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,
            0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,
            0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00);

    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertRotationAngleEquals(expected.getRotation(), result.getRotation(), tol);
    AssertTools.assertComponentEquals(expected.getRotationDerivative(),
        result.getRotationDerivative(), tol);
  }

  @Test
  public void hgi1E9Test() {
    StateTransform result = hgi2j2000STF.getStateTransform(et_1E9, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(2.4417086305312133E-01, 8.8962237270918409E-01,
        3.8594380887789309E-01, -9.6198243865000510E-01, 1.7199431971237683E-01,
        2.1215028097003727E-01, 1.2235349347232782E-01, -4.2307208364764332E-01,
        8.9779710106079025E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,
            0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,
            0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00);

    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertRotationAngleEquals(expected.getRotation(), result.getRotation(), tol);
    AssertTools.assertComponentEquals(expected.getRotationDerivative(),
        result.getRotationDerivative(), tol);
  }

  @Test
  public void hci0Test() {
    StateTransform result = hci2j2000STF.getStateTransform(et_0, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(2.4588567646795143E-01, 8.8931429511598448E-01,
        3.8556493436288763E-01, -9.6154555624942462E-01, 1.7358023084557003E-01,
        2.1283807628472792E-01, 1.2235349347232782E-01, -4.2307208364764332E-01,
        8.9779710106079025E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,
            0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,
            0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00);

    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertComponentRelativeEquality(expected, result, tol);
  }

  @Test
  public void hci1E8Test() {
    StateTransform result = hci2j2000STF.getStateTransform(et_1E8, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(2.4588567646795143E-01, 8.8931429511598448E-01,
        3.8556493436288763E-01, -9.6154555624942462E-01, 1.7358023084557003E-01,
        2.1283807628472792E-01, 1.2235349347232782E-01, -4.2307208364764332E-01,
        8.9779710106079025E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,
            0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,
            0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00);

    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertComponentRelativeEquality(expected, result, tol);
  }

  @Test
  public void hci1E9Test() {
    StateTransform result = hci2j2000STF.getStateTransform(et_1E9, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(2.4588567646795143E-01, 8.8931429511598448E-01,
        3.8556493436288763E-01, -9.6154555624942462E-01, 1.7358023084557003E-01,
        2.1283807628472792E-01, 1.2235349347232782E-01, -4.2307208364764332E-01,
        8.9779710106079025E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,
            0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,
            0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00);


    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertComponentRelativeEquality(expected, result, tol);
  }

  /**
   * TODO Temporarily ignored, but needs to be investigated 
   */
  @Test
  @Ignore
  public void mso0Test() {
    StateTransform result = mso2j2000STF.getStateTransform(et_0, new StateTransform());

    RotationMatrixIJK rotMat = new RotationMatrixIJK(2.7888865692267240E-01, 8.5877431973517515E-01,
        4.2979970312141330E-01, -9.5599259200253828E-01, 2.0581573327680405E-01,
        2.0908861272675755E-01, 9.1100390098706366E-02, -4.6919777461015155E-01,
        8.7838156129596956E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(-5.3259989622822777E-07, 1.1466348076583360E-07, 1.1648685813297548E-07,
            -1.5537366450968392E-07, -4.7843791299666919E-07, -2.3944882372977363E-07,
            -2.7741927186614012E-14, 5.9725620666592062E-15, 6.0675376775055323E-15);


    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertRotationAngleEquals(expected.getRotation(), result.getRotation(), tol);
    AssertTools.assertComponentEquals(expected.getRotationDerivative(),
        result.getRotationDerivative(), tol);
  }


  /**
   * TODO Temporarily ignored, but needs to be investigated 
   */
  @Test
  @Ignore
  public void mso1E8Test() {
    StateTransform result = mso2j2000STF.getStateTransform(et_1E8, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(-3.8752066232632520E-01,
        7.9582312628186036E-01, 4.6528839223124024E-01, -9.1734945648462973E-01,
        -3.8277474464663119E-01, -1.0933192373712462E-01, 9.1091772172466084E-02,
        -4.6920043322195726E-01, 8.7838103491981601E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(-5.6131967608814134E-07, -2.3421717226838305E-07, -6.6899434653171810E-08,
            2.3712115105012417E-07, -4.8695849348118719E-07, -2.8470683563488695E-07,
            -1.4949356304189709E-13, -6.2377930250914467E-14, -1.7817003883275212E-14);

    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertRotationAngleEquals(expected.getRotation(), result.getRotation(), tol);
    AssertTools.assertComponentEquals(expected.getRotationDerivative(),
        result.getRotationDerivative(), tol);
  }

  /**
   * TODO Temporarily ignored, but needs to be investigated 
   */
  @Test
  @Ignore
  public void mso1E9Test() {
    StateTransform result = mso2j2000STF.getStateTransform(et_1E9, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(2.9923767014516911E-01,
        -8.2838188078368691E-01, -4.7354015284384554E-01, 9.4982740185714298E-01,
        3.0594308511144458E-01, 6.5013347504959726E-02, 9.1020456202989181E-02,
        -4.6923585568642662E-01, 8.7836950555607685E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(1.1620652986822886E-06, 3.7430573374138549E-07, 7.9540509085090974E-08,
            -3.6610199480364946E-07, 1.0134828758645328E-06, 5.7935224434613016E-07,
            -6.5866186457382039E-14, -2.1215753800914023E-14, -4.5083783357885489E-15);

    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertRotationAngleEquals(expected.getRotation(), result.getRotation(), tol);
    AssertTools.assertComponentEquals(expected.getRotationDerivative(),
        result.getRotationDerivative(), tol);
  }

  /**
   * TODO Temporarily ignored, but needs to be investigated 
   */
  @Test
  @Ignore
  public void heeq0Test() {
    StateTransform result = heeq2j2000STF.getStateTransform(et_0, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(-1.7394628511077065E-01,
        8.8145099256714998E-01, 4.3907497947224089E-01, -9.7712451229681041E-01,
        -2.0989082814746285E-01, 3.4256790980660874E-02, 1.2235349347232782E-01,
        -4.2307208364764332E-01, 8.9779710106079025E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(-2.0015664290936060E-07, -4.2994565186693896E-08, 7.0172472323096899E-09,
            3.5631594577939038E-08, -1.8055863847551583E-07, -8.9941223222496205E-08,
            0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00);


    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertComponentRelativeEquality(expected, result, tol);
  }

  /**
   * TODO Temporarily ignored, but needs to be investigated 
   */
  @Test
  @Ignore
  public void heeq1E8Test() {
    StateTransform result = heeq2j2000STF.getStateTransform(et_1E8, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(-9.4774249830765600E-01,
        2.1874394493212512E-01, 2.3223962507955909E-01, -2.9464178172261962E-01,
        -8.7929579698391203E-01, -3.7419930768207899E-01, 1.2235349347232782E-01,
        -4.2307208364764332E-01, 8.9779710106079025E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(-6.0176998040799356E-08, -1.7958546524877275E-07, -7.6425654479825041E-08,
            1.9356487097791590E-07, -4.4675788575054891E-08, -4.7432116998821822E-08,
            0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00);


    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertComponentRelativeEquality(expected, result, tol);
  }

  /**
   * TODO Temporarily ignored, but needs to be investigated 
   */
  @Test
  @Ignore
  public void heeq1E9Test() {
    StateTransform result = heeq2j2000STF.getStateTransform(et_1E9, new StateTransform());
    RotationMatrixIJK rotMat = new RotationMatrixIJK(9.6362511762161163E-01,
        -1.6592382244432968E-01, -2.0951352661391007E-01, 2.3760525104435734E-01,
        8.9077454902096531E-01, 3.8738152704103462E-01, 1.2235349347232782E-01,
        -4.2307208364764332E-01, 8.9779710106079025E-01);
    MatrixIJK dRotMat =
        new MatrixIJK(4.6993156478603565E-08, 1.7617585295488713E-07, 7.6615649852625826E-08,
            -1.9058411268297835E-07, 3.2816127242064955E-08, 4.1437223703080132E-08,
            0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00);


    StateTransform expected = new StateTransform(rotMat, dRotMat);
    AssertTools.assertComponentRelativeEquality(expected, result, tol);
  }


  @Override
  public DynamicFrameFunction create()
      throws FKInstantiationException, DynamicFrameDefinitionException {

    UnwritableVectorIJK xVec = VectorIJK.I;
    UnwritableVectorIJK yVec = VectorIJK.J;
    DefinableStateVectorFunction function = EasyMock.createMock(DefinableStateVectorFunction.class);
    function.define(null, null, null);
    expect(function.getVector(0.0)).andReturn(xVec);
    expect(function.getStateVector(0.0)).andReturn(new StateVector(xVec, yVec));

    DefinableStateVectorFunction function2 =
        EasyMock.createMock(DefinableStateVectorFunction.class);
    function2.define(null, null, null);
    expect(function2.getVector(0.0)).andReturn(yVec);
    expect(function2.getStateVector(0.0)).andReturn(new StateVector(yVec, xVec));
    EasyMock.replay(function, function2);
    return new DynamicTwoVectorFrameFunction(1, 1, function, function2,
        new TwoVectorMatrix(Axis.I, Axis.J), Math.PI / 1000.0);
  }
}
