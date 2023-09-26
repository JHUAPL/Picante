package picante.spice.kernel.tk.fk.dynamic;

import static org.easymock.EasyMock.capture;
import static org.easymock.EasyMock.expect;
import org.easymock.EasyMock;
import org.junit.BeforeClass;
import org.junit.Test;
import picante.junit.AssertTools;
import picante.junit.CaptureAndAnswer;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.StateTransform;
import picante.spice.kernel.tk.fk.FKInstantiationException;

public class DynamicFrozenFrameFunctionTest {

  private static DynamicFrameFunction function;
  private static DynamicFrozenFrameFunction frozenFunction;
  private static RotationMatrixIJK testRotMat;
  private static StateTransform testStateTrans;
  private static final double TOLERANCE = 1e-14;

  @BeforeClass
  public static void setup() throws FKInstantiationException, DynamicFrameDefinitionException {
    testRotMat = new RotationMatrixIJK(1, 0, 0, 0, 1, 0, 0, 0, 1);
    MatrixIJK dRotMat = new MatrixIJK(2.55, 0, 50, 0, 15, 0, 60, 70, 3);
    testStateTrans = new StateTransform(testRotMat, dRotMat);
    function = EasyMock.createMock(DynamicFrameFunction.class);
    CaptureAndAnswer<RotationMatrixIJK> rotMatCapture = new CaptureAndAnswer<RotationMatrixIJK>() {
      @Override
      public void set(RotationMatrixIJK rotMat) {
        rotMat.setTo(testRotMat);
      }
    };
    function.define(null, null, null);
    expect(function.getTransform(EasyMock.eq(1.0), capture(rotMatCapture.getCapture())))
        .andAnswer(rotMatCapture).anyTimes();
    CaptureAndAnswer<StateTransform> stateTransCapture = new CaptureAndAnswer<StateTransform>() {
      @Override
      public void set(StateTransform stateTrans) {
        stateTrans.setTo(testStateTrans);
      }
    };
    expect(function.getStateTransform(EasyMock.eq(1.0), capture(stateTransCapture.getCapture())))
        .andAnswer(stateTransCapture).anyTimes();
    EasyMock.replay(function);
    frozenFunction = new DynamicFrozenFrameFunction(function, 1.0);
    frozenFunction.define(null, null, null);
  }


  @Test
  public void frozenTransformTest() {
    RotationMatrixIJK frozenRotationMatrix =
        frozenFunction.getTransform(2.0, new RotationMatrixIJK());
    AssertTools.assertComponentRelativeEquality(testRotMat, frozenRotationMatrix, TOLERANCE);
  }

  @Test
  public void frozenStateTransformTest() {
    StateTransform frozenStateTransform =
        frozenFunction.getStateTransform(2.0, new StateTransform());
    AssertTools.assertComponentRelativeEquality(testStateTrans, frozenStateTransform, TOLERANCE);
  }
}

