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

public class DynamicInertialFrameFunctionTest {
  private static DynamicFrameFunction function;
  private static DynamicInertialFrameFunction inertialFunction;
  private static RotationMatrixIJK testRotMat;
  private static StateTransform testStateTrans;
  private static final double TOLERANCE = 1e-14;


  @BeforeClass
  public static void setup() {
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
    inertialFunction = new DynamicInertialFrameFunction(function);
  }


  @Test
  public void inertialTransformTest() {
    RotationMatrixIJK inertialRotationMatrix =
        inertialFunction.getTransform(1.0, new RotationMatrixIJK());
    AssertTools.assertComponentRelativeEquality(testRotMat, inertialRotationMatrix, TOLERANCE);
  }

  @Test
  public void inertialStateTransformTest() {
    StateTransform testInertialStateTransform = new StateTransform(testRotMat, MatrixIJK.ZEROS);
    StateTransform inertialStateTransform =
        inertialFunction.getStateTransform(1.0, new StateTransform());
    AssertTools.assertComponentRelativeEquality(testInertialStateTransform, inertialStateTransform,
        TOLERANCE);
  }
}
