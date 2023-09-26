package picante.mechanics;

import static org.easymock.EasyMock.capture;
import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.eq;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.expectLastCall;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.reset;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static picante.junit.AssertTools.assertComponentRelativeEquality;
import static picante.junit.AssertTools.assertEquivalentMatrix;
import static picante.junit.AssertTools.assertEquivalentStateTransform;
import org.easymock.EasyMock;
import org.junit.Before;
import org.junit.Test;
import picante.junit.CaptureAndAnswer;
import picante.math.functions.UnivariateFunction;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.rotations.AxisAndAngle;
import picante.mechanics.rotations.EulerAngles;
import picante.mechanics.rotations.MatrixWrapper;
import picante.mechanics.rotations.WrapperWithRate;
import picante.mechanics.utilities.SimpleFrameID;

public class StateTransformFunctionsTest {

  private SimpleFrameID frameID;
  private SimpleFrameID otherID;
  private SimpleFrameID anotherID;
  private RotationMatrixIJK rotationBuffer;
  private StateTransform buffer;
  private Coverage coverage;
  private StateTransformFunction function;
  private StateTransformFunction anotherFunction;

  @Before
  public void setUp() throws Exception {
    frameID = new SimpleFrameID("FRAME");
    otherID = new SimpleFrameID("OTHER");
    anotherID = new SimpleFrameID("ANOTHER");
    rotationBuffer = new RotationMatrixIJK();
    buffer = new StateTransform();
    coverage = createMock(Coverage.class);
    function = createMock(StateTransformFunction.class);
    anotherFunction = createMock(StateTransformFunction.class);
  }

  @Test
  public void testInvertFrameEvaluation() {

    final RotationMatrixIJK rawRotation =
        new RotationMatrixIJK(VectorIJK.J, VectorIJK.I, VectorIJK.MINUS_K);

    CaptureAndAnswer<RotationMatrixIJK> capture = CaptureAndAnswer.create(rawRotation);

    expect(function.getTransform(eq(10.0), capture(capture.getCapture()))).andAnswer(capture);
    replay(function);

    StateTransformFunction f = StateTransformFunctions.invert(function);
    RotationMatrixIJK result = f.getTransform(10.0, rotationBuffer);
    RotationMatrixIJK expected = new RotationMatrixIJK(rawRotation);
    expected.transpose();

    assertSame(rotationBuffer, result);
    assertEquivalentMatrix(expected, result);
    verify(function);

    /*
     * Now verify that the to and from IDs are switched.
     */
    reset(function);
    expect(function.getFromID()).andReturn(frameID);
    replay(function);
    assertSame(frameID, f.getToID());
    verify(function);

    reset(function);
    expect(function.getToID()).andReturn(frameID);
    replay(function);
    assertSame(frameID, f.getFromID());
    verify(function);

  }

  @Test
  public void testInvertStateEvaluation() {

    final StateTransform rawState =
        new StateTransform(new RotationMatrixIJK(VectorIJK.J, VectorIJK.I, VectorIJK.MINUS_K),
            new MatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9));

    CaptureAndAnswer<StateTransform> capture = CaptureAndAnswer.create(rawState);

    expect(function.getStateTransform(eq(10.0), capture(capture.getCapture()))).andAnswer(capture);
    replay(function);

    StateTransformFunction f = StateTransformFunctions.invert(function);
    StateTransform result = f.getStateTransform(10.0, buffer);
    StateTransform expected = new StateTransform(rawState).invert();

    assertSame(buffer, result);
    assertEquivalentStateTransform(expected, result);
    verify(function);

    /*
     * Now verify that the to and from IDs are switched.
     */
    reset(function);
    expect(function.getFromID()).andReturn(frameID);
    replay(function);
    assertSame(frameID, f.getToID());
    verify(function);

    reset(function);
    expect(function.getToID()).andReturn(frameID);
    replay(function);
    assertSame(frameID, f.getFromID());
    verify(function);

  }

  @Test
  public void testCreateFixedAllTimeFrameEvaluation() {

    RotationMatrixIJK fixed = new RotationMatrixIJK(VectorIJK.J, VectorIJK.I, VectorIJK.MINUS_K);

    StateTransformFunction f = StateTransformFunctions.createFixed(frameID, otherID, fixed);

    assertSame(frameID, f.getFromID());
    assertSame(otherID, f.getToID());

    RotationMatrixIJK result = f.getTransform(10.0, rotationBuffer);
    assertSame(result, rotationBuffer);
    assertEquivalentMatrix(fixed, result);

    Coverage coverage = f.getCoverage();
    assertSame(coverage, Coverage.ALL_TIME);

    /*
     * Alter fixed and verify it has no impact on f.
     */
    RotationMatrixIJK fixedCopy = new RotationMatrixIJK(fixed);

    fixed.setTo(RotationMatrixIJK.IDENTITY);

    result = f.getTransform(11.0, rotationBuffer);
    assertSame(result, rotationBuffer);
    assertEquivalentMatrix(fixedCopy, result);

  }

  @Test
  public void testCreateFixedAllTimeStateEvaluation() {

    RotationMatrixIJK fixed = new RotationMatrixIJK(VectorIJK.J, VectorIJK.I, VectorIJK.MINUS_K);

    StateTransform expected = new StateTransform(fixed, MatrixIJK.ZEROS);

    StateTransformFunction f = StateTransformFunctions.createFixed(frameID, otherID, fixed);

    assertSame(frameID, f.getFromID());
    assertSame(otherID, f.getToID());

    StateTransform result = f.getStateTransform(10.0, buffer);
    assertSame(result, buffer);
    assertEquivalentStateTransform(expected, result);

    Coverage coverage = f.getCoverage();
    assertSame(coverage, Coverage.ALL_TIME);

    /*
     * Alter fixed and verify it has no impact on f.
     */
    fixed.setTo(RotationMatrixIJK.IDENTITY);

    result = f.getStateTransform(11.0, buffer);
    assertSame(result, buffer);
    assertEquivalentStateTransform(expected, result);

  }

  @Test
  public void testCreateFixedFrameEvaluation() {

    RotationMatrixIJK fixed = new RotationMatrixIJK(VectorIJK.J, VectorIJK.I, VectorIJK.MINUS_K);

    expect(coverage.contains(-1000.0)).andReturn(true).atLeastOnce();
    expect(coverage.contains(1000.0)).andReturn(true);

    replay(coverage);

    StateTransformFunction f =
        StateTransformFunctions.createFixed(frameID, otherID, coverage, fixed);

    assertSame(frameID, f.getFromID());
    assertSame(otherID, f.getToID());
    assertSame(coverage, f.getCoverage());

    RotationMatrixIJK result = f.getTransform(-1000.0, rotationBuffer);
    assertSame(result, rotationBuffer);
    assertEquivalentMatrix(fixed, result);

    result = f.getTransform(1000.0, rotationBuffer);
    assertSame(result, rotationBuffer);
    assertEquivalentMatrix(fixed, result);

    RotationMatrixIJK fixedCopy = new RotationMatrixIJK(fixed);

    fixed.setTo(RotationMatrixIJK.IDENTITY);

    Coverage coverage = f.getCoverage();

    result = f.getTransform(-1000.0, rotationBuffer);
    assertSame(result, rotationBuffer);
    assertEquivalentMatrix(fixedCopy, result);

    verify(coverage);

  }

  @Test
  public void testCreateFixedStateEvaluation() {

    RotationMatrixIJK fixed = new RotationMatrixIJK(VectorIJK.J, VectorIJK.I, VectorIJK.MINUS_K);

    StateTransform expected = new StateTransform(fixed, MatrixIJK.ZEROS);

    expect(coverage.contains(-1000.0)).andReturn(true).atLeastOnce();
    expect(coverage.contains(1000.0)).andReturn(true);

    replay(coverage);

    StateTransformFunction f =
        StateTransformFunctions.createFixed(frameID, otherID, coverage, fixed);

    assertSame(frameID, f.getFromID());
    assertSame(otherID, f.getToID());
    assertSame(coverage, f.getCoverage());

    StateTransform result = f.getStateTransform(-1000.0, buffer);
    assertSame(result, buffer);
    assertEquivalentStateTransform(expected, result);

    result = f.getStateTransform(1000.0, buffer);
    assertSame(result, buffer);
    assertEquivalentStateTransform(expected, result);

    fixed.setTo(RotationMatrixIJK.IDENTITY);

    result = f.getStateTransform(-1000.0, buffer);
    assertSame(result, buffer);
    assertEquivalentStateTransform(expected, result);

  }

  @Test(expected = IllegalArgumentException.class)
  public void testMxmIncompatibleIDException() {

    expect(function.getFromID()).andReturn(frameID).anyTimes();
    expect(function.getToID()).andReturn(otherID).anyTimes();
    expect(function.getCoverage()).andReturn(Coverages.create(0, 10));

    expect(anotherFunction.getFromID()).andReturn(new SimpleFrameID("TEST")).anyTimes();
    expect(anotherFunction.getToID()).andReturn(anotherID).anyTimes();
    expect(anotherFunction.getCoverage()).andReturn(Coverages.create(0, 10));

    replay(function);
    replay(anotherFunction);

    /*
     * Should throw an illegal argument exception due to the incompatibility of function.getFromID()
     * and anotherFunction.getToID().
     */
    StateTransformFunctions.mxm(function, anotherFunction);

  }

  @Test(expected = IllegalArgumentException.class)
  public void testMxmIncompatibleCoverageException() {

    expect(function.getFromID()).andReturn(frameID).anyTimes();
    expect(function.getToID()).andReturn(anotherID).anyTimes();
    expect(function.getCoverage()).andReturn(Coverages.create(0, 10));

    expect(anotherFunction.getFromID()).andReturn(otherID).anyTimes();
    expect(anotherFunction.getToID()).andReturn(frameID).anyTimes();
    expect(anotherFunction.getCoverage()).andReturn(Coverages.create(20, 30));

    replay(function, anotherFunction);

    StateTransformFunctions.mxm(function, anotherFunction);

  }

  @Test
  public void testMxm() {

    expect(function.getFromID()).andReturn(frameID).anyTimes();
    expect(function.getToID()).andReturn(anotherID).anyTimes();
    expect(function.getCoverage()).andReturn(Coverages.create(0, 10));
    final UnwritableRotationMatrixIJK functionMatrix = UnwritableRotationMatrixIJK
        .copyOf(new EulerAngles.IJI(0.1, 0.2, 0.3).getRotation(new RotationMatrixIJK()));
    CaptureAndAnswer<RotationMatrixIJK> functionCapture = CaptureAndAnswer.create(functionMatrix);
    expect(function.getTransform(eq(0.0), capture(functionCapture.getCapture())))
        .andAnswer(functionCapture);
    final UnwritableStateTransform functionTransform =
        UnwritableStateTransform.copyOf(new StateTransform(functionMatrix, MatrixIJK.ZEROS));
    CaptureAndAnswer<StateTransform> functionStateCapture =
        CaptureAndAnswer.create(functionTransform);
    expect(function.getStateTransform(eq(0.0), capture(functionStateCapture.getCapture())))
        .andAnswer(functionStateCapture);

    expect(anotherFunction.getFromID()).andReturn(otherID).anyTimes();
    expect(anotherFunction.getToID()).andReturn(frameID).anyTimes();
    expect(anotherFunction.getCoverage()).andReturn(Coverages.create(0, 10));
    final UnwritableRotationMatrixIJK anotherFunctionMatrix = UnwritableRotationMatrixIJK
        .copyOf(new EulerAngles.KIJ(0.15, 0.0, 0.31).getRotation(new RotationMatrixIJK()));
    CaptureAndAnswer<RotationMatrixIJK> anotherFunctionCapture =
        CaptureAndAnswer.create(anotherFunctionMatrix);
    expect(anotherFunction.getTransform(eq(0.0), capture(anotherFunctionCapture.getCapture())))
        .andAnswer(anotherFunctionCapture);
    final UnwritableStateTransform anotherFunctionTransform =
        UnwritableStateTransform.copyOf(new StateTransform(anotherFunctionMatrix, MatrixIJK.ZEROS));
    CaptureAndAnswer<StateTransform> anotherFunctionStateCapture =
        CaptureAndAnswer.create(anotherFunctionTransform);
    expect(anotherFunction.getStateTransform(eq(0.0),
        capture(anotherFunctionStateCapture.getCapture()))).andAnswer(anotherFunctionStateCapture);

    replay(function, anotherFunction);

    StateTransformFunction mxm = StateTransformFunctions.mxm(function, anotherFunction);

    assertEquals(anotherID, mxm.getToID());
    assertEquals(otherID, mxm.getFromID());
    /*
     * We can use equals() because this fundamentally is executing the same code to perform the
     * matrix multiply.
     */
    assertEquals(RotationMatrixIJK.mxm(functionMatrix, anotherFunctionMatrix),
        mxm.getTransform(0.0, new RotationMatrixIJK()));

    assertEquals(
        StateTransform.mxm(functionTransform, anotherFunctionTransform, new StateTransform()),
        mxm.getStateTransform(0.0, new StateTransform()));

    assertEquals(Coverages.create(0, 10), mxm.getCoverage());

    verify(function, anotherFunction);

  }

  @Test(expected = IllegalArgumentException.class)
  public void testMxmiIncompatibleIDException() {

    expect(function.getFromID()).andReturn(frameID).anyTimes();
    expect(function.getToID()).andReturn(otherID).anyTimes();
    expect(function.getCoverage()).andReturn(Coverages.create(0, 10));

    expect(anotherFunction.getFromID()).andReturn(anotherID).anyTimes();
    expect(anotherFunction.getToID()).andReturn(new SimpleFrameID("TEST")).anyTimes();
    expect(anotherFunction.getCoverage()).andReturn(Coverages.create(0, 10));

    replay(function, anotherFunction);

    /*
     * Should throw an illegal argument exception due to the incompatibility of function.getFromID()
     * and anotherFunction.getFromID().
     */
    StateTransformFunctions.mxmi(function, anotherFunction);

  }

  @Test(expected = IllegalArgumentException.class)
  public void testMxmtIncompatibleCoverageException() {

    expect(function.getFromID()).andReturn(frameID).anyTimes();
    expect(function.getToID()).andReturn(anotherID).anyTimes();
    expect(function.getCoverage()).andReturn(Coverages.create(0, 10));

    expect(anotherFunction.getFromID()).andReturn(frameID).anyTimes();
    expect(anotherFunction.getToID()).andReturn(otherID).anyTimes();
    expect(anotherFunction.getCoverage()).andReturn(Coverages.create(20, 30));

    replay(function, anotherFunction);

    StateTransformFunctions.mxmi(function, anotherFunction);

  }

  @Test
  public void testMxmi() {

    expect(function.getFromID()).andReturn(frameID).anyTimes();
    expect(function.getToID()).andReturn(anotherID).anyTimes();
    expect(function.getCoverage()).andReturn(Coverages.create(0, 10));
    final UnwritableRotationMatrixIJK functionMatrix = UnwritableRotationMatrixIJK
        .copyOf(new EulerAngles.IJI(0.1, 0.2, 0.3).getRotation(new RotationMatrixIJK()));
    CaptureAndAnswer<RotationMatrixIJK> functionCapture = CaptureAndAnswer.create(functionMatrix);
    expect(function.getTransform(eq(0.0), capture(functionCapture.getCapture())))
        .andAnswer(functionCapture);
    final UnwritableStateTransform functionTransform =
        UnwritableStateTransform.copyOf(new StateTransform(functionMatrix, MatrixIJK.ZEROS));
    CaptureAndAnswer<StateTransform> functionStateCapture =
        CaptureAndAnswer.create(functionTransform);
    expect(function.getStateTransform(eq(0.0), capture(functionStateCapture.getCapture())))
        .andAnswer(functionStateCapture);

    expect(anotherFunction.getFromID()).andReturn(frameID).anyTimes();
    expect(anotherFunction.getToID()).andReturn(otherID).anyTimes();
    expect(anotherFunction.getCoverage()).andReturn(Coverages.create(0, 10));
    final UnwritableRotationMatrixIJK anotherFunctionMatrix = UnwritableRotationMatrixIJK
        .copyOf(new EulerAngles.KIJ(0.15, 0.0, 0.31).getRotation(new RotationMatrixIJK()));
    CaptureAndAnswer<RotationMatrixIJK> anotherFunctionCapture =
        CaptureAndAnswer.create(anotherFunctionMatrix);
    expect(anotherFunction.getTransform(eq(0.0), capture(anotherFunctionCapture.getCapture())))
        .andAnswer(anotherFunctionCapture);
    final UnwritableStateTransform anotherFunctionTransform =
        UnwritableStateTransform.copyOf(new StateTransform(anotherFunctionMatrix, MatrixIJK.ZEROS));
    CaptureAndAnswer<StateTransform> anotherFunctionStateCapture =
        CaptureAndAnswer.create(anotherFunctionTransform);
    expect(anotherFunction.getStateTransform(eq(0.0),
        capture(anotherFunctionStateCapture.getCapture()))).andAnswer(anotherFunctionStateCapture);

    replay(function, anotherFunction);

    StateTransformFunction mxmi = StateTransformFunctions.mxmi(function, anotherFunction);

    assertEquals(anotherID, mxmi.getToID());
    assertEquals(otherID, mxmi.getFromID());
    /*
     * We can use equals() because this fundamentally is executing the same code to perform the
     * matrix multiply.
     */
    assertEquals(RotationMatrixIJK.mxmt(functionMatrix, anotherFunctionMatrix),
        mxmi.getTransform(0.0, new RotationMatrixIJK()));
    assertEquals(
        StateTransform.mxmi(functionTransform, anotherFunctionTransform, new StateTransform()),
        mxmi.getStateTransform(0.0, new StateTransform()));

    assertEquals(Coverages.create(0, 10), mxmi.getCoverage());

    verify(function, anotherFunction);

  }

  @Test(expected = IllegalArgumentException.class)
  public void testMixmIncompatibleIDException() {

    expect(function.getFromID()).andReturn(frameID).anyTimes();
    expect(function.getToID()).andReturn(otherID).anyTimes();
    expect(function.getCoverage()).andReturn(Coverages.create(0, 10));

    expect(anotherFunction.getFromID()).andReturn(anotherID).anyTimes();
    expect(anotherFunction.getToID()).andReturn(new SimpleFrameID("TEST")).anyTimes();
    expect(anotherFunction.getCoverage()).andReturn(Coverages.create(0, 10));

    replay(function);
    replay(anotherFunction);

    /*
     * Should throw an illegal argument exception due to the incompatibility of function.getToID()
     * and anotherFunction.getToID().
     */
    StateTransformFunctions.mixm(function, anotherFunction);

  }

  @Test(expected = IllegalArgumentException.class)
  public void testMixmIncompatibleCoverageException() {

    expect(function.getFromID()).andReturn(anotherID).anyTimes();
    expect(function.getToID()).andReturn(frameID).anyTimes();
    expect(function.getCoverage()).andReturn(Coverages.create(0, 10));

    expect(anotherFunction.getFromID()).andReturn(otherID).anyTimes();
    expect(anotherFunction.getToID()).andReturn(frameID).anyTimes();
    expect(anotherFunction.getCoverage()).andReturn(Coverages.create(20, 30));

    replay(function, anotherFunction);

    StateTransformFunctions.mixm(function, anotherFunction);

  }

  @Test
  public void testMtxm() {

    expect(function.getFromID()).andReturn(anotherID).anyTimes();
    expect(function.getToID()).andReturn(frameID).anyTimes();
    expect(function.getCoverage()).andReturn(Coverages.create(0, 10));
    final UnwritableRotationMatrixIJK functionMatrix = UnwritableRotationMatrixIJK
        .copyOf(new EulerAngles.IJI(0.1, 0.2, 0.3).getRotation(new RotationMatrixIJK()));
    CaptureAndAnswer<RotationMatrixIJK> functionCapture = CaptureAndAnswer.create(functionMatrix);
    expect(function.getTransform(eq(0.0), capture(functionCapture.getCapture())))
        .andAnswer(functionCapture);
    final UnwritableStateTransform functionTransform =
        UnwritableStateTransform.copyOf(new StateTransform(functionMatrix, MatrixIJK.ZEROS));
    CaptureAndAnswer<StateTransform> functionStateCapture =
        CaptureAndAnswer.create(functionTransform);
    expect(function.getStateTransform(eq(0.0), capture(functionStateCapture.getCapture())))
        .andAnswer(functionStateCapture);

    expect(anotherFunction.getFromID()).andReturn(otherID).anyTimes();
    expect(anotherFunction.getToID()).andReturn(frameID).anyTimes();
    expect(anotherFunction.getCoverage()).andReturn(Coverages.create(0, 10));
    final UnwritableRotationMatrixIJK anotherFunctionMatrix = UnwritableRotationMatrixIJK
        .copyOf(new EulerAngles.KIJ(0.15, 0.0, 0.31).getRotation(new RotationMatrixIJK()));
    CaptureAndAnswer<RotationMatrixIJK> anotherFunctionCapture =
        CaptureAndAnswer.create(anotherFunctionMatrix);
    expect(anotherFunction.getTransform(eq(0.0), capture(anotherFunctionCapture.getCapture())))
        .andAnswer(anotherFunctionCapture);
    final UnwritableStateTransform anotherFunctionTransform =
        UnwritableStateTransform.copyOf(new StateTransform(anotherFunctionMatrix, MatrixIJK.ZEROS));
    CaptureAndAnswer<StateTransform> anotherFunctionStateCapture =
        CaptureAndAnswer.create(anotherFunctionTransform);
    expect(anotherFunction.getStateTransform(eq(0.0),
        capture(anotherFunctionStateCapture.getCapture()))).andAnswer(anotherFunctionStateCapture);

    replay(function, anotherFunction);

    StateTransformFunction mixm = StateTransformFunctions.mixm(function, anotherFunction);

    assertEquals(anotherID, mixm.getToID());
    assertEquals(otherID, mixm.getFromID());
    /*
     * We can use equals() because this fundamentally is executing the same code to perform the
     * matrix multiply.
     */
    assertEquals(RotationMatrixIJK.mtxm(functionMatrix, anotherFunctionMatrix),
        mixm.getTransform(0.0, new RotationMatrixIJK()));
    assertEquals(
        StateTransform.mixm(functionTransform, anotherFunctionTransform, new StateTransform()),
        mixm.getStateTransform(0.0, new StateTransform()));

    assertEquals(Coverages.create(0, 10), mixm.getCoverage());

    verify(function, anotherFunction);

  }

  @Test(expected = IllegalArgumentException.class)
  public void testZeroDeltaEstimateDerivative() {
    StateTransformFunctions.estimateDerivative(createMock(FrameTransformFunction.class), 0.0);
  }

  @Test
  public void testPositiveDeltaConstantEstimateDerivative() {

    SimpleFrameID fromID = new SimpleFrameID("FROM");
    SimpleFrameID toID = new SimpleFrameID("TO");

    FrameTransformFunction f = EasyMock.createMock(FrameTransformFunction.class);

    AxisAndAngle aa = new AxisAndAngle(new VectorIJK(1, 1, 1), Math.toRadians(15.0));

    final RotationMatrixIJK atTime = aa.getRotation(new RotationMatrixIJK());
    CaptureAndAnswer<RotationMatrixIJK> atTimeCapture = new CaptureAndAnswer<RotationMatrixIJK>() {
      @Override
      public void set(RotationMatrixIJK captured) {
        captured.setTo(atTime);
      }
    };

    aa.setAngle(Math.toRadians(45.0));

    final RotationMatrixIJK atOtherTime = aa.getRotation(new RotationMatrixIJK());
    CaptureAndAnswer<RotationMatrixIJK> otherTimeCapture =
        new CaptureAndAnswer<RotationMatrixIJK>() {
          @Override
          public void set(RotationMatrixIJK captured) {
            captured.setTo(atOtherTime);
          }
        };

    f.getTransform(eq(0.0), capture(atTimeCapture.getCapture()));
    expectLastCall().andAnswer(atTimeCapture).anyTimes();
    f.getTransform(eq(1.0), capture(otherTimeCapture.getCapture()));
    expectLastCall().andAnswer(otherTimeCapture).anyTimes();

    expect(f.getFromID()).andReturn(fromID).anyTimes();
    expect(f.getToID()).andReturn(toID).anyTimes();

    expect(f.getCoverage()).andReturn(Coverage.ALL_TIME).anyTimes();

    replay(f);

    StateTransformFunction s = StateTransformFunctions.estimateDerivative(f, 1.0);

    assertSame(fromID, s.getFromID());
    assertSame(toID, s.getToID());
    assertSame(Coverage.ALL_TIME, s.getCoverage());
    assertEquals(atTime, s.getTransform(0.0, new RotationMatrixIJK()));

    /*
     * The rotation component should be atTime. The angular velocity should be 30 degrees per second
     * about the -1, -1, -1 axis.
     */
    WrapperWithRate<MatrixWrapper> expectedGenerator =
        new WrapperWithRate<MatrixWrapper>(new MatrixWrapper(atTime));
    expectedGenerator.setRate(new VectorIJK(-1, -1, -1).unitize().scale(Math.toRadians(30.0)));

    assertComponentRelativeEquality(expectedGenerator.getTransform(new StateTransform()),
        s.getStateTransform(0.0, new StateTransform()), 1e-15);

    verify(f);
  }

  @Test
  public void testNegativeDeltaConstantEstimateDerivative() {

    SimpleFrameID fromID = new SimpleFrameID("FROM");
    SimpleFrameID toID = new SimpleFrameID("TO");

    FrameTransformFunction f = EasyMock.createMock(FrameTransformFunction.class);

    AxisAndAngle aa = new AxisAndAngle(new VectorIJK(1, 1, 1), Math.toRadians(45.0));

    final RotationMatrixIJK atTime = aa.getRotation(new RotationMatrixIJK());
    CaptureAndAnswer<RotationMatrixIJK> atTimeCapture = new CaptureAndAnswer<RotationMatrixIJK>() {
      @Override
      public void set(RotationMatrixIJK captured) {
        captured.setTo(atTime);
      }
    };

    aa.setAngle(Math.toRadians(15.0));

    final RotationMatrixIJK atOtherTime = aa.getRotation(new RotationMatrixIJK());
    CaptureAndAnswer<RotationMatrixIJK> otherTimeCapture =
        new CaptureAndAnswer<RotationMatrixIJK>() {
          @Override
          public void set(RotationMatrixIJK captured) {
            captured.setTo(atOtherTime);
          }
        };

    f.getTransform(eq(0.0), capture(atTimeCapture.getCapture()));
    expectLastCall().andAnswer(atTimeCapture).anyTimes();
    f.getTransform(eq(-1.0), capture(otherTimeCapture.getCapture()));
    expectLastCall().andAnswer(otherTimeCapture).anyTimes();

    expect(f.getFromID()).andReturn(fromID).anyTimes();
    expect(f.getToID()).andReturn(toID).anyTimes();

    expect(f.getCoverage()).andReturn(Coverage.ALL_TIME).anyTimes();

    replay(f);

    StateTransformFunction s = StateTransformFunctions.estimateDerivative(f, -1.0);

    assertSame(fromID, s.getFromID());
    assertSame(toID, s.getToID());
    assertSame(Coverage.ALL_TIME, s.getCoverage());
    assertEquals(atTime, s.getTransform(0.0, new RotationMatrixIJK()));

    /*
     * The rotation component should be atTime. The angular velocity should be 30 degrees per second
     * about the -1, -1, -1 axis.
     */
    WrapperWithRate<MatrixWrapper> expectedGenerator =
        new WrapperWithRate<MatrixWrapper>(new MatrixWrapper(atTime));
    expectedGenerator.setRate(new VectorIJK(-1, -1, -1).unitize().scale(Math.toRadians(30.0)));

    assertComponentRelativeEquality(expectedGenerator.getTransform(new StateTransform()),
        s.getStateTransform(0.0, new StateTransform()), 1e-15);

    verify(f);
  }

  @Test
  public void testDeltaFunctionEstimateDerivative() {

    SimpleFrameID fromID = new SimpleFrameID("FROM");
    SimpleFrameID toID = new SimpleFrameID("TO");

    UnivariateFunction dF = EasyMock.createMock(UnivariateFunction.class);
    expect(dF.evaluate(0.0)).andReturn(-1.0).anyTimes();

    FrameTransformFunction f = EasyMock.createMock(FrameTransformFunction.class);

    AxisAndAngle aa = new AxisAndAngle(new VectorIJK(1, 1, 1), Math.toRadians(45.0));

    final RotationMatrixIJK atTime = aa.getRotation(new RotationMatrixIJK());
    CaptureAndAnswer<RotationMatrixIJK> atTimeCapture = new CaptureAndAnswer<RotationMatrixIJK>() {
      @Override
      public void set(RotationMatrixIJK captured) {
        captured.setTo(atTime);
      }
    };

    aa.setAngle(Math.toRadians(15.0));

    final RotationMatrixIJK atOtherTime = aa.getRotation(new RotationMatrixIJK());
    CaptureAndAnswer<RotationMatrixIJK> otherTimeCapture =
        new CaptureAndAnswer<RotationMatrixIJK>() {
          @Override
          public void set(RotationMatrixIJK captured) {
            captured.setTo(atOtherTime);
          }
        };

    f.getTransform(eq(0.0), capture(atTimeCapture.getCapture()));
    expectLastCall().andAnswer(atTimeCapture).anyTimes();
    f.getTransform(eq(-1.0), capture(otherTimeCapture.getCapture()));
    expectLastCall().andAnswer(otherTimeCapture).anyTimes();

    expect(f.getFromID()).andReturn(fromID).anyTimes();
    expect(f.getToID()).andReturn(toID).anyTimes();

    expect(f.getCoverage()).andReturn(Coverage.ALL_TIME).anyTimes();

    replay(f, dF);

    StateTransformFunction s = StateTransformFunctions.estimateDerivative(f, dF);

    assertSame(fromID, s.getFromID());
    assertSame(toID, s.getToID());
    assertSame(Coverage.ALL_TIME, s.getCoverage());
    assertEquals(atTime, s.getTransform(0.0, new RotationMatrixIJK()));

    /*
     * The rotation component should be atTime. The angular velocity should be 30 degrees per second
     * about the -1, -1, -1 axis.
     */
    WrapperWithRate<MatrixWrapper> expectedGenerator =
        new WrapperWithRate<MatrixWrapper>(new MatrixWrapper(atTime));
    expectedGenerator.setRate(new VectorIJK(-1, -1, -1).unitize().scale(Math.toRadians(30.0)));

    assertComponentRelativeEquality(expectedGenerator.getTransform(new StateTransform()),
        s.getStateTransform(0.0, new StateTransform()), 1e-15);

    verify(f, dF);


  }

  @Test
  public void testDeltaFunctionZeroAngularRate() {

    SimpleFrameID fromID = new SimpleFrameID("FROM");
    SimpleFrameID toID = new SimpleFrameID("TO");

    UnivariateFunction dF = EasyMock.createMock(UnivariateFunction.class);
    expect(dF.evaluate(0.0)).andReturn(-1.0).anyTimes();

    FrameTransformFunction f = EasyMock.createMock(FrameTransformFunction.class);

    AxisAndAngle aa = new AxisAndAngle(new VectorIJK(1, 1, 1), Math.toRadians(15.0));

    final RotationMatrixIJK atTime = aa.getRotation(new RotationMatrixIJK());
    CaptureAndAnswer<RotationMatrixIJK> atTimeCapture = new CaptureAndAnswer<RotationMatrixIJK>() {
      @Override
      public void set(RotationMatrixIJK captured) {
        captured.setTo(atTime);
      }
    };

    aa.setAngle(Math.toRadians(15.0));

    final RotationMatrixIJK atOtherTime = aa.getRotation(new RotationMatrixIJK());
    CaptureAndAnswer<RotationMatrixIJK> otherTimeCapture =
        new CaptureAndAnswer<RotationMatrixIJK>() {
          @Override
          public void set(RotationMatrixIJK captured) {
            captured.setTo(atOtherTime);
          }
        };

    f.getTransform(eq(0.0), capture(atTimeCapture.getCapture()));
    expectLastCall().andAnswer(atTimeCapture).anyTimes();
    f.getTransform(eq(-1.0), capture(otherTimeCapture.getCapture()));
    expectLastCall().andAnswer(otherTimeCapture).anyTimes();

    expect(f.getFromID()).andReturn(fromID).anyTimes();
    expect(f.getToID()).andReturn(toID).anyTimes();

    expect(f.getCoverage()).andReturn(Coverage.ALL_TIME).anyTimes();

    replay(f, dF);

    StateTransformFunction s = StateTransformFunctions.estimateDerivative(f, dF);

    assertSame(fromID, s.getFromID());
    assertSame(toID, s.getToID());
    assertSame(Coverage.ALL_TIME, s.getCoverage());
    assertEquals(atTime, s.getTransform(0.0, new RotationMatrixIJK()));

    /*
     * The rotation component should be atTime. The angular velocity should be 30 degrees per second
     * about the -1, -1, -1 axis.
     */
    WrapperWithRate<MatrixWrapper> expectedGenerator =
        new WrapperWithRate<MatrixWrapper>(new MatrixWrapper(atTime));
    expectedGenerator.setRate(VectorIJK.ZERO);

    assertComponentRelativeEquality(expectedGenerator.getTransform(new StateTransform()),
        s.getStateTransform(0.0, new StateTransform()), 1e-15);

    verify(f, dF);
  }

  @Test(expected = FrameEvaluationException.class)
  public void testDeltaFunctionZeroValuedEstimateDerivativeException() {
    UnivariateFunction dF = createMock(UnivariateFunction.class);
    expect(dF.evaluate(0.0)).andReturn(0.0).anyTimes();
    FrameTransformFunction f = createMock(FrameTransformFunction.class);
    StateTransformFunctions.estimateDerivative(f, dF).getStateTransform(0.0, new StateTransform());
  }

}
