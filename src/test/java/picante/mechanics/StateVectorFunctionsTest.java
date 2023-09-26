package picante.mechanics;

import static org.easymock.EasyMock.capture;
import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.eq;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.reset;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static picante.junit.AssertTools.assertEquivalentStateVector;
import static picante.junit.AssertTools.assertEquivalentVector;
import org.junit.Before;
import org.junit.Test;
import picante.junit.AssertTools;
import picante.junit.CaptureAndAnswer;
import picante.math.functions.DifferentiableVectorIJKFunction;
import picante.math.functions.VectorIJKFunction;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.utilities.SimpleEphemerisID;
import picante.mechanics.utilities.SimpleFrameID;

public class StateVectorFunctionsTest {

  private StateVectorFunction function;
  private CaptureAndAnswer<StateVector> capture;
  private CaptureAndAnswer<VectorIJK> vectorCapture;
  private StateVector buffer;
  private StateVector expected;
  private SimpleEphemerisID ephemerisID;
  private SimpleEphemerisID otherID;
  private SimpleFrameID frameID;
  private StateTransformFunction transformFunction;
  private Coverage coverage;
  private VectorIJK vectorBuffer;

  @Before
  public void setUp() throws Exception {
    ephemerisID = new SimpleEphemerisID("ID");
    otherID = new SimpleEphemerisID("OTHER");
    frameID = new SimpleFrameID("ID");
    function = createMock(StateVectorFunction.class);
    transformFunction = createMock(StateTransformFunction.class);
    coverage = createMock(Coverage.class);
    buffer = new StateVector();
    expected = new StateVector();
    vectorBuffer = new VectorIJK();
  }

  @Test
  public void testAdapt() {

    final StateVector rawState =
        new StateVector(new VectorIJK(100, 200, 300), new VectorIJK(-1, -2, -3));

    capture = CaptureAndAnswer.create(rawState);

    vectorCapture = CaptureAndAnswer.create(rawState.getPosition());

    expect(function.getPosition(eq(10.0), capture(vectorCapture.getCapture())))
        .andAnswer(vectorCapture);
    expect(function.getState(eq(10.0), capture(capture.getCapture()))).andAnswer(capture)
        .atLeastOnce();

    replay(function);

    DifferentiableVectorIJKFunction f = StateVectorFunctions.adapt(function);

    VectorIJK result = f.evaluate(10.0, vectorBuffer);

    assertSame(result, vectorBuffer);
    assertEquivalentVector(rawState.getPosition(), result);

    result = f.differentiate(10.0, vectorBuffer);
    assertSame(result, vectorBuffer);
    assertEquivalentVector(rawState.getVelocity(), result);

    verify(function);

  }

  @Test
  public void testAdaptVelocity() {

    final StateVector rawState =
        new StateVector(new VectorIJK(100, 200, 300), new VectorIJK(-1, -2, -3));

    capture = CaptureAndAnswer.create(rawState);

    expect(function.getState(eq(10.0), capture(capture.getCapture()))).andAnswer(capture)
        .atLeastOnce();

    replay(function);

    VectorIJKFunction f = StateVectorFunctions.adaptVelocity(function);

    VectorIJK result = f.evaluate(10.0, vectorBuffer);

    assertSame(result, vectorBuffer);
    assertEquivalentVector(rawState.getVelocity(), result);

    verify(function);
  }

  @Test
  public void testScaleStateVectorFunctionDouble() {

    final StateVector rawState =
        new StateVector(new VectorIJK(1000, 2000, 3000), new VectorIJK(-1, -2, -3));

    capture = CaptureAndAnswer.create(rawState);

    vectorCapture = CaptureAndAnswer.create(rawState.getPosition());

    expect(function.getState(eq(10.0), capture(capture.getCapture()))).andAnswer(capture);
    expect(function.getPosition(eq(10.0), capture(vectorCapture.getCapture())))
        .andAnswer(vectorCapture);
    replay(function);

    StateVectorFunction f = StateVectorFunctions.scale(function, 0.001);

    StateVector result = f.getState(10.0, buffer);
    expected.setTo(new StateVector(new VectorIJK(1, 2, 3), new VectorIJK(-0.001, -0.002, -0.003)));

    assertSame(buffer, result);
    assertEquivalentStateVector(expected, result);

    VectorIJK vectorResult = f.getPosition(10.0, buffer.getPosition());

    assertSame(buffer.getPosition(), vectorResult);
    assertEquivalentVector(expected.getPosition(), vectorResult);

    verify(function);

  }

  @Test(expected = IllegalArgumentException.class)
  public void testScaleNegativeScaleException() {
    StateVectorFunctions.scale(function, -1.0);
  }

  @Test
  public void testScaleStateVectorFunctionDoubleDouble() {

    final StateVector rawState =
        new StateVector(new VectorIJK(1000, 2000, 3000), new VectorIJK(-1, -2, -3));

    capture = CaptureAndAnswer.create(rawState);
    vectorCapture = CaptureAndAnswer.create(rawState.getPosition());

    expect(function.getState(eq(10.0), capture(capture.getCapture()))).andAnswer(capture);
    expect(function.getPosition(eq(10.0), capture(vectorCapture.getCapture())))
        .andAnswer(vectorCapture);
    replay(function);

    StateVectorFunction f = StateVectorFunctions.scale(function, 0.001, 1000);

    StateVector result = f.getState(10.0, buffer);
    expected.setTo(new StateVector(new VectorIJK(1, 2, 3), new VectorIJK(-1000, -2000, -3000)));

    assertSame(buffer, result);
    assertEquivalentStateVector(expected, result);

    VectorIJK vectorResult = f.getPosition(10.0, buffer.getPosition());

    assertSame(buffer.getPosition(), vectorResult);
    assertEquivalentVector(expected.getPosition(), vectorResult);

    verify(function);

  }

  @Test(expected = IllegalArgumentException.class)
  public void testScaleNegativePositionScaleException() {
    StateVectorFunctions.scale(function, -1.0, 1.0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testScaleNegativeVelocityScaleException() {
    StateVectorFunctions.scale(function, 1.0, -1.0);
  }

  @Test
  public void testNegateStateEvaluation() {

    final StateVector rawState =
        new StateVector(new VectorIJK(1000, 2000, 3000), new VectorIJK(-1, -2, -3));

    capture = CaptureAndAnswer.create(rawState);
    expect(function.getState(eq(10.0), capture(capture.getCapture()))).andAnswer(capture);
    replay(function);

    StateVectorFunction f = StateVectorFunctions.negate(function);
    StateVector result = f.getState(10.0, buffer);
    expected.setTo(rawState).negate();
    assertSame(buffer, result);
    assertEquivalentStateVector(expected, result);
    verify(function);

    /*
     * Now test that the observer and target IDs are switched.
     */
    reset(function);
    expect(function.getTargetID()).andReturn(ephemerisID);
    replay(function);
    assertSame(ephemerisID, f.getObserverID());
    verify(function);

    reset(function);
    expect(function.getObserverID()).andReturn(ephemerisID);
    replay(function);
    assertSame(ephemerisID, f.getTargetID());
    verify(function);

  }

  @Test
  public void testNegatePositionEvaluation() {

    final VectorIJK rawVector = new VectorIJK(1000, 2000, 3000);

    vectorCapture = CaptureAndAnswer.create(rawVector);

    expect(function.getPosition(eq(10.0), capture(vectorCapture.getCapture())))
        .andAnswer(vectorCapture);
    replay(function);

    StateVectorFunction f = StateVectorFunctions.negate(function);
    VectorIJK result = f.getPosition(10.0, vectorBuffer);
    VectorIJK expected = new VectorIJK(rawVector);
    expected.negate();
    assertSame(vectorBuffer, result);
    assertEquivalentVector(expected, result);
    verify(function);

    /*
     * Now test that the observer and target IDs are switched.
     */
    reset(function);
    expect(function.getTargetID()).andReturn(ephemerisID);
    replay(function);
    assertSame(ephemerisID, f.getObserverID());
    verify(function);

    reset(function);
    expect(function.getObserverID()).andReturn(ephemerisID);
    replay(function);
    assertSame(ephemerisID, f.getTargetID());
    verify(function);

  }

  @Test
  public void testTransformStateTransformEvaluation() {

    final StateVector rawState =
        new StateVector(new VectorIJK(1000, 2000, 3000), new VectorIJK(2, 4, -6));

    capture = CaptureAndAnswer.create(rawState);

    final StateTransform rawTransform =
        new StateTransform(new RotationMatrixIJK(VectorIJK.J, VectorIJK.K, VectorIJK.I),
            new MatrixIJK(1.0, VectorIJK.I, 2.0, VectorIJK.J, 3.0, VectorIJK.K));

    CaptureAndAnswer<StateTransform> transformCapture = CaptureAndAnswer.create(rawTransform);

    expect(transformFunction.getFromID()).andReturn(frameID);
    expect(transformFunction.getCoverage()).andReturn(Coverages.create(0, 10));
    expect(function.getCoverage()).andReturn(Coverages.create(5, 15));
    expect(function.getFrameID()).andReturn(frameID);
    expect(function.getState(eq(10.0), capture(capture.getCapture()))).andAnswer(capture);
    expect(transformFunction.getStateTransform(eq(10.0), capture(transformCapture.getCapture())))
        .andAnswer(transformCapture);
    expect(transformFunction.getToID()).andReturn(frameID);

    replay(function, transformFunction);

    StateVectorFunction f = StateVectorFunctions.transform(transformFunction, function);

    StateVector result = f.getState(10.0, buffer);
    rawTransform.mxv(rawState, expected);

    assertSame(buffer, result);
    assertEquivalentStateVector(expected, result);

    assertEquals(Coverages.create(5, 10), f.getCoverage());
    assertSame(frameID, f.getFrameID());

    verify(function, transformFunction);

  }

  @Test
  public void testTransformTransformEvaluation() {

    final VectorIJK rawPosition = new VectorIJK(1000, 2000, 3000);

    vectorCapture = CaptureAndAnswer.create(rawPosition);

    final StateTransform rawTransform =
        new StateTransform(new RotationMatrixIJK(VectorIJK.J, VectorIJK.K, VectorIJK.I),
            new MatrixIJK(1.0, VectorIJK.I, 2.0, VectorIJK.J, 3.0, VectorIJK.K));

    CaptureAndAnswer<RotationMatrixIJK> frameCapture =
        CaptureAndAnswer.create(rawTransform.getRotation());

    expect(transformFunction.getFromID()).andReturn(frameID);
    expect(transformFunction.getCoverage()).andReturn(Coverages.create(0, 10));
    expect(function.getCoverage()).andReturn(Coverages.create(5, 15));
    expect(function.getFrameID()).andReturn(frameID);
    expect(function.getPosition(eq(10.0), capture(vectorCapture.getCapture())))
        .andAnswer(vectorCapture);
    expect(transformFunction.getTransform(eq(10.0), capture(frameCapture.getCapture())))
        .andAnswer(frameCapture);
    expect(transformFunction.getToID()).andReturn(frameID);

    replay(function, transformFunction);

    StateVectorFunction f = StateVectorFunctions.transform(transformFunction, function);

    VectorIJK expected = new VectorIJK();

    VectorIJK result = f.getPosition(10.0, vectorBuffer);
    rawTransform.getRotation().mxv(rawPosition, expected);

    assertSame(vectorBuffer, result);
    assertEquivalentVector(expected, result);
    assertEquals(Coverages.create(5, 10), f.getCoverage());
    assertSame(frameID, f.getFrameID());

    verify(function, transformFunction);

  }

  @Test(expected = IllegalArgumentException.class)
  public void testTransformFrameIDMismatchException() {
    expect(transformFunction.getFromID()).andReturn(frameID).atLeastOnce();
    expect(function.getFrameID()).andReturn(new SimpleFrameID("FAILURE")).atLeastOnce();

    replay(function);
    replay(transformFunction);

    StateVectorFunctions.transform(transformFunction, function);
  }

  @Test
  public void testCreateFixedAllCoveragePositionEvaluation() {

    VectorIJK fixed = new VectorIJK(1000, 2000, 3000);

    StateVectorFunction f = StateVectorFunctions.createFixed(ephemerisID, otherID, frameID, fixed);

    assertSame(ephemerisID, f.getTargetID());
    assertSame(otherID, f.getObserverID());
    assertSame(frameID, f.getFrameID());

    VectorIJK result = f.getPosition(-1000.0, vectorBuffer);
    assertSame(result, vectorBuffer);
    assertEquivalentVector(fixed, result);

    Coverage coverage = f.getCoverage();
    assertSame(coverage, Coverage.ALL_TIME);

  }

  @Test
  public void testCreateFixedAllCoverageStateEvaluation() {

    VectorIJK fixed = new VectorIJK(1000, 2000, 3000);

    StateVectorFunction f = StateVectorFunctions.createFixed(ephemerisID, otherID, frameID, fixed);

    assertSame(ephemerisID, f.getTargetID());
    assertSame(otherID, f.getObserverID());
    assertSame(frameID, f.getFrameID());

    StateVector result = f.getState(-1000.0, buffer);
    assertSame(result, buffer);
    assertEquivalentStateVector(new StateVector(fixed, VectorIJK.ZERO), result);

    Coverage coverage = f.getCoverage();
    assertSame(coverage, Coverage.ALL_TIME);

  }

  @Test
  public void testCreateFixedPositionEvaluation() {

    VectorIJK fixed = new VectorIJK(1000, 2000, 3000);

    expect(coverage.contains(-1000.0)).andReturn(true).atLeastOnce();
    expect(coverage.contains(1000.0)).andReturn(true);

    replay(coverage);

    StateVectorFunction f =
        StateVectorFunctions.createFixed(ephemerisID, otherID, frameID, coverage, fixed);

    assertSame(ephemerisID, f.getTargetID());
    assertSame(otherID, f.getObserverID());
    assertSame(frameID, f.getFrameID());
    assertSame(coverage, f.getCoverage());

    VectorIJK result = f.getPosition(-1000.0, vectorBuffer);
    assertEquivalentVector(fixed, result);
    assertSame(result, vectorBuffer);

    result = f.getPosition(1000.0, vectorBuffer);
    assertEquivalentVector(fixed, result);
    assertSame(result, vectorBuffer);

    fixed.setTo(VectorIJK.K);

    result = f.getPosition(-1000.0, vectorBuffer);
    assertEquivalentVector(new VectorIJK(1000, 2000, 3000), result);

    verify(coverage);

  }

  @Test
  public void testCreateFixedStateEvaluation() {

    VectorIJK fixed = new VectorIJK(1000, 2000, 3000);

    expect(coverage.contains(-1000.0)).andReturn(true).atLeastOnce();
    expect(coverage.contains(1000.0)).andReturn(true);

    replay(coverage);

    StateVectorFunction f =
        StateVectorFunctions.createFixed(ephemerisID, otherID, frameID, coverage, fixed);

    assertSame(ephemerisID, f.getTargetID());
    assertSame(otherID, f.getObserverID());
    assertSame(frameID, f.getFrameID());
    assertSame(coverage, f.getCoverage());

    StateVector result = f.getState(-1000.0, buffer);
    assertEquivalentStateVector(new StateVector(fixed, VectorIJK.ZERO), result);
    assertSame(result, buffer);

    result = f.getState(1000.0, buffer);
    assertEquivalentStateVector(new StateVector(fixed, VectorIJK.ZERO), result);
    assertSame(result, buffer);

    fixed.setTo(VectorIJK.K);

    result = f.getState(-1000.0, buffer);
    assertEquivalentStateVector(new StateVector(new VectorIJK(1000, 2000, 3000), VectorIJK.ZERO),
        result);

    verify(coverage);

  }

  @Test(expected = EphemerisEvaluationException.class)
  public void testCreateFixedPositionLookupException() {

    VectorIJK fixed = new VectorIJK(1000, 2000, 3000);

    expect(coverage.contains(0.0)).andReturn(false);

    replay(coverage);

    StateVectorFunction f =
        StateVectorFunctions.createFixed(ephemerisID, otherID, frameID, coverage, fixed);

    f.getPosition(0.0, buffer.getPosition());

  }

  @Test(expected = EphemerisEvaluationException.class)
  public void testCreateFixedStateLookupException() {

    VectorIJK fixed = new VectorIJK(1000, 2000, 3000);

    expect(coverage.contains(0.0)).andReturn(false);

    replay(coverage);

    StateVectorFunction f =
        StateVectorFunctions.createFixed(ephemerisID, otherID, frameID, coverage, fixed);

    f.getState(0.0, buffer);

  }

  @Test
  public void testCreateQuadraticDerivative() {

    StateVectorFunction source = new StateVectorFunction() {

      @Override
      public EphemerisID getTargetID() {
        return new SimpleEphemerisID("TARGET");
      }

      @Override
      public VectorIJK getPosition(double time, VectorIJK buffer) {
        buffer.setTo(time * time * time, 2 * time * time * time, 3 * time * time * time);
        return buffer;
      }

      @Override
      public EphemerisID getObserverID() {
        return new SimpleEphemerisID("OBSERVER");
      }

      @Override
      public FrameID getFrameID() {
        return new SimpleFrameID("FRAME");
      }

      @Override
      public Coverage getCoverage() {
        return Coverage.ALL_TIME;
      }

      @Override
      public StateVector getState(double time, StateVector buffer) {
        getPosition(time, buffer.getPosition());
        buffer.getVelocity().setTo(3 * time * time, 6 * time * time, 9 * time * time);
        return buffer;
      }
    };


    StateVectorFunction underTest = StateVectorFunctions.createQuadraticDerivative(source);

    assertEquals(new SimpleEphemerisID("TARGET"), underTest.getTargetID());
    assertEquals(new SimpleEphemerisID("OBSERVER"), underTest.getObserverID());
    assertEquals(new SimpleFrameID("FRAME"), underTest.getFrameID());
    assertSame(Coverage.ALL_TIME, underTest.getCoverage());

    VectorIJK exVec = new VectorIJK(12, 24, 36);
    AssertTools.assertComponentRelativeEquality(exVec, underTest.getPosition(2.0, new VectorIJK()),
        1e-14);

    StateVector exState = new StateVector(exVec, new VectorIJK(12, 24, 36));
    AssertTools.assertComponentRelativeEquality(exState, underTest.getState(2.0, new StateVector()),
        1e-14);

  }

}
