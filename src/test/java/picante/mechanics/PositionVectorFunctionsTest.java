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
import static picante.junit.AssertTools.assertEquivalentVector;
import org.junit.Before;
import org.junit.Test;
import picante.junit.CaptureAndAnswer;
import picante.math.functions.VectorIJKFunction;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.utilities.SimpleEphemerisID;
import picante.mechanics.utilities.SimpleFrameID;

public class PositionVectorFunctionsTest {

  private PositionVectorFunction function;
  private CaptureAndAnswer<VectorIJK> capture;
  private CaptureAndAnswer<RotationMatrixIJK> rotationCapture;
  private VectorIJK buffer;
  private VectorIJK expected;
  private SimpleEphemerisID ephemerisID;
  private SimpleEphemerisID otherID;
  private SimpleFrameID frameID;
  private FrameTransformFunction frameFunction;
  private Coverage coverage;

  @Before
  public void setUp() throws Exception {
    ephemerisID = new SimpleEphemerisID("ID");
    otherID = new SimpleEphemerisID("OTHER");
    frameID = new SimpleFrameID("ID");
    function = createMock(PositionVectorFunction.class);
    frameFunction = createMock(FrameTransformFunction.class);
    coverage = createMock(Coverage.class);
    buffer = new VectorIJK();
    expected = new VectorIJK();
  }

  @Test
  public void testAdapt() {

    final VectorIJK rawVector = new VectorIJK(100, 200, 300);

    capture = CaptureAndAnswer.create(rawVector);

    expect(function.getPosition(eq(10.0), capture(capture.getCapture()))).andAnswer(capture);

    replay(function);

    VectorIJKFunction f = PositionVectorFunctions.adapt(function);

    VectorIJK result = f.evaluate(10.0, buffer);

    assertSame(result, buffer);
    assertEquivalentVector(rawVector, result);

    verify(function);

  }

  @Test
  public void testScale() {

    /*
     * Setup the value that will be returned from the mocked position function.
     */
    final VectorIJK rawPosition = new VectorIJK(1000, 2000, 3000);

    capture = CaptureAndAnswer.create(rawPosition);

    expect(function.getPosition(eq(10.0), capture(capture.getCapture()))).andAnswer(capture);
    replay(function);

    PositionVectorFunction f = PositionVectorFunctions.scale(function, 0.001);

    VectorIJK result = f.getPosition(10.0, buffer);
    expected.setTo(1, 2, 3);

    assertSame(buffer, result);
    assertEquivalentVector(expected, result);

    verify(function);

  }

  @Test(expected = IllegalArgumentException.class)
  public void testScaleNegativeFactorException() {
    PositionVectorFunctions.scale(function, -1.0);
  }

  @Test
  public void testNegate() {

    final VectorIJK rawPosition = new VectorIJK(1000, 2000, 3000);
    capture = CaptureAndAnswer.create(rawPosition);
    expect(function.getPosition(eq(10.0), capture(capture.getCapture()))).andAnswer(capture);
    replay(function);
    PositionVectorFunction f = PositionVectorFunctions.negate(function);
    VectorIJK result = f.getPosition(10.0, buffer);
    expected.setToNegated(rawPosition);
    assertSame(buffer, result);
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
  public void testTransform() {

    final VectorIJK rawPosition = new VectorIJK(1000, 2000, 3000);

    capture = CaptureAndAnswer.create(rawPosition);
    final RotationMatrixIJK rawRotation =
        new RotationMatrixIJK(VectorIJK.J, VectorIJK.K, VectorIJK.I);

    rotationCapture = CaptureAndAnswer.create(rawRotation);

    expect(frameFunction.getFromID()).andReturn(frameID);
    expect(function.getCoverage()).andReturn(Coverages.create(0, 10));
    expect(function.getFrameID()).andReturn(frameID);
    expect(function.getPosition(eq(10.0), capture(capture.getCapture()))).andAnswer(capture);
    expect(frameFunction.getTransform(eq(10.0), capture(rotationCapture.getCapture())))
        .andAnswer(rotationCapture);
    expect(frameFunction.getToID()).andReturn(frameID);
    expect(frameFunction.getCoverage()).andReturn(Coverages.create(5, 15));

    replay(function, frameFunction);

    PositionVectorFunction f = PositionVectorFunctions.transform(frameFunction, function);

    VectorIJK result = f.getPosition(10.0, buffer);
    rawRotation.mxv(rawPosition, expected);

    assertSame(buffer, result);
    assertEquivalentVector(expected, result);

    assertSame(frameID, f.getFrameID());
    assertEquals(Coverages.create(5, 10), f.getCoverage());

    verify(function, frameFunction);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testTransformFrameIDMismatchException() {

    expect(frameFunction.getFromID()).andReturn(frameID).atLeastOnce();
    expect(function.getFrameID()).andReturn(new SimpleFrameID("FAILURE")).atLeastOnce();

    replay(function);
    replay(frameFunction);

    PositionVectorFunctions.transform(frameFunction, function);

  }

  @Test
  public void testCreateFixedAllCoverage() {

    VectorIJK fixed = new VectorIJK(1000, 2000, 3000);

    PositionVectorFunction f =
        PositionVectorFunctions.createFixed(ephemerisID, otherID, frameID, fixed);

    assertSame(ephemerisID, f.getTargetID());
    assertSame(otherID, f.getObserverID());
    assertSame(frameID, f.getFrameID());

    VectorIJK result = f.getPosition(-1000.0, buffer);
    assertSame(result, buffer);
    assertEquivalentVector(fixed, result);

    Coverage coverage = f.getCoverage();
    assertSame(coverage, Coverage.ALL_TIME);

  }

  @Test
  public void testCreateFixed() {

    VectorIJK fixed = new VectorIJK(1000, 2000, 3000);

    expect(coverage.contains(-1000.0)).andReturn(true).atLeastOnce();
    expect(coverage.contains(1000.0)).andReturn(true);

    replay(coverage);

    PositionVectorFunction f =
        PositionVectorFunctions.createFixed(ephemerisID, otherID, frameID, coverage, fixed);

    assertSame(ephemerisID, f.getTargetID());
    assertSame(otherID, f.getObserverID());
    assertSame(frameID, f.getFrameID());
    assertSame(coverage, f.getCoverage());

    /*
     * First determine that the function returns the fixed vector for a series of times.
     */
    VectorIJK result = f.getPosition(-1000.0, buffer);
    assertEquivalentVector(fixed, result);
    assertSame(result, buffer);

    result = f.getPosition(1000.0, buffer);
    assertEquivalentVector(fixed, result);
    assertSame(result, buffer);

    /*
     * Now change fixed and make certain it does not result in the output being altered.
     */
    fixed.setTo(VectorIJK.K);

    result = f.getPosition(-1000.0, buffer);
    assertEquivalentVector(new VectorIJK(1000, 2000, 3000), result);

    verify(coverage);
  }

  @Test(expected = EphemerisEvaluationException.class)
  public void testCreateFixedLookupException() {

    VectorIJK fixed = new VectorIJK(1000, 2000, 3000);

    expect(coverage.contains(0.0)).andReturn(false);

    replay(coverage);

    PositionVectorFunction f =
        PositionVectorFunctions.createFixed(ephemerisID, otherID, frameID, coverage, fixed);

    f.getPosition(0.0, buffer);

  }

}
