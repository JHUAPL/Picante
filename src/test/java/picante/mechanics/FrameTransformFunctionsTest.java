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
import static picante.junit.AssertTools.assertEquivalentMatrix;
import org.junit.Before;
import org.junit.Test;
import picante.junit.CaptureAndAnswer;
import picante.math.functions.RotationMatrixIJKFunction;
import picante.math.functions.RotationMatrixIJKFunctions;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.rotations.EulerAngles;
import picante.mechanics.utilities.SimpleFrameID;

public class FrameTransformFunctionsTest {

  private SimpleFrameID frameID;
  private SimpleFrameID otherID;
  private SimpleFrameID anotherID;
  private RotationMatrixIJK buffer;
  private Coverage coverage;
  private FrameTransformFunction function;
  private FrameTransformFunction anotherFunction;

  @Before
  public void setUp() throws Exception {
    frameID = new SimpleFrameID("ID");
    otherID = new SimpleFrameID("OTHER");
    anotherID = new SimpleFrameID("ANOTHER");
    buffer = new RotationMatrixIJK();
    coverage = createMock(Coverage.class);
    function = createMock(FrameTransformFunction.class);
    anotherFunction = createMock(FrameTransformFunction.class);
  }

  @Test
  public void testInvert() {

    final RotationMatrixIJK rawRotation =
        new RotationMatrixIJK(VectorIJK.J, VectorIJK.I, VectorIJK.MINUS_K);

    CaptureAndAnswer<RotationMatrixIJK> capture = CaptureAndAnswer.create(rawRotation);

    expect(function.getTransform(eq(10.0), capture(capture.getCapture()))).andAnswer(capture);
    replay(function);

    FrameTransformFunction f = FrameTransformFunctions.invert(function);
    RotationMatrixIJK result = f.getTransform(10.0, buffer);
    RotationMatrixIJK expected = new RotationMatrixIJK(rawRotation);
    expected.transpose();

    assertSame(buffer, result);
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
  public void testCreateFixedAllTime() {

    RotationMatrixIJK fixed = new RotationMatrixIJK(VectorIJK.J, VectorIJK.I, VectorIJK.MINUS_K);

    FrameTransformFunction f = FrameTransformFunctions.createFixed(frameID, otherID, fixed);

    assertSame(frameID, f.getFromID());
    assertSame(otherID, f.getToID());

    RotationMatrixIJK result = f.getTransform(10.0, buffer);
    assertSame(result, buffer);
    assertEquivalentMatrix(result, fixed);

    Coverage coverage = f.getCoverage();
    assertSame(coverage, Coverage.ALL_TIME);

    /*
     * Alter fixed and verify that it has no impact on f.
     */
    RotationMatrixIJK fixedCopy = new RotationMatrixIJK(fixed);

    fixed.setTo(RotationMatrixIJK.IDENTITY);

    result = f.getTransform(11.0, buffer);
    assertSame(result, buffer);
    assertEquivalentMatrix(fixedCopy, result);

  }

  @Test
  public void testCreateFixed() {

    RotationMatrixIJK fixed = new RotationMatrixIJK(VectorIJK.J, VectorIJK.I, VectorIJK.MINUS_K);

    expect(coverage.contains(-1000.0)).andReturn(true).atLeastOnce();
    expect(coverage.contains(1000.0)).andReturn(true);

    replay(coverage);

    FrameTransformFunction f =
        FrameTransformFunctions.createFixed(frameID, otherID, coverage, fixed);

    assertSame(frameID, f.getFromID());
    assertSame(otherID, f.getToID());
    assertSame(coverage, f.getCoverage());

    RotationMatrixIJK result = f.getTransform(-1000.0, buffer);
    assertSame(result, buffer);
    assertEquivalentMatrix(fixed, result);

    result = f.getTransform(1000.0, buffer);
    assertSame(result, buffer);
    assertEquivalentMatrix(fixed, result);

    RotationMatrixIJK fixedCopy = new RotationMatrixIJK(fixed);

    fixed.setTo(RotationMatrixIJK.IDENTITY);

    result = f.getTransform(-1000.0, buffer);
    assertSame(result, buffer);
    assertEquivalentMatrix(fixedCopy, result);

    verify(coverage);

  }

  @Test
  public void testAdaptFrameTransformFunction() {

    expect(coverage.contains(-1000.0)).andReturn(true).atLeastOnce();
    expect(coverage.contains(1000.0)).andReturn(true);

    replay(coverage);

    RotationMatrixIJK rotation = new RotationMatrixIJK(VectorIJK.J, VectorIJK.I, VectorIJK.MINUS_K);

    FrameTransformFunction frameFunction =
        FrameTransformFunctions.createFixed(frameID, otherID, coverage, rotation);

    RotationMatrixIJKFunction rotFunct = FrameTransformFunctions.adapt(frameFunction);

    assertSame(frameID, frameFunction.getFromID());
    assertSame(otherID, frameFunction.getToID());
    assertSame(coverage, frameFunction.getCoverage());

    RotationMatrixIJK result = rotFunct.evaluate(-1000.0, buffer);
    assertSame(result, buffer);
    assertEquivalentMatrix(rotation, result);

    result = rotFunct.evaluate(1000.0, buffer);
    assertSame(result, buffer);
    assertEquivalentMatrix(rotation, result);

    RotationMatrixIJK fixedCopy = new RotationMatrixIJK(rotation);

    rotation.setTo(RotationMatrixIJK.IDENTITY);

    result = rotFunct.evaluate(-1000.0, buffer);
    assertSame(result, buffer);
    assertEquivalentMatrix(fixedCopy, result);

    verify(coverage);
  }

  @Test
  public void testAdaptRotationMatrixIJKFunctionAllTime() {

    RotationMatrixIJK rotation = new RotationMatrixIJK(VectorIJK.J, VectorIJK.I, VectorIJK.MINUS_K);

    RotationMatrixIJKFunction rotFunct = RotationMatrixIJKFunctions.create(rotation);

    FrameTransformFunction frameFunction =
        FrameTransformFunctions.adapt(frameID, otherID, rotFunct);

    assertSame(frameID, frameFunction.getFromID());
    assertSame(otherID, frameFunction.getToID());

    RotationMatrixIJK result = frameFunction.getTransform(10.0, buffer);
    assertSame(result, buffer);
    assertEquivalentMatrix(result, rotation);

    Coverage coverage = frameFunction.getCoverage();
    assertSame(coverage, Coverage.ALL_TIME);

    /*
     * Alter fixed and verify that it has no impact on f.
     */
    RotationMatrixIJK fixedCopy = new RotationMatrixIJK(rotation);

    rotation.setTo(RotationMatrixIJK.IDENTITY);

    result = frameFunction.getTransform(11.0, buffer);
    assertSame(result, buffer);
    assertEquivalentMatrix(fixedCopy, result);

  }

  @Test
  public void testAdaptRotationMatrixIJKFunction() {

    RotationMatrixIJK rotation = new RotationMatrixIJK(VectorIJK.J, VectorIJK.I, VectorIJK.MINUS_K);

    expect(coverage.contains(-1000.0)).andReturn(true).atLeastOnce();
    expect(coverage.contains(1000.0)).andReturn(true);

    replay(coverage);

    RotationMatrixIJKFunction rotFunct = RotationMatrixIJKFunctions.create(rotation);

    FrameTransformFunction frameFunction =
        FrameTransformFunctions.adapt(frameID, otherID, coverage, rotFunct);

    assertSame(frameID, frameFunction.getFromID());
    assertSame(otherID, frameFunction.getToID());
    assertSame(coverage, frameFunction.getCoverage());

    RotationMatrixIJK result = rotFunct.evaluate(-1000.0, buffer);
    assertSame(result, buffer);
    assertEquivalentMatrix(rotation, result);

    result = frameFunction.getTransform(1000.0, buffer);
    assertSame(result, buffer);
    assertEquivalentMatrix(rotation, result);

    RotationMatrixIJK fixedCopy = new RotationMatrixIJK(rotation);

    rotation.setTo(RotationMatrixIJK.IDENTITY);

    result = frameFunction.getTransform(-1000.0, buffer);
    assertSame(result, buffer);
    assertEquivalentMatrix(fixedCopy, result);

    verify(coverage);

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
    FrameTransformFunctions.mxm(function, anotherFunction);

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

    FrameTransformFunctions.mxm(function, anotherFunction);

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

    expect(anotherFunction.getFromID()).andReturn(otherID).anyTimes();
    expect(anotherFunction.getToID()).andReturn(frameID).anyTimes();
    expect(anotherFunction.getCoverage()).andReturn(Coverages.create(0, 10));
    final UnwritableRotationMatrixIJK anotherFunctionMatrix = UnwritableRotationMatrixIJK
        .copyOf(new EulerAngles.KIJ(0.15, 0.0, 0.31).getRotation(new RotationMatrixIJK()));
    CaptureAndAnswer<RotationMatrixIJK> anotherFunctionCapture =
        CaptureAndAnswer.create(anotherFunctionMatrix);
    expect(anotherFunction.getTransform(eq(0.0), capture(anotherFunctionCapture.getCapture())))
        .andAnswer(anotherFunctionCapture);

    replay(function, anotherFunction);

    FrameTransformFunction mxm = FrameTransformFunctions.mxm(function, anotherFunction);

    assertEquals(anotherID, mxm.getToID());
    assertEquals(otherID, mxm.getFromID());
    /*
     * We can use equals() because this fundamentally is executing the same code to perform the
     * matrix multiply.
     */
    assertEquals(RotationMatrixIJK.mxm(functionMatrix, anotherFunctionMatrix),
        mxm.getTransform(0.0, new RotationMatrixIJK()));
    assertEquals(Coverages.create(0, 10), mxm.getCoverage());

    verify(function, anotherFunction);

  }

  @Test(expected = IllegalArgumentException.class)
  public void testMxmtIncompatibleIDException() {

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
    FrameTransformFunctions.mxmt(function, anotherFunction);

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

    FrameTransformFunctions.mxmt(function, anotherFunction);

  }

  @Test
  public void testMxmt() {

    expect(function.getFromID()).andReturn(frameID).anyTimes();
    expect(function.getToID()).andReturn(anotherID).anyTimes();
    expect(function.getCoverage()).andReturn(Coverages.create(0, 10));
    final UnwritableRotationMatrixIJK functionMatrix = UnwritableRotationMatrixIJK
        .copyOf(new EulerAngles.IJI(0.1, 0.2, 0.3).getRotation(new RotationMatrixIJK()));
    CaptureAndAnswer<RotationMatrixIJK> functionCapture = CaptureAndAnswer.create(functionMatrix);
    expect(function.getTransform(eq(0.0), capture(functionCapture.getCapture())))
        .andAnswer(functionCapture);

    expect(anotherFunction.getFromID()).andReturn(frameID).anyTimes();
    expect(anotherFunction.getToID()).andReturn(otherID).anyTimes();
    expect(anotherFunction.getCoverage()).andReturn(Coverages.create(0, 10));
    final UnwritableRotationMatrixIJK anotherFunctionMatrix = UnwritableRotationMatrixIJK
        .copyOf(new EulerAngles.KIJ(0.15, 0.0, 0.31).getRotation(new RotationMatrixIJK()));
    CaptureAndAnswer<RotationMatrixIJK> anotherFunctionCapture =
        CaptureAndAnswer.create(anotherFunctionMatrix);
    expect(anotherFunction.getTransform(eq(0.0), capture(anotherFunctionCapture.getCapture())))
        .andAnswer(anotherFunctionCapture);

    replay(function, anotherFunction);

    FrameTransformFunction mxmt = FrameTransformFunctions.mxmt(function, anotherFunction);

    assertEquals(anotherID, mxmt.getToID());
    assertEquals(otherID, mxmt.getFromID());
    /*
     * We can use equals() because this fundamentally is executing the same code to perform the
     * matrix multiply.
     */
    assertEquals(RotationMatrixIJK.mxmt(functionMatrix, anotherFunctionMatrix),
        mxmt.getTransform(0.0, new RotationMatrixIJK()));
    assertEquals(Coverages.create(0, 10), mxmt.getCoverage());

    verify(function, anotherFunction);

  }

  @Test(expected = IllegalArgumentException.class)
  public void testMtxmIncompatibleIDException() {

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
    FrameTransformFunctions.mtxm(function, anotherFunction);

  }

  @Test(expected = IllegalArgumentException.class)
  public void testMtxmIncompatibleCoverageException() {

    expect(function.getFromID()).andReturn(anotherID).anyTimes();
    expect(function.getToID()).andReturn(frameID).anyTimes();
    expect(function.getCoverage()).andReturn(Coverages.create(0, 10));

    expect(anotherFunction.getFromID()).andReturn(otherID).anyTimes();
    expect(anotherFunction.getToID()).andReturn(frameID).anyTimes();
    expect(anotherFunction.getCoverage()).andReturn(Coverages.create(20, 30));

    replay(function, anotherFunction);

    FrameTransformFunctions.mtxm(function, anotherFunction);

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

    expect(anotherFunction.getFromID()).andReturn(otherID).anyTimes();
    expect(anotherFunction.getToID()).andReturn(frameID).anyTimes();
    expect(anotherFunction.getCoverage()).andReturn(Coverages.create(0, 10));
    final UnwritableRotationMatrixIJK anotherFunctionMatrix = UnwritableRotationMatrixIJK
        .copyOf(new EulerAngles.KIJ(0.15, 0.0, 0.31).getRotation(new RotationMatrixIJK()));
    CaptureAndAnswer<RotationMatrixIJK> anotherFunctionCapture =
        CaptureAndAnswer.create(anotherFunctionMatrix);
    expect(anotherFunction.getTransform(eq(0.0), capture(anotherFunctionCapture.getCapture())))
        .andAnswer(anotherFunctionCapture);

    replay(function, anotherFunction);

    FrameTransformFunction mtxm = FrameTransformFunctions.mtxm(function, anotherFunction);

    assertEquals(anotherID, mtxm.getToID());
    assertEquals(otherID, mtxm.getFromID());
    /*
     * We can use equals() because this fundamentally is executing the same code to perform the
     * matrix multiply.
     */
    assertEquals(RotationMatrixIJK.mtxm(functionMatrix, anotherFunctionMatrix),
        mtxm.getTransform(0.0, new RotationMatrixIJK()));
    assertEquals(Coverages.create(0, 10), mtxm.getCoverage());

    verify(function, anotherFunction);

  }


}
