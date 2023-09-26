package picante.mechanics.utilities;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.Coverages;
import picante.mechanics.FrameEvaluationException;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.mechanics.UnwritableStateTransform;

@SuppressWarnings("deprecation")
public class FixedOffsetFrameFunctionTest {

  private FixedOffsetFrameFunction allTime;
  private FixedOffsetFrameFunction func;

  private Coverage coverage;
  private UnwritableRotationMatrixIJK rotation;
  private UnwritableStateTransform transform;

  private FrameID from;
  private FrameID to;

  @Before
  public void setUp() throws Exception {
    coverage = Coverages.create(0, 10);
    rotation = RotationMatrixIJK.IDENTITY;
    transform = new UnwritableStateTransform(rotation, MatrixIJK.ZEROS);
    from = new SimpleFrameID("FROM");
    to = new SimpleFrameID("TO");
    allTime = new FixedOffsetFrameFunction(from, to, Coverage.ALL_TIME, rotation);
    func = new FixedOffsetFrameFunction(from, to, coverage, rotation);
  }

  @Test
  public void testFixedOffsetFrameFunctionFrameIDFrameIDUnwritableRotationMatrixIJK() {
    FixedOffsetFrameFunction newFunc =
        new FixedOffsetFrameFunction(from, to, RotationMatrixIJK.IDENTITY);
    assertEquals(from, newFunc.getFromID());
    assertEquals(to, newFunc.getToID());
    assertEquals(rotation, newFunc.getTransform(0.0, new RotationMatrixIJK()));
    assertEquals(transform, newFunc.getStateTransform(0.0, new StateTransform()));
    assertSame(Coverage.ALL_TIME, newFunc.getCoverage());
  }

  @Test
  public void testGetFromID() {
    assertSame(from, allTime.getFromID());
    assertSame(from, func.getFromID());
  }

  @Test
  public void testGetToID() {
    assertSame(to, allTime.getToID());
    assertSame(to, func.getToID());
  }

  @Test
  public void testGetCoverage() {
    assertSame(Coverage.ALL_TIME, allTime.getCoverage());
    assertSame(coverage, func.getCoverage());
  }

  @Test
  public void testGetTransform() {
    assertEquals(rotation, allTime.getTransform(0.0, new RotationMatrixIJK()));
    assertEquals(rotation, func.getTransform(5.0, new RotationMatrixIJK()));
  }

  @Test(expected = FrameEvaluationException.class)
  public void testGetTransformEvaluationException() {
    func.getTransform(-10, new RotationMatrixIJK());
  }

  @Test
  public void testGetStateTransform() {
    assertEquals(transform, allTime.getStateTransform(0.0, new StateTransform()));
    assertEquals(transform, func.getStateTransform(5.0, new StateTransform()));

  }

  @Test(expected = FrameEvaluationException.class)
  public void testGetStateTransformEvaluationException() {
    func.getStateTransform(-10, new StateTransform());
  }

}
