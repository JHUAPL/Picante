package picante.mechanics.utilities;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.Coverages;
import picante.mechanics.EphemerisEvaluationException;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.StateVector;
import picante.mechanics.UnwritableStateVector;

@SuppressWarnings("deprecation")
public class FixedOffsetPositionFunctionTest {

  private FixedOffsetPositionFunction allTime;
  private FixedOffsetPositionFunction func;

  private EphemerisID observer;
  private EphemerisID target;
  private FrameID frame;

  private Coverage coverage;
  private UnwritableVectorIJK vector;
  private UnwritableStateVector state;

  @Before
  public void setUp() throws Exception {
    coverage = Coverages.create(-10, 10);
    observer = new SimpleEphemerisID("OBSERVER");
    target = new SimpleEphemerisID("TARGET");
    frame = new SimpleFrameID("FRAME");

    vector = VectorIJK.K;
    state = new UnwritableStateVector(vector, VectorIJK.ZERO);

    allTime = new FixedOffsetPositionFunction(target, observer, frame, Coverage.ALL_TIME, vector);
    func = new FixedOffsetPositionFunction(target, observer, frame, coverage, vector);
  }

  @Test
  public void testFixedOffsetPositionFunctionEphemerisIDEphemerisIDFrameIDUnwritableVectorIJK() {
    FixedOffsetPositionFunction newFunc =
        new FixedOffsetPositionFunction(target, observer, frame, vector);
    assertSame(target, newFunc.getTargetID());
    assertSame(observer, newFunc.getObserverID());
    assertSame(frame, newFunc.getFrameID());
    assertEquals(vector, newFunc.getPosition(0.0, new VectorIJK()));
    assertEquals(state, newFunc.getState(0.0, new StateVector()));
    assertSame(Coverage.ALL_TIME, newFunc.getCoverage());
  }

  @Test
  public void testGetObserverID() {
    assertSame(observer, func.getObserverID());
    assertSame(observer, allTime.getObserverID());
  }

  @Test
  public void testGetTargetID() {
    assertSame(target, func.getTargetID());
    assertSame(target, allTime.getTargetID());
  }

  @Test
  public void testGetFrameID() {
    assertSame(frame, func.getFrameID());
    assertSame(frame, allTime.getFrameID());
  }

  @Test
  public void testGetCoverage() {
    assertSame(Coverage.ALL_TIME, allTime.getCoverage());
    assertSame(coverage, func.getCoverage());
  }

  @Test
  public void testGetPosition() {
    assertEquals(vector, allTime.getPosition(0.0, new VectorIJK()));
    assertEquals(vector, func.getPosition(0.0, new VectorIJK()));
  }

  @Test(expected = EphemerisEvaluationException.class)
  public void testGetPositionEvaluationException() {
    func.getPosition(-100, new VectorIJK());
  }

  @Test
  public void testGetState() {
    assertEquals(state, allTime.getState(0.0, new StateVector()));
    assertEquals(state, func.getState(0.0, new StateVector()));
  }

  @Test(expected = EphemerisEvaluationException.class)
  public void testGetStateEvaluationException() {
    func.getState(-100, new StateVector());
  }

}
