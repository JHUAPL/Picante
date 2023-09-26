package picante.mechanics.utilities;

import static org.junit.Assert.assertSame;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;

public class AbstractPositionVectorFunctionTest {

  private AbstractPositionVectorFunction function;
  private SimpleFrameID frameID;
  private SimpleEphemerisID targetID;
  private SimpleEphemerisID observerID;
  private Coverage coverage;

  @Before
  public void setUp() throws Exception {
    this.frameID = new SimpleFrameID("FRAME");
    this.targetID = new SimpleEphemerisID("TARGET");
    this.observerID = new SimpleEphemerisID("OBSERVER");
    this.coverage = Coverage.ALL_TIME;
    this.function = new AbstractPositionVectorFunction(targetID, observerID, frameID, coverage) {

      @Override
      public VectorIJK getPosition(@SuppressWarnings("unused") double time,
          @SuppressWarnings("unused") VectorIJK buffer) {
        throw new UnsupportedOperationException();
      }
    };
  }

  @Test
  public void testGetObserverID() {
    assertSame(observerID, function.getObserverID());
  }

  @Test
  public void testGetTargetID() {
    assertSame(targetID, function.getTargetID());
  }

  @Test
  public void testGetFrameID() {
    assertSame(frameID, function.getFrameID());
  }

  @Test
  public void testGetCoverage() {
    assertSame(coverage, function.getCoverage());
  }

}
