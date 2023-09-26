package picante.mechanics.utilities;

import static org.junit.Assert.assertSame;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;

public class AbstractFrameTransformFunctionTest {

  private AbstractFrameTransformFunction function;
  private SimpleFrameID fromID;
  private SimpleFrameID toID;
  private Coverage coverage;

  @Before
  public void setUp() throws Exception {
    this.fromID = new SimpleFrameID("FROM");
    this.toID = new SimpleFrameID("TO");
    this.coverage = Coverage.ALL_TIME;
    function = new AbstractFrameTransformFunction(fromID, toID, coverage) {

      @Override
      public RotationMatrixIJK getTransform(@SuppressWarnings("unused") double time,
          @SuppressWarnings("unused") RotationMatrixIJK buffer) {
        throw new UnsupportedOperationException();
      }
    };
  }

  @Test
  public void testGetCoverage() {
    assertSame(coverage, function.getCoverage());
  }

  @Test
  public void testGetFromID() {
    assertSame(fromID, function.getFromID());
  }

  @Test
  public void testGetToID() {
    assertSame(toID, function.getToID());
  }

}
