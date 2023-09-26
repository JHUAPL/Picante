package picante.spice.kernel.pck;

import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;
import picante.math.intervals.Interval;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.StateTransform;

public class AbstractPCKSegmentTest {

  private ConcreteSegment segment;

  @Before
  public void setUp() {
    segment = new ConcreteSegment("name", 3000, 17, 12345, 67890);
  }

  @Test
  public void testGetBodyFrameID() {
    assertEquals(3000, segment.getBodyFrameID());
  }

  @Test
  public void testGetReferenceFrameID() {
    assertEquals(17, segment.getReferenceFrameID());
  }

  @Test
  public void testGetName() {
    assertEquals("name", segment.getName());
  }

  @Test
  public void testGetCoverage() {
    Coverage coverage = segment.getCoverage();
    Interval interval = coverage.getBoundingInterval(new Interval());

    assertEquals(12345, interval.getBegin(), 0.0);
    assertEquals(67890, interval.getEnd(), 0.0);
  }
}


class ConcreteSegment extends AbstractPCKSegment {

  public ConcreteSegment(String name, int bodyFrameID, int referenceFrameID, double initialET,
      double finalET) {
    super(name, bodyFrameID, referenceFrameID, initialET, finalET);
  }

  @Override
  public RotationMatrixIJK getTransform(@SuppressWarnings("unused") double encodedSCLK,
      @SuppressWarnings("unused") RotationMatrixIJK buffer) {
    throw new UnsupportedOperationException();
  }

  @Override
  public int getType() {
    throw new UnsupportedOperationException();
  }

  public boolean hasAngularVelocity() {
    throw new UnsupportedOperationException();
  }

  @SuppressWarnings("unused")
  @Override
  public StateTransform getTransform(double time, StateTransform buffer) {
    throw new UnsupportedOperationException();
  }

}
