package picante.spice.kernel.spk;

import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;
import picante.math.intervals.Interval;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.StateVector;

public class AbstractSPKSegmentTest {

  private AbstractSPKSegment segment;
  private AbstractSPKSegment segment2;

  @Before
  public void setUp() throws Exception {
    segment =
        new ConcreteSPKSegment("Segment", -82, 6, 1, 2.2896000100000E+08, 2.2999680000000E+08, 1);
    segment2 =
        new ConcreteSPKSegment("Segment2", 399, 3, 1, 2.2896000100000E+08, 2.2999680000000E+08, 2);
  }

  @Test
  public void testGetCoverage() {
    Coverage coverage = segment.getCoverage();
    Interval bounding = coverage.getBoundingInterval(new Interval());
    assertEquals(2.2896000100000E+08, bounding.getBegin(), 0.0);
    assertEquals(2.2999680000000E+08, bounding.getEnd(), 0.0);

    coverage = segment2.getCoverage();
    bounding = coverage.getBoundingInterval(new Interval());
    assertEquals(2.2896000100000E+08, bounding.getBegin(), 0.0);
    assertEquals(2.2999680000000E+08, bounding.getEnd(), 0.0);
  }

  @Test
  public void testGetName() {
    assertEquals("Segment", segment.getName());
    assertEquals("Segment2", segment2.getName());
  }

  @Test
  public void testGetFrameID() {
    assertEquals(1, segment.getFrameID());
    assertEquals(1, segment2.getFrameID());
  }

  @Test
  public void testGetObserverID() {
    assertEquals(6, segment.getObserverID());
    assertEquals(3, segment2.getObserverID());
  }

  @Test
  public void testGetTargetID() {
    assertEquals(-82, segment.getTargetID());
    assertEquals(399, segment2.getTargetID());
  }

  @Test
  public void testGetType() {
    assertEquals(1, segment.getType());
    assertEquals(2, segment2.getType());
  }

}


class ConcreteSPKSegment extends AbstractSPKSegment {

  private final int type;

  public ConcreteSPKSegment(String name, int targetID, int observerID, int frameID, double startET,
      double finalET, int type) {
    super(name, targetID, observerID, frameID, startET, finalET);
    this.type = type;
  }

  @Override
  public int getType() {
    return type;
  }

  @Override
  public VectorIJK getPosition(@SuppressWarnings("unused") double time,
      @SuppressWarnings("unused") VectorIJK buffer) {
    throw new UnsupportedOperationException();
  }

  @Override
  public StateVector getState(@SuppressWarnings("unused") double time,
      @SuppressWarnings("unused") StateVector buffer) {
    throw new UnsupportedOperationException();
  }

}
