package picante.spice.kernel.ck;

import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.RotationMatrixIJK;

public class AbstractCKSegmentTest {

  private ConcreteSegment segment;

  @Before
  public void setUp() {
    segment = new ConcreteSegment("name", -1000, 2000, 12345, 67890);
  }

  @Test
  public void testGetInstrumentID() {
    assertEquals(-1000, segment.getInstrumentID());
  }

  @Test
  public void testGetReferenceID() {
    assertEquals(2000, segment.getReferenceID());
  }

  @Test
  public void testGetName() {
    assertEquals("name", segment.getName());
  }

  @Test
  public void testGetInitialEncodedSCLK() {
    assertEquals(12345.0, segment.getInitialEncodedSCLK(), 0.0);
  }

  @Test
  public void testGetFinalEncodedSCLK() {
    assertEquals(67890.0, segment.getFinalEncodedSCLK(), 0.0);
  }

}


class ConcreteSegment extends AbstractCKSegment {

  public ConcreteSegment(String name, int instrumentID, int referenceID, double initialEncodedSCLK,
      double finalEncodedSCLK) {
    super(name, instrumentID, referenceID, initialEncodedSCLK, finalEncodedSCLK);
  }

  @Override
  public CKCoverage getCoverage() {
    throw new UnsupportedOperationException();
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

  @Override
  public boolean hasAngularVelocity() {
    throw new UnsupportedOperationException();
  }

}
