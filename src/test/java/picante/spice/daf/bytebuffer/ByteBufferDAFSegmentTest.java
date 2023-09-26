package picante.spice.daf.bytebuffer;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import java.nio.ByteBuffer;
import java.nio.DoubleBuffer;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

public class ByteBufferDAFSegmentTest {

  private double[] testData;
  ByteBufferDAFSegment segment;

  @Before
  public void setUp() throws Exception {

    testData = new double[] {0, 1, 2, 3, 4, -4, -3, -2, -1, 0};

    /*
     * Create a byte buffer with some double values.
     */
    ByteBuffer source = ByteBuffer.allocate(8 * 10);
    DoubleBuffer buffer = source.asDoubleBuffer();
    buffer.put(testData);

    segment = new ByteBufferDAFSegment("TestSegment", new double[] {-10.0, 20.0},
        new int[] {20, 40, 60, 80}, source);

  }

  @Ignore
  @Test
  public void testByteBufferDAFSegment() {
    fail("Not yet implemented.");
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetLowException() {
    double[] values = new double[10];
    segment.get(-1, values, 0, 1);
  }

  /*
   * TODO: Replace Exception with actual exception
   */
  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetHighException() {
    double[] values = new double[12];
    segment.get(0, values, 0, 12);
  }

  @Test
  public void testGet() {
    double[] values = new double[10];
    segment.get(0, values, 0, 10);

    for (int i = 0; i < 10; i++) {
      assertEquals(testData[i], values[i], 0.0);
    }

    for (int i = 0; i < 10; i++) {
      values[i] = -1000;
    }

    segment.get(5, values, 2, 3);

    assertEquals(-1000, values[0], 0.0);
    assertEquals(-1000, values[1], 0.0);
    assertEquals(-4, values[2], 0.0);
    assertEquals(-3, values[3], 0.0);
    assertEquals(-2, values[4], 0.0);
    assertEquals(-1000, values[5], 0.0);
    assertEquals(-1000, values[6], 0.0);
    assertEquals(-1000, values[7], 0.0);
    assertEquals(-1000, values[8], 0.0);
    assertEquals(-1000, values[9], 0.0);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetDoubleComponentLowException() {
    segment.getDoubleComponent(-1);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetDoubleComponentHighException() {
    segment.getDoubleComponent(2);
  }

  @Test
  public void testGetDoubleComponent() {
    assertEquals(-10.0, segment.getDoubleComponent(0), 0.0);
    assertEquals(20.0, segment.getDoubleComponent(1), 0.0);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetIntComponentLowException() {
    segment.getIntComponent(-1);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetIntComponentHighException() {
    segment.getIntComponent(4);
  }

  @Test
  public void testGetIntComponent() {
    assertEquals(20, segment.getIntComponent(0));
    assertEquals(40, segment.getIntComponent(1));
    assertEquals(60, segment.getIntComponent(2));
    assertEquals(80, segment.getIntComponent(3));
  }

  @Test
  public void testGetND() {
    assertEquals(2, segment.getND());
  }

  @Test
  public void testGetNI() {
    assertEquals(4, segment.getNI());
  }

  @Test
  public void testGetName() {
    assertEquals("TestSegment", segment.getName());
  }

  @Test
  public void testGetLength() {
    assertEquals(10, segment.getLength());
  }

}
