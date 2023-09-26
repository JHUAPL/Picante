package picante.spice.daf.bytebuffer;

import java.nio.BufferUnderflowException;
import java.nio.ByteBuffer;
import java.nio.DoubleBuffer;
import picante.spice.daf.DAFSegment;

/**
 * Package private class implementing the <code>DAFSegment</code> interface. Generally, instances of
 * this class are created when the <code>ByteBufferDAF</code> constructor is executed.
 */
class ByteBufferDAFSegment implements DAFSegment {

  /**
   * A view of the appropriate segment contents as a double buffer.
   */
  private final DoubleBuffer doubleBuffer;

  /**
   * An array containing the double metadata components in the correct order.
   */
  private final double[] dc;

  /**
   * An array containing the integer metadata components in the correct order.
   */
  private final int[] ic;

  /**
   * The segment name, describing the content.
   */
  private final String name;

  /**
   * Constructs a <code>DAFSegment</code> associated with the particular byte buffer.
   * 
   * @param name name of the segment
   * @param dc double precision metadata components. A reference to the supplied array is held
   *        internally by the instance post construction
   * @param ic integer metadata components. A reference to the supplied array is held internally by
   *        the instance past construction
   * @param buffer <code>ByteBuffer</code> containing the data elements of the properly configured
   *        <code>ByteOrder</code>
   */
  ByteBufferDAFSegment(String name, double[] dc, int[] ic, ByteBuffer buffer) {
    super();
    this.name = name;
    this.dc = dc;
    this.ic = ic;
    this.doubleBuffer = buffer.asDoubleBuffer();
  }

  @Override
  public void get(int index, double[] buffer, int offset, int length) {
    try {
      doubleBuffer.position(index);
      doubleBuffer.get(buffer, offset, length);
    } catch (BufferUnderflowException e) {
      throw new IndexOutOfBoundsException("Invalid index and length arguments specified.");
    } catch (IllegalArgumentException e) {
      throw new IndexOutOfBoundsException("Invalid index and length arguments specified.");
    }
  }

  @Override
  public double getDoubleComponent(int index) {
    return dc[index];
  }

  @Override
  public int getIntComponent(int index) {
    return ic[index];
  }

  @Override
  public int getND() {
    return dc.length;
  }

  @Override
  public int getNI() {
    return ic.length;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public int getLength() {
    return doubleBuffer.capacity();

  }

}
