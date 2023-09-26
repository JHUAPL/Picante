package picante.spice.daf.content.recordtables;

import picante.spice.daf.DAFSegment;

/**
 * Package private implementation of the {@link DAFSegment} interface used for testing.
 */
class ArrayDAFSegment implements DAFSegment {

  private final double[] data;

  /**
   * Creates an ArrayDAFSegment that captures data incrementally from lower to upper.
   * 
   * @param lower the value to place at segment index 0
   * @param upper the value to place at segment index upper-lower
   * 
   * @return a newly created segment wrapped around the sequential sequence.
   */
  public static ArrayDAFSegment createSequentialSegment(int lower, int upper) {
    double[] data = new double[upper - lower + 1];
    for (int i = lower; i <= upper; i++) {
      data[i - lower] = i;
    }
    return new ArrayDAFSegment(data);
  }

  /**
   * Creates a new ArrayDAFSegment for testing, maintaining a reference to the supplied data array.
   * 
   * @param data the data to wrap in a DAFSegment
   */
  public ArrayDAFSegment(double[] data) {
    this.data = data;
  }

  @Override
  public void get(int index, double[] buffer, int offset, int length) {
    System.arraycopy(data, index, buffer, offset, length);
  }

  @Override
  public double getDoubleComponent(@SuppressWarnings("unused") int index) {
    throw new UnsupportedOperationException();
  }

  @Override
  public int getIntComponent(@SuppressWarnings("unused") int index) {
    throw new UnsupportedOperationException();
  }

  @Override
  public int getLength() {
    return data.length;
  }

  @Override
  public int getND() {
    throw new UnsupportedOperationException();
  }

  @Override
  public int getNI() {
    throw new UnsupportedOperationException();
  }

  @Override
  public String getName() {
    return "ARRAY_TEST_SEGMENT";
  }

}
