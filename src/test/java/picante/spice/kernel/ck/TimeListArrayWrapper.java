package picante.spice.kernel.ck;

import picante.data.list.AbstractGaugedRetrievable;


/**
 * Simple implementation of the record table that can be used to provide an the list of interval
 * start times to various C-kernel segment test suites.
 */
class TimeListArrayWrapper extends AbstractGaugedRetrievable<Object> {

  private final double[] times;

  public TimeListArrayWrapper(double[] times) {
    this.times = times;
  }

  @Override
  public double getGauge(int index) {
    return times[index];
  }

  @Override
  public int size() {
    return times.length;
  }

  @Override
  public Object get(@SuppressWarnings("unused") int index,
      @SuppressWarnings("unused") Object buffer) {
    throw new UnsupportedOperationException();
  }

}
