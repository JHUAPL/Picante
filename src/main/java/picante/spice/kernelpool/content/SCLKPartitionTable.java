package picante.spice.kernelpool.content;

import java.util.List;

import com.google.common.collect.ImmutableList;
import picante.data.list.AbstractGaugedRetrievable;
import picante.math.intervals.Interval;
import picante.math.intervals.UnwritableInterval;
import picante.spice.Utilities;
import picante.spice.kernel.tk.sclk.SCLKInstantiationException;

/**
 * Implementation of the required record table interface that captures the partition table from the
 * lists directly retrieved from the kernel pool.
 */
class SCLKPartitionTable extends AbstractGaugedRetrievable<Interval> {

  /**
   * Maintains the encoded SCLK value of the start of the partition entry.
   */
  private final double[] encodedSCLKStarts;

  /**
   * Start and stop tick values for each partition.
   */
  private final List<UnwritableInterval> intervals;

  /**
   * Creates a new SCLK partition table from the supplied partition start and end lists generally
   * supplied by the kernel pool.
   * <p>
   * This constructor rounds the supplied lists of doubles to their closest integral value on input.
   * </p>
   * 
   * @param clockID the NAIF ID for the clock
   * @param starts a list of clock tick valued start times
   * @param ends a list of clock tick valued end times
   * 
   * @throws SCLKInstantiationException if the starts and ends arrays are incompatible.
   */
  SCLKPartitionTable(int clockID, List<Double> starts, List<Double> ends)
      throws SCLKInstantiationException {

    if (starts.size() != ends.size()) {
      throw new SCLKInstantiationException(clockID, "Partition start array has: " + starts.size()
          + " elements, but the end array has: " + ends.size());
    }

    encodedSCLKStarts = new double[starts.size()];

    ImmutableList.Builder<UnwritableInterval> builder = ImmutableList.builder();
    double encodedSCLK = 0;
    for (int i = 0; i < starts.size(); i++) {
      encodedSCLKStarts[i] = encodedSCLK;

      /*
       * Round the partition table values to the nearest integer.
       */
      double start = Utilities.round(starts.get(i));
      double end = Utilities.round(ends.get(i));

      if (start > end) {
        throw new SCLKInstantiationException(clockID,
            "Partition " + (i + 1) + " is invalid.  Start: " + start + " exceeds End: " + end);
      }

      encodedSCLK += end - start;
      builder.add(new UnwritableInterval(start, end));
    }

    intervals = builder.build();
  }

  @Override
  public double getGauge(int index) {
    return encodedSCLKStarts[index];
  }

  @Override
  public Interval get(int index, Interval buffer) {
    return buffer.setTo(intervals.get(index));
  }

  @Override
  public int size() {
    return encodedSCLKStarts.length;
  }

}
