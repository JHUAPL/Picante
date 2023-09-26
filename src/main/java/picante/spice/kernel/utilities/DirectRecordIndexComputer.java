package picante.spice.kernel.utilities;

import picante.data.list.Retrievable;
import picante.spice.kernel.pck.PCKType2;
import picante.spice.kernel.spk.SPKType2;
import picante.spice.kernel.spk.SPKType3;

/**
 * Simple class that consolidates the direct index computation arithmetic for record list segments
 * that have a fixed time interval between records.
 * 
 * @see SPKType2
 * @see SPKType3
 * @see PCKType2
 * 
 */
public class DirectRecordIndexComputer {

  /**
   * The initial, starting epoch of coverage for the record list.
   */
  private final double initialEpoch;

  /**
   * The length of each time interval between records.
   */
  private final double intervalLength;

  /**
   * The record list to which this computer is tied.
   */
  private final Retrievable<?> list;

  /**
   * Creates a record index computer for a record list.
   * 
   * @param initialEpoch the initial epoch of the first record in the table.F
   * @param intervalLength the length of each time delta between the records
   * @param list the list of records
   */
  public DirectRecordIndexComputer(double initialEpoch, double intervalLength,
      Retrievable<?> list) {
    this.initialEpoch = initialEpoch;
    this.intervalLength = intervalLength;
    this.list = list;
  }

  public double getInitialEpoch() {
    return initialEpoch;
  }

  public double getIntervalLength() {
    return intervalLength;
  }

  public int getRecordListLength() {
    return list.size();
  }

  public int computeRecordIndexForTime(double time) {
    return Math.min(list.size() - 1, (int) ((time - initialEpoch) / intervalLength));
  }

}
