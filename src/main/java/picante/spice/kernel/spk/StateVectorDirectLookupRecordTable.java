package picante.spice.kernel.spk;

import static com.google.common.base.Preconditions.checkArgument;
import picante.data.list.GaugedRetrievableLLT;
import picante.data.list.Retrievable;
import picante.mechanics.StateVector;

class StateVectorDirectLookupRecordTable implements GaugedRetrievableLLT<StateVector> {

  private final double initialEpoch;
  private final double intervalLength;

  private final Retrievable<StateVector> recordList;

  private final int firstIndex;
  private final int lastIndex;

  public StateVectorDirectLookupRecordTable(double initialEpoch, double intervalLength,
      Retrievable<StateVector> recordList) {
    this.initialEpoch = initialEpoch;
    this.intervalLength = intervalLength;
    this.recordList = recordList;
    this.firstIndex = 0;
    this.lastIndex = recordList.size() - 1;
  }

  /**
   * Given an index, checks that it is a valid index, and then calculates the epoch that would be
   * associated with that index.
   */
  @Override
  public double getGauge(int index) {
    checkArgument(index <= lastIndex, "The requested index " + index
        + " is greater than the last index " + lastIndex + " in the recordList");
    checkArgument(index >= firstIndex, "The requested index " + index
        + " is less than the first index " + firstIndex + " in the recordList");
    return initialEpoch + (index * intervalLength);
  }

  @Override
  public StateVector get(int index, StateVector buffer) {
    return recordList.get(index, buffer);
  }

  @Override
  public int size() {
    return recordList.size();
  }

  /**
   * return the index that last in
   */
  @Override
  public int indexLastLessThan(double time) {
    // the double representation of the integer we want to return
    double d = (time - initialEpoch) / intervalLength;

    // checks that d is not an integer value, eg floor(6) == 6 should return
    // 5 not 6, since 5 is the integer that is last less than
    if (Math.floor(d) != d) {
      return Math.max(Math.min((int) (d), lastIndex), firstIndex - 1);
      // if it is return
    } else {
      return Math.max(Math.min((int) (d - 1), lastIndex), firstIndex - 1);
    }
  }

}
