package picante.spice.kernel.spk;

import com.google.common.base.Preconditions;
import picante.data.list.GaugedRetrievableLLT;

class FirstIndexRetrieverTable<R> implements FirstIndexRetriever {

  private final GaugedRetrievableLLT<R> table;

  public FirstIndexRetrieverTable(GaugedRetrievableLLT<R> table) {
    this.table = table;
  }

  /**
   * 
   * Given a record table and a time, we want a list of indices which allow us to retrieve. There is
   * an apparent asymmetry which arises from the assumption that the time entered into this function
   * is never above or below the minimum value in the record table, no explicit check is done in
   * this method for this condition. The way spice is implemented though, if a value below the start
   * epoch gets in here it shifts it to the start epoch, but the analogous shift for the end of the
   * segment is not performed. There are also no checks to make sure that the degree is the same
   * size as the segment, in which case this will break. There is also no check to ensure that the
   * window size extends off the edge of both borders, in which case you have another problem.
   * 
   * @param time
   * @param windowSize
   * @return firstIndex
   */
  @Override
  public int getFirstIndex(double time, int windowSize) {
    Preconditions.checkArgument(windowSize > 0, "The window size must be greater than 0");
    int degree = windowSize - 1;

    // Ultimately, we want to find the first and last indices that
    // correspond to StateVectors in the table. Since
    int first;

    // first, find the index immediately below the time in question, in a
    // number line, we can think of the index immediately to the left.
    int left = table.indexLastLessThan(time);

    // perform check to make sure that the time is not equal to or before
    // the first index. If it is, set the left index equal to 0. Note, this
    // is being done since this is what is done in spice (spkr09.f). It may
    // make more sense to use llte instead of llt and then through a runtime
    // exception if the time is off the end. We are counting on the
    // AbstractRecordTableWithException to thrown an exception if the time
    // is invalid.
    if (left == -1) {
      return left = 0;
    }

    // the left index along with the right index forms a pair of indices
    // which together bracket the epoch in question.
    int right = left + 1;

    // check whether the window size is even or odd
    if (windowSize % 2 == 0) {

      // if it is even then the first index is
      first = Math.min(Math.max(left - (degree / 2), 0), table.size() - degree - 1);
    }

    // if it is not even, then it must be odd. In this case, the odd guy out
    // is which ever of the two bracketing indices corresponding et value is
    // closest to the et value in question. Ties go to the right.
    else {
      int near;
      if (Math.abs(time - table.getGauge(left)) < Math.abs(time - table.getGauge(right))) {
        near = left;
      } else {
        near = right;
      }

      first = Math.min(Math.max(near - (degree / 2), 0), table.size() - degree - 1);
    }

    return first;
  }

}
