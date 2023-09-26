package picante.spice.kernel.spk;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Preconditions;
import picante.data.list.Retrievable;

class FirstIndexRetrieverList<R> implements FirstIndexRetriever {

  private final Retrievable<R> list;

  private final double startTime;
  private final double stepSize;

  public FirstIndexRetrieverList(Retrievable<R> list, double startTime, double stepSize) {
    this.list = list;
    this.startTime = startTime;
    this.stepSize = stepSize;
  }

  @Override
  public int getFirstIndex(double time, int windowSize) {
    Preconditions.checkArgument(windowSize > 0, "The window size must be greater than 0");
    int degree = windowSize - 1;

    // Ultimately, we want to find the first and last indices that
    // correspond to StateVectors in the table. Since
    int first;

    // first, find the index immediately below the time in question, in a
    // number line, we can think of the index immediately to the left.
    int left;

    // check whether the window size is even or odd
    if (windowSize % 2 == 0) {

      left = (int) ((time - startTime) / stepSize);

      // if it is even then the first index is
      first = Math.min(Math.max(left - (degree / 2), 0), list.size() - degree - 1);
    }

    // if it is not even, then it must be odd. In this case, the odd guy out
    // is which ever of the two bracketing indices corresponding et value is
    // closest to the et value in question. Ties go to the right.
    else {
      int near = (int) Math.round((time - startTime) / stepSize);

      first = Math.min(Math.max(near - (degree / 2), 0), list.size() - degree - 1);

    }

    return first;
  }

  @VisibleForTesting
  double getStartTime() {
    return startTime;
  }

  @VisibleForTesting
  double getStepSize() {
    return stepSize;
  }

}
