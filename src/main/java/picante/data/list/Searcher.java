package picante.data.list;

/**
 * Simple class that provides implementations of the two standard search algorithms supported by
 * this package using binary search over the {@link Gauged#getGauge(int)} method.
 * <p>
 * This class is package private, as there is no need for external users to access it. The abstract
 * classes provided in the package give indirect access to it, as do the static methods on various
 * utility classes.
 * </p>
 */
class Searcher {

  private final GaugedRetrievable<?> list;

  Searcher(GaugedRetrievable<?> list) {
    this.list = list;
  }

  public int indexLastLessThanOrEqualTo(double time) {
    int items = list.size();
    int begin = 0;
    int end = items - 1;
    int middle;
    int j;

    /*
     * Next handle the case where none of the elements in the array are less than the search value.
     */
    double queryValue = list.getGauge(begin);

    if (time < queryValue) {
      return -1;
    }

    /*
     * And the case where all of the elements in the array are less than or equal to the search
     * value.
     */
    queryValue = list.getGauge(end);

    if (time >= queryValue) {
      return end;
    }

    /*
     * The boundary cases have been handled, initiate the search over the segment contents.
     */
    while (items > 2) {

      j = items / 2;
      middle = begin + j;

      queryValue = list.getGauge(middle);

      if (queryValue <= time) {
        begin = middle;
      } else {
        end = middle;
      }

      items = 1 + (end - begin);
    }

    return begin;

  }

  public int indexLastLessThan(double time) {

    int items = list.size();
    int begin = 0;
    int end = items - 1;
    int middle;
    int j;

    /*
     * Next handle the case where none of the elements in the array are less than the search value.
     */
    double queryValue = list.getGauge(begin);

    if (time <= queryValue) {
      return -1;
    }

    /*
     * And the case where all of the elements in the array are less than the search value.
     */
    queryValue = list.getGauge(end);

    if (queryValue < time) {
      return end;
    }

    /*
     * The boundary cases have been handled, initiate the search over the segment contents.
     */
    while (items > 2) {

      j = items / 2;
      middle = begin + j;

      queryValue = list.getGauge(middle);

      if (queryValue < time) {
        begin = middle;
      } else {
        end = middle;
      }

      items = 1 + (end - begin);

    }

    return begin;

  }

}
