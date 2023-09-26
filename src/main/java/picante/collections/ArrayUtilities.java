package picante.collections;

import java.util.Arrays;

/**
 * This class consists exclusively of static methods that operate on or return arrays. It is much
 * like the Arrays class in the java.util package, only it provides other useful methods.
 * 
 * TODO: Flush out the class with the remaining primitive array search methods
 * 
 */
public class ArrayUtilities {

  /**
   * Determine the index of the last element less than or equal to the specified key. The list must
   * be sorted into ascending order {@link Arrays#sort(double[])}. If it is not sorted, the results
   * are undefined. If the list contains multiple elements equal to the specified one, the last one
   * is guaranteed to be extracted.
   * <p>
   * The method utilizes {@link Arrays#binarySearch(double[], int, int, double)} so its performance
   * characteristics are closely related.
   * </p>
   * 
   * @param list the list containing sorted doubles over which to search
   * @param key the key to locate the last element less than or equal to in the list
   * 
   * @return the range returned is [-1,list.length-1], where -1 indicates that key precedes all
   *         elements in the list.
   */
  public static int lastLessThanOrEqualTo(double[] list, double key) {
    return lastLessThanOrEqualTo(list, 0, list.length, key);
  }

  /**
   * Determine the index of the last element less than or equal to the specified key. The list must
   * be sorted into ascending order {@link Arrays#sort(double[])}. If it is not sorted, the results
   * are undefined. If the list contains multiple elements equal to the specified key value, the
   * last one is guaranteed to be extracted.
   * <p>
   * The method utilizes {@link Arrays#binarySearch(double[], int, int, double)} so its performance
   * characteristics are closely related.
   * </p>
   * 
   * @param list the list over which a subset of values is to be searched
   * @param startIndex the starting position to perform the search
   * @param length the length over which to perform the search
   * @param key the key to locate
   * 
   * @return the index of the element last less than or equal to the supplied key in the sublist.
   *         The range returned is [pos-1,pos+length-1] where pos-1 indicates that key precedes all
   *         elements in the specified range within list.
   */
  public static int lastLessThanOrEqualTo(double[] list, int startIndex, int length, double key) {
    int result = Arrays.binarySearch(list, startIndex, startIndex + length, key);

    /*
     * If the result was located in the list directly, locate the last element equal to it and
     * return that index.
     */
    if (result >= 0) {
      int lastEqualIndex = locateLastElementEqualTo(result, list, startIndex + length);
      return lastEqualIndex;
    }

    return convertIndexForLessThan(result);
  }

  /**
   * Determine the index of the last element strictly less than the specified key. The list must be
   * sorted into ascending order {@link Arrays#sort(double[])}. If it is not sorted, the results are
   * undefined. If the list contains multiple equal elements to the specified one, the last one is
   * guaranteed to be extracted.
   * <p>
   * This method utilizes {@link Arrays#binarySearch(double[], int, int, double)} so its performance
   * characteristics are closely related.
   * </p>
   * 
   * @param list the list containing sorted doubles over which to search
   * @param key the key to locate the last element strictly less than in the list
   * 
   * @return the range returned is [-1, list.length-1], where -1 indicates that key precedes all
   *         elements in the list.
   */
  public static int lastLessThan(double[] list, double key) {
    return lastLessThan(list, 0, list.length, key);
  }

  /**
   * Determine the index of the last element strictly less than the specified key. The list must be
   * sorted into ascending order {@link Arrays#sort(double[])}. If it is not sorted, the results are
   * undefined. If the list contains multiple elements equal to the specified key value, the last
   * one is guaranteed to be extracted.
   * <p>
   * This method utilizes {@link Arrays#binarySearch(double[], int, int, double)} so its performance
   * characteristics are closely related.
   * </p>
   * 
   * @param list the list over which a subset of values is to be searched
   * @param startIndex the starting position from which to perform the search
   * @param length the length over which to perform the search
   * @param key the key to locate
   * 
   * @return the index of the last element strictly less than the supplied key in the sublist. The
   *         range returned is [pos-1,pos+length-1] where pos-1 indicates that key preceds all
   *         elements in the specified range within the list.
   */
  public static int lastLessThan(double[] list, int startIndex, int length, double key) {
    int result = Arrays.binarySearch(list, startIndex, startIndex + length, key);

    /*
     * If the result was located in the list directly, locate the first element equal to it and
     * return that index less 1.
     */
    if (result >= 0) {
      int lastEqualIndex = locateFirstElementEqualTo(result, list, startIndex);
      return lastEqualIndex - 1;
    }

    return convertIndexForLessThan(result);
  }

  /**
   * Determine the index of the first element greater than or equal to the specified key. The list
   * must be sorted into ascending order {@link Arrays#sort(double[])}. If it is not sorted, the
   * results are undefined. If the list contains multiple elements equal to the specified key, the
   * first one is guaranteed to be located.
   * <p>
   * This method utilizes {@link Arrays#binarySearch(double[], int, int, double)} so its performance
   * characteristics are closely related.
   * </p>
   * 
   * @param list the sorted list containing the values to be searched
   * @param key the key to locate
   * 
   * @return the range returned is [0, list.length], where list.length indicates that key follows
   *         all the elements in the list.
   */
  public static int firstGreaterThanOrEqualTo(double[] list, double key) {

    /*
     * This is just as simple as invoking the corresponding lastLessThan method and adding one.
     */
    return lastLessThan(list, key) + 1;
  }

  /**
   * Determine the index of the first element greater than or equal to the specified key in the
   * range within the supplied list. The range must be sorted into ascending order
   * {@link Arrays#sort(double[], int, int)}. If it is not sorted, the results are undefined. If the
   * list contains multiple elements equal to the specified key, the first one is guaranteed to be
   * located.
   * <p>
   * This method utilizes {@link Arrays#binarySearch(double[], int, int, double)} so its performance
   * characteristics are closely related.
   * </p>
   * 
   * @param list the list from which the range is to be searched
   * @param startIndex the start index of the search range
   * @param length the length of the search range
   * @param key the key to locate
   * 
   * @return the range returned is [pos, pos+length], where pos+length indicates that key follows
   *         all elements in the range.
   */
  public static int firstGreaterThanOrEqualTo(double[] list, int startIndex, int length,
      double key) {

    /*
     * This is just as simple as invoking the corresponding lastLessThan method and adding one.
     */
    return lastLessThan(list, startIndex, length, key) + 1;
  }

  /**
   * Determine the index of the first element strictly greater than the specified key. The list must
   * be sorted into ascending order: {@link Arrays#sort(double[])}. If it is not sorted, the results
   * are undefined. If the list contains multiple elements equal to the specified object, the first
   * one is guaranteed to be located.
   * <p>
   * This method utilizes {@link Arrays#binarySearch(double[], int, int, double)} so its performance
   * characteristics are closely related.
   * </p>
   * 
   * @param list the list over which to perform the search
   * @param key the value to locate
   * 
   * @return the range returned is [0, list.length], where list.length indicates that key follows
   *         all elements in the list.
   */
  public static int firstGreaterThan(double[] list, double key) {
    /*
     * This is just as simple as invoking the corresponding lastLessThanOrEqualTo method and adding
     * one.
     */
    return lastLessThanOrEqualTo(list, key) + 1;
  }

  /**
   * Determine the index of the first element strictly greater than the specified key within the
   * specified range of list. The range must be sorted into ascending order:
   * {@link Arrays#sort(double[], int, int)}. If it is not sorted, the results are undefined. If the
   * list contains multiple elements equal to the specified key, the first one in the range is
   * guaranteed to be located.
   * <p>
   * This method utilizes {@link Arrays#binarySearch(double[], int, int, double)} so its performance
   * characteristics are closely related.
   * </p>
   * 
   * @param list the list from which the range is to be searched
   * @param startIndex the start index of the search range
   * @param length the length of the search range
   * @param key the key to locate
   * 
   * @return the range returned is [pos, pos+length], where pos+length indicates that key follows
   *         all elements in the range
   */
  public static int firstGreaterThan(double[] list, int startIndex, int length, double key) {

    /*
     * This is just as simple as invoking the corresponding lastLessThanOrEqualTo method and adding
     * one.
     */
    return lastLessThanOrEqualTo(list, startIndex, length, key) + 1;
  }

  /**
   * Locate the last element in the list equal to the element by the supplied index.
   * 
   * @param index the index at which to start the search. It should be an index to an existing
   *        element of the list that is of interest.
   * @param list the list over which the search is to be performed
   * @param maxIndex the maximum index which is to be considered
   * 
   * @return the index of the last element equal to list[index] in the sorted list.
   */
  private static int locateLastElementEqualTo(int index, double[] list, int maxIndex) {

    /*
     * If we are already at the end of the list, just return.
     */
    if (index == list.length - 1) {
      return index;
    }

    int result = index;

    /*
     * Loop over elements in the list incrementing result as long as they are equal to list[index].
     */
    while ((result < maxIndex) && (list[++result] == list[index])) {
    }

    /*
     * Result will have been incremented one past the desired index, decrement it.
     */
    return --result;

  }

  /**
   * Locate the first element in the list equal to the element at position index.
   * 
   * @param index the index at which to start the search for the first value.
   * @param list the list over which the search is to be performed.
   * @param firstIndex the minimum index over which the search is to happen
   * 
   * @return the index of the first element equal to list[index] in the sorted list.
   */
  private static int locateFirstElementEqualTo(int index, double[] list, int firstIndex) {

    /*
     * If we are already at the start of the range, just return.
     */
    if (index == firstIndex) {
      return index;
    }

    int result = index;

    /*
     * Loop over the previous elements in the list as long as they continue to compare with equality
     * to the element at list[index].
     */
    while ((result > firstIndex) && (list[--result] == list[index])) {
    }

    /*
     * Result will have been decremented one past the desired index, increment it.
     */
    return ++result;
  }

  /**
   * In the event that the binarySearch algorithm from the Collections class is unable to turn up a
   * matched element, locate the index of the element that is strictly less than the one sought.
   * 
   * @param result a negative result from either {@link Arrays#binarySearch(byte[], byte)}
   * 
   * @param listSize the size of the list over which the binary search was performed at the time of
   *        the search
   * 
   * @return the index of the element strictly less than the one sought after in the binary search.
   */
  private static int convertIndexForLessThan(int result) {

    int insertionPoint = -result - 1;

    /*
     * Since the insertion point is defined as the index at which everyone will be shifted to the
     * right:
     * 
     * value: 10 20 30 40 index: 0 1 2 3
     * 
     * If insertion point is 1, then that means the result of inserting this element into the list
     * will result:
     * 
     * value: 10 15 20 30 40 index: 0 1 2 3 4
     * 
     * would be the result, and 15 would have reduced to an insertion point of 1. So, long story
     * short, if we reach here then just subtract one and we'll get the answer we desire.
     */
    return insertionPoint - 1;

  }

  /**
   * Checks if an array is ragged, e.g:
   * 
   * <pre>
   * This is a ragged array
   *  { {1, 2, 3} }
   *  { {3, 4}    }
   * 
   * This is not a ragged array
   *  { {1, 2, 3} }
   *  { {3, 4, 5} }
   * </pre>
   * 
   * @param r input array
   * @param <R> the array type parameter
   * 
   * @return true if the input is a ragged array, false if it is 'rectangular'
   */
  public static <R> boolean isRagged(R[][] r) {

    if (r.length > 0) {

      int width0 = r[0].length;

      for (int i = 0; i < r.length; i++) {

        if (r[i].length != width0) {
          return true;
        }

      }

    }
    return false;
  }

  /**
   * Checks if an array is ragged.
   * 
   * @param r input array
   * @param <R> the array type parameter
   * 
   * @return true if the input is a ragged array, false if it is 'rectangular'
   */
  public static <R> boolean isRagged(R[][][] r) {

    if (r.length > 0 && r[0].length > 0) {

      int width0 = r[0].length;
      int depth0 = r[0][0].length;

      for (int i = 0; i < r.length; i++) {

        if (r[i].length != width0) {
          return true;
        }

        for (int j = 0; j < r[0].length; j++) {
          if (r[i][j].length != depth0) {
            return true;
          }
        }

      }

    }

    return false;
  }

  /**
   * Checks if an array is ragged, e.g:
   * 
   * <pre>
   * This is a ragged array
   *  { {1, 2, 3} }
   *  { {3, 4}    }
   * 
   * This is not a ragged array
   *  { {1, 2, 3} }
   *  { {3, 4, 5} }
   * </pre>
   * 
   * @param r input array
   * @return true if the input is a ragged array, false if it is 'rectangular'
   */
  public static boolean isRagged(double[][] r) {

    if (r.length > 0) {

      int width0 = r[0].length;

      for (int i = 0; i < r.length; i++) {

        if (r[i].length != width0) {
          return true;
        }

      }

    }
    return false;
  }

  /**
   * Checks if an array is ragged.
   * 
   * @param r input array
   * @return true if the input is a ragged array, false if it is 'rectangular'
   */
  public static boolean isRagged(double[][][] r) {

    if (r.length > 0 && r[0].length > 0) {

      int width0 = r[0].length;
      int depth0 = r[0][0].length;

      for (int i = 0; i < r.length; i++) {

        if (r[i].length != width0) {
          return true;
        }

        for (int j = 0; j < r[0].length; j++) {
          if (r[i][j].length != depth0) {
            return true;
          }
        }

      }

    }

    return false;
  }

}
