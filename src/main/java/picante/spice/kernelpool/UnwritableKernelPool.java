package picante.spice.kernelpool;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import picante.math.intervals.Interval;

/*
 * An important implementation detail to recall for maintenance is that this class contains one
 * underlying map for each of the supported value types: numeric and string.
 * 
 * Each method on this class can safely assume that the two maps are properly synchronized, because
 * the onus is on the developer when modifying these maps to insure that keys are never present in
 * multiple data type maps.
 * 
 * For example, if adding a new keyword of the numeric type, we must check the string map to verify
 * that the keyword is NOT present there. If it is we either must remove it or throw some sort of
 * exception disallowing the add.
 * 
 * Returning direct references to the lists stored in the kernel pool has maps would normally be a
 * very, very bad idea. It exposes internal details of the pool's data structures to the caller.
 * However, a combination of the Collections API (unmodifiableList() to be exact) and judicious
 * creation of new lists as data is added into the structure, we can safely return references to the
 * lists.
 */

/**
 * Unwritable kernel pool implementations.
 * <p>
 * This class implements parent-class of the weak immutability pattern. It allows users to pass
 * around references to kernel pool content without fear of their modification.
 */
public class UnwritableKernelPool {

  private static Interval INTEGER_RANGE = new Interval(Integer.MIN_VALUE, Integer.MAX_VALUE);

  /**
   * The string Map that connects keywords to string data. The onus is on the methods in this
   * routine to keep the user from assigning keywords into both this and the double Map.
   */
  final Map<String, KernelPoolValues<String>> strings =
      new HashMap<String, KernelPoolValues<String>>();

  /**
   * The double Map that connects keywords to double data. The onus is on the methods in this
   * routine to keep the user from assigning keywords into both this and the double Map.
   */
  final Map<String, KernelPoolValues<Double>> doubles =
      new HashMap<String, KernelPoolValues<Double>>();

  UnwritableKernelPool() {}

  /**
   * Copy constructor which copies the contents of an existing pool.
   * 
   * @param pool the pool to copy
   */
  public UnwritableKernelPool(UnwritableKernelPool pool) {

    /*
     * Since this instance has only empty maps, use the mergeMaps method to copy the contents of
     * pool.
     */
    mergeMaps(pool.doubles, this.doubles);
    mergeMaps(pool.strings, this.strings);

  }

  /**
   * Retrieve the list of string values associated with a supplied keyword.
   * 
   * @param key the keyword to retrieve
   * 
   * @return an unmodifiable view of the list of strings associated with the supplied keyword, or
   *         null if keyword is associated with the numeric type or absent from the pool
   */
  public List<String> getStrings(String key) {

    /*
     * Attempt to fetch the values associated with key.
     */
    KernelPoolValues<String> kPoolValues = strings.get(key);

    /*
     * Check to see if values is assigned to null, if so, then the key is missing... return null.
     */
    if (kPoolValues == null) {
      return null;
    }

    /*
     * Return a reference to the internally held unmodifiable list.
     */
    return kPoolValues.values;

  }

  /**
   * Retrieve the list of numeric values associated with a supplied keyword.
   * 
   * @param key the keyword to retrieve
   * 
   * @return an unmodifiable view of the list of numeric data associated with the supplied keyword,
   *         or null if keyword is associated with the string type or absent from the pool
   */
  public List<Double> getDoubles(String key) {

    /*
     * Attempt to fetch the values associated with key.
     */
    KernelPoolValues<Double> kPoolValues = doubles.get(key);

    /*
     * Check to see if values is assigned to null, if so, then the key is missing... return null.
     */
    if (kPoolValues == null) {
      return null;
    }

    /*
     * Return a reference to the internally held unmodifiable list.
     */
    return kPoolValues.values;
  }

  /**
   * Retrieve a list of numeric values, converted to integer values, associated with a supplied
   * keyword.
   * <p>
   * Note: Non-integral values are rounded to the nearest integer in the conversion process.
   * </p>
   * 
   * @param key the keyword to retrieve
   * 
   * @return an unmodifiable view of the list of numeric data, as integers, associated with the
   *         supplied keyword, or null if keyword is associated with the string type or is absent
   *         from the pool.
   * 
   * @throws KernelPoolFormatException if any of the values stored in the retrieve list lie outside
   *         the range supported by the integers.
   */
  public List<Integer> getIntegers(String key) {

    KernelPoolValues<Double> kPoolValues = doubles.get(key);

    if (kPoolValues == null) {
      return null;
    }

    ArrayList<Integer> result = new ArrayList<Integer>(kPoolValues.values.size());

    for (Double d : kPoolValues.values) {
      if (INTEGER_RANGE.closedContains(d)) {
        result.add(Integer.valueOf((int) Math.round(d)));
      } else {
        throw new KernelPoolFormatException(key, d);
      }

    }

    return Collections.unmodifiableList(result);

  }

  /**
   * Retrieve a copy of the set of all keywords currently present in pool.
   * 
   * @return an unmodifiable view of the set of all keywords currently loaded into a pool. If the
   *         set is empty, an empty set of strings is returned.
   */
  public Set<String> getKeywords() {
    Set<String> theKeys = new HashSet<String>(strings.size() + doubles.size());

    theKeys.addAll(strings.keySet());
    theKeys.addAll(doubles.keySet());
    return Collections.unmodifiableSet(theKeys);
  }

  /**
   * Is a particular keyword present in pool and associated with string data?
   * 
   * @param key the keyword
   * 
   * @return true, if the keyword is present and associated with string data; false otherwise.
   */
  public boolean isStringValued(String key) {
    return strings.containsKey(key);
  }

  /**
   * Is a particular keyword present in pool and associated with numeric data?
   * 
   * @param key the keyword
   * 
   * @return true, if the keyword is present and associated with numeric data; false otherwise.
   */
  public boolean isDoubleValued(String key) {
    return doubles.containsKey(key);
  }

  /**
   * Is a particular keyword present in pool and associated with integral data?
   * 
   * @param key the keyword
   * 
   * @return true, if the keyword is present and associated with integral, numeric data; false
   *         otherwise. If any of the numeric values associated with key are outside the range
   *         between {@link Integer#MIN_VALUE} and {@link Integer#MAX_VALUE} then this method will
   *         return false.
   */
  public boolean isIntegerValued(String key) {

    // TODO: Consider migrating the integer range check off to the constructor of KernelPoolValues
    // itself for numeric types. This might be more efficient in the long run, since it won't
    // require repeatedly iterating over the values to find out if it is OK for casting to an
    // integer.

    /*
     * First confirm that the backing type for key is double.
     */
    if (!isDoubleValued(key)) {
      return false;
    }

    /*
     * Check to see if the values mapped to by key are in fact assignable to integers. Note: this
     * only verifies that the value is within range of casting to an integer, not that there is no
     * "fractional" part that will be discarded in the cast.
     */
    for (double value : doubles.get(key).values) {
      if (!INTEGER_RANGE.closedContains(value)) {
        return false;
      }
    }

    return true;
  }

  /**
   * Is a particular keyword present in the pool?
   * 
   * @param key the keyword
   * 
   * @return true, if the keyword is present regardless of the data type with which it is
   *         associated. false; otherwise.
   */
  public boolean hasKeyword(String key) {
    return isStringValued(key) || isDoubleValued(key);
  }

  /**
   * Copy and merge the contents of one keyword values map into another.
   * 
   * @param <T> The data type of the values associated with the keyword.
   * 
   * @param giver The map containing the keyword value of type T associations that are to be merged
   * @param receiver The map connecting keywords to data of type T that is to receive the contents
   *        of giver
   */
  static <T> void mergeMaps(Map<String, KernelPoolValues<T>> giver,
      Map<String, KernelPoolValues<T>> receiver) {

    Set<String> keys = giver.keySet();

    /*
     * Loop over each key in the giver map. Take care to preserve the state of appendState as values
     * are inserted into receiver.
     */
    for (String key : keys) {

      KernelPoolValues<T> giversValues = giver.get(key);

      /*
       * If the receiver does not already contain the key, or the giver's value for this specific
       * key indicates not to append, then simply place the new value into the pool.
       */
      if (!receiver.containsKey(key) || !giversValues.appendState) {
        receiver.put(key, new KernelPoolValues<T>(giversValues));
      } else {

        /*
         * Now just append the contents of giversValues to the existing values in receiver. Leave
         * the appendState alone, as an appending action from giver has no impact on the append
         * state in the receiver.
         */
        receiver.put(key, new KernelPoolValues<T>(receiver.get(key), giversValues.values));
      }
    }
  }
}
