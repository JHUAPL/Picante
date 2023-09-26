package picante.spice.kernelpool;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

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
 * Basic implementation of the NAIF text kernel pool.
 * 
 * It is unlikely that you intend to be using this particular class, see the subclass KernelPool.
 * This particular class contains the rudimentary implementation of the kernel pool data model, but
 * does not contain any of the listener code. As such the majority of the usage documentation lives
 * in the more featured subclass.
 * 
 * In practice the main reason to utilize this particular class is if you have a large block of
 * keywords that you wish to inject into an existing instance of the KernelPool subclass, but do not
 * want to suffer the performance penalties associated with the repeated firing of listeners when no
 * listeners are registered.
 */
public class BasicKernelPool extends UnwritableKernelPool {

  /**
   * Default constructor assembles an empty pool.
   */
  public BasicKernelPool() {}

  /**
   * Copy constructor which copies the contents of an existing pool.
   * 
   * @param pool the pool to copy
   */
  public BasicKernelPool(UnwritableKernelPool pool) {

    /*
     * Since this instance has only empty maps, use the mergeMaps method to copy the contents of
     * pool.
     */
    mergeMaps(pool.doubles, this.doubles);
    mergeMaps(pool.strings, this.strings);

  }

  /**
   * Add string data values to a keyword, destroying any prior associations of data to the supplied
   * keyword.
   * 
   * @param key the keyword.
   * @param values the string data values.
   * 
   * @throws UnsupportedOperationException if the key is null, values is null, or if values is an
   *         empty list.
   */
  public void addStrings(String key, List<String> values) {

    /*
     * Check that the supplied inputs are reasonable. This could result in an
     * UnsupportedOperationException being thrown.
     */
    checkInputs(key, values);

    /*
     * Replace, in the strings map, the value of key if it exists. Note: because this is an add, any
     * appendState must be set to false.
     */
    strings.put(key, new KernelPoolValues<String>(false, values));

    /*
     * Now check that doubles does not contain the value key. If it does, remove it, as the set of
     * keys in strings and doubles must be unique.
     */
    if (doubles.containsKey(key)) {
      doubles.remove(key);
    }
  }

  /**
   * Append string data values to an existing keyword, create the keyword and add the data
   * otherwise.
   * 
   * @param key the keyword.
   * @param values the string data values.
   * 
   * @throws UnsupportedOperationException if the key is null, values is null, or if values is an
   *         empty list. Also, if the supplied keyword exists in the pool and is associated with the
   *         numeric type.
   */
  public void appendStrings(String key, List<String> values) {

    /*
     * Check to see if the supplied inputs are reasonable. This could result in the throwing of an
     * UnsupportedOperationException.
     */
    checkInputs(key, values);

    /*
     * Now, since this is the append routine, we have one additional special check to make. If the
     * key we are attempting to append to is already in the kernel pool with numeric data, then
     * throw an UnsupportedOperationException. This is necessary because the set of keys from the
     * strings and doubles maps must have null intersection. This check ensures that criteria is met
     * at all times.
     */
    if (doubles.containsKey(key)) {
      throw new UnsupportedOperationException("Unable to append String" + " values into key: " + key
          + " because it exists in the " + "pool and is of numeric type.");
    }

    /*
     * Now, check to see if strings contains key.
     */
    if (strings.containsKey(key)) {
      /*
       * Append new values to the ones already present in the list.
       */
      strings.put(key, new KernelPoolValues<String>(strings.get(key), values));
    } else {
      /*
       * Otherwise just insert the values into the map associated with key. Note, since this key
       * didn't already exist we set the appendState to true.
       */
      strings.put(key, new KernelPoolValues<String>(true, values));
    }
  }

  /**
   * Add numeric data values to a keyword, destroying any prior association of data to the supplied
   * keyword.
   * 
   * @param key the keyword.
   * @param values the numeric data values.
   * 
   * @throws UnsupportedOperationException if the key is null, values is null, or if values is an
   *         empty list.
   */
  public void addDoubles(String key, List<Double> values) {

    /*
     * Check that the input values are valid. This may result in the throwing of an
     * UnsupportedOperationException.
     */
    checkInputs(key, values);

    /*
     * Replace, in the doubles map, the value of key if it exists. Note: since this is an add
     * operation, the appendState must be set to false.
     */
    doubles.put(key, new KernelPoolValues<Double>(false, values));

    /*
     * Now check that strings does not contain the value key. If it does, remove it, as the set of
     * keys in strings and doubles must be unique.
     */
    if (strings.containsKey(key)) {
      strings.remove(key);
    }
  }

  /**
   * Append numeric data values to an existing keyword, create the keyword and add the data
   * otherwise.
   * 
   * @param key the keyword.
   * @param values the numeric data values.
   * 
   * @throws UnsupportedOperationException if the key is null, values is null, or if values is an
   *         empty list. Also, if the supplied keyword exists in the pool and is associated with the
   *         string type.
   */
  public void appendDoubles(String key, List<Double> values) {

    /*
     * Check that the supplied inputs are reasonable. This may result in the throwing of an
     * UnsupportedOperationException.
     */
    checkInputs(key, values);

    /*
     * Now, since this is the append routine, we have one additional special check to make. If the
     * key we are attempting to append to is already in the kernel pool with string data, then throw
     * an UnsupportedOperationException. This is necessary because the set of keys from the strings
     * and doubles maps must have null intersection. This check ensures that criteria is met at all
     * times.
     */
    if (strings.containsKey(key)) {
      throw new UnsupportedOperationException("Unable to append Double" + " values into key: " + key
          + " because it exists in the " + "pool and is of string type.");
    }

    /*
     * Now, check to see if doubles contains key.
     */
    if (doubles.containsKey(key)) {

      /*
       * Append new values to the ones already present in the list.
       */
      doubles.put(key, new KernelPoolValues<Double>(doubles.get(key), values));

    } else {

      /*
       * Otherwise just insert the values into the map associated with key. Since this key did not
       * already exist, set the appendState to true.
       */
      doubles.put(key, new KernelPoolValues<Double>(true, values));
    }
  }

  public void addIntegers(String key, List<Integer> values) {
    checkInputs(key, values);
    addDoubles(key, convertToDoubles(values));
  }

  public void appendIntegers(String key, List<Integer> values) {
    checkInputs(key, values);
    appendDoubles(key, convertToDoubles(values));
  }

  private List<Double> convertToDoubles(List<Integer> list) {
    ArrayList<Double> returnValue = new ArrayList<Double>(list.size());
    for (Integer i : list) {
      returnValue.add(Double.valueOf(i));
    }
    return returnValue;
  }

  /**
   * Load the contents of the supplied pool into this pool. This method respects the mechanism by
   * which keywords defined in the input pool argument were assigned and applies them appropriately.
   * For example, if you have two existing pools, receivingPool and givingPool, populated with data
   * in the following fashion:
   * 
   * <pre>
   * &lt;code&gt;
   * List&lt;String&gt; stringList = new ArrayList&lt;String&gt;();
   * stringList.add(&quot;VALUE 1&quot;);
   * stringList.add(&quot;VALUE 2&quot;);
   * receivingPool.addString(&quot;K&quot;, stringList);
   * givingPool.appendString(&quot;K&quot;, otherStringList);
   * &lt;/code&gt;
   * </pre>
   * 
   * then invoking the following code:
   * 
   * <pre>
   * &lt;code&gt;
   * receivingPool.load(givingPool);
   * System.out.println(receivingPool.getStrings(&quot;K&quot;));
   * &lt;/code&gt;
   * </pre>
   * 
   * results in the following output:
   * 
   * <pre>
   * &lt;code&gt;
   *     [ VALUE 1, VALUE 2, VALUE 1, VALUE 2 ]
   * &lt;/code&gt;
   * </pre>
   * 
   * The reason for this is givingPool knows that the keyword K was assigned with an append
   * operation, so the load operation in receivingPool respects that and appends the data content to
   * it's already existing K.
   * 
   * @param pool the pool whose contents are to be loaded into this instance
   * 
   * @throws UnsupportedOperationException if the contents of the input pool require an append of
   *         one data type to an existing keyword in this instance that has another data type.
   */
  public void load(UnwritableKernelPool pool) {

    /*
     * Cross check the contents of pool requesting appends for conflicts with elements existing in
     * this instance.
     */
    crossCheck(pool.strings, this.doubles);
    crossCheck(pool.doubles, this.strings);

    /*
     * Just merge the contents of pool's maps into this pools maps.
     */
    mergeMaps(pool.strings, strings);
    mergeMaps(pool.doubles, doubles);
  }

  /**
   * Removes a list of keywords and their associated data values from a pool. Only the keywords in
   * the list that are actually in the pool are removed. If other keywords are absent, they are
   * simply skipped.
   * 
   * @param keywords the list of keywords to remove.
   */
  public void removeKeywords(List<String> keywords) {

    /*
     * Loop through the list of supplied keywords. Remove any that are present in either the strings
     * or doubles maps. If the keyword is not present in either map, simply do nothing.
     */
    for (String key : keywords) {
      if (doubles.containsKey(key)) {
        doubles.remove(key);
      } else if (strings.containsKey(key)) {
        strings.remove(key);
      }
    }
  }

  /**
   * Removes a single keyword and its associated data values from a pool. This routine is a no-op if
   * the keyword is not present in the pool.
   * 
   * @param keyword the keyword to remove, if present
   */
  public void removeKeyword(String keyword) {
    if (doubles.containsKey(keyword)) {
      doubles.remove(keyword);
    } else if (strings.containsKey(keyword)) {
      strings.remove(keyword);
    }
  }

  /**
   * Clears all keywords and their associated data values from a pool.
   */
  public void clear() {
    doubles.clear();
    strings.clear();
  }

  /**
   * Verify that the add and append routine inputs are reasonable for inclusion in the kernel pool.
   * This routine consolidates the UnsupportedOperationException handler code.
   * 
   * @param key
   * @param values
   * 
   * @throws UnsupportedOperationException if either of the two inputs do not meet the requirements
   *         for insertion into the KernelPool.
   */
  private void checkInputs(String key, List<?> values) {
    /*
     * First verify that key and values are both not null. Null assignments to keys are invalid, as
     * are assignments to null keys.
     */
    if (key == null) {
      throw new UnsupportedOperationException(
          "Inserting null keys into " + "the KernelPool is not permitted.");
    }

    if (values == null) {
      throw new UnsupportedOperationException(
          "Inserting null values into " + "the KernelPool is not permitted.");
    }

    if (values.size() < 1) {
      throw new UnsupportedOperationException(
          "There must be at least one " + "value in the list to insert into the KernelPool.");
    }

    for (int i = 0; i < values.size(); i++) {
      if (values.get(i) == null) {
        throw new UnsupportedOperationException(
            "The value at position: " + i + " in the list is null.");
      }
    }

  }

  /**
   * Check if there are any append operations in an input keyword values map that would conflict
   * with content specified in another map; remove any content from the other map that will be
   * replaced with values in the input. This method exists solely to manage the cross comparison of
   * two different data type's underlying maps. It checks that appends specified in the input map do
   * not conflict with those in the other type, and generates an exception when this occurs.
   * 
   * @param <T> the data type of the values associated with keywords in the input map
   * @param <S> the data type of the values associated with keywords in the map to merge contents
   *        into; this should not be the same as T
   * 
   * @param giver the map of keyword value assignments destined for merging
   * @param receiver the map of keyword value assignments to receive the contents of giver
   * 
   * @throws UnsupportedOperationException if any of the keywords in the giver map have an
   *         appendState set to true and exist in the receiver map.
   */
  private <T, S> void crossCheck(Map<String, KernelPoolValues<T>> giver,
      Map<String, KernelPoolValues<S>> receiver) {

    List<String> keysToRemove = new ArrayList<String>();

    for (String key : giver.keySet()) {

      if (receiver.containsKey(key)) {
        if (giver.get(key).appendState) {
          throw new UnsupportedOperationException("Attempt to append keyword " + key
              + " to the pool has failed.  Appending values of "
              + giver.get(key).values.get(0).getClass() + " to a keyword containing values of type "
              + receiver.get(key).values.get(0).getClass() + " is not permitted.");
        } else {
          keysToRemove.add(key);
        }
      }
    }

    /*
     * Remove any of the keys that were present in receiver but are to be replaced with ones from
     * giver.
     */
    for (String key : keysToRemove) {
      receiver.remove(key);
    }
  }

}
