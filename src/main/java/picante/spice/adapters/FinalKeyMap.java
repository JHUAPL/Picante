package picante.spice.adapters;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * The purpose of this implementation of the {@link Map} interface is to provide a map where keys
 * may be added after instantiation, but no existing key present in the map may have its contents
 * modified.
 * <p>
 * This implementation delegates calls to methods in the interface wherever possible. Operations
 * that would not violate the final nature of the existing keys are allowed to complete without
 * error. However any violation of the key content already present in the map will result in an
 * {@link UnsupportedOperationException}.
 * </p>
 * 
 * @param <K> the key type
 * @param <V> the value type
 */
class FinalKeyMap<K, V> implements Map<K, V> {

  private static final String CLEAR_MESSAGE = "Attempt to clear non-empty map is not supported.";
  private static final String REPLACE_MESSAGE =
      "Attempt to replace key value pair is not supported: [";
  private static final String REMOVE_MESSAGE = "Attempt to remove key contents is not supported: [";

  /**
   * The reference to the original map supplied to the constuctor.
   */
  private final Map<K, V> map;

  FinalKeyMap() {
    this(new HashMap<K, V>());
  }

  /**
   * Constructs a view of the map that blocks modification of the key content once added.
   * 
   * @param map the map of which to provide a view. A reference is retained by this implementation,
   *        as methods typically delegate to the supplied map.
   */
  FinalKeyMap(Map<K, V> map) {
    this.map = map;
  }

  /**
   * {@inheritDoc}
   * 
   * Will effectively be a no-op for an empty map, but it will fail with an
   * {@link UnsupportedOperationException} otherwise.
   */
  @Override
  public void clear() {
    if (map.size() > 0) {
      throw new UnsupportedOperationException(CLEAR_MESSAGE);
    }
    map.clear();
  }

  @Override
  public boolean containsKey(Object key) {
    return map.containsKey(key);
  }

  @Override
  public boolean containsValue(Object value) {
    return map.containsValue(value);
  }

  /**
   * {@inheritDoc}
   * 
   * The return value is an unmodifiable view of the entry set.
   */
  @Override
  public Set<Entry<K, V>> entrySet() {
    return Collections.unmodifiableSet(map.entrySet());
  }

  @Override
  public boolean equals(Object o) {
    return map.equals(o);
  }

  @Override
  public V get(Object key) {
    return map.get(key);
  }

  @Override
  public int hashCode() {
    return map.hashCode();
  }

  @Override
  public boolean isEmpty() {
    return map.isEmpty();
  }

  /**
   * {@inheritDoc}
   * 
   * The return value is an unmodifiable view of the key set.
   */
  @Override
  public Set<K> keySet() {
    return Collections.unmodifiableSet(map.keySet());
  }

  /**
   * {@inheritDoc}
   * 
   * Any attempt to insert a key value pair, where the key exists and the value is different than
   * that already present in the map, as determined by equals(), will generate an
   * {@link UnsupportedOperationException}.
   */
  @Override
  public V put(K key, V value) {
    checkKeyValue(key, value);
    return map.put(key, value);
  }

  /**
   * {@inheritDoc}
   * 
   * Any attempt to insert a key value pair from the supplied map, where the key exists in the
   * instance and the value is different than that already present as determined by equals(), will
   * generate an {@link UnsupportedOperationException}. In this case the underlying map will not be
   * modified.
   */
  @Override
  public void putAll(Map<? extends K, ? extends V> t) {

    /*
     * Check the map to see if any of the supplied key value pairs present in t would violate the
     * final key contract.
     */
    for (K key : t.keySet()) {
      checkKeyValue(key, t.get(key));
    }

    map.putAll(t);
  }

  /**
   * Checks to see if the key value pair is acceptable for addition. If key is already present in
   * the map, then the map is consulted to see if the supplied value is equal (using equals()) to
   * the one stored in the map. An {@link UnsupportedOperationException} is generated if that is not
   * the case.
   * 
   * @param key the candidate key to insert
   * @param value the value to associate with key
   */
  void checkKeyValue(K key, V value) {
    if (map.containsKey(key)) {
      if (!map.get(key).equals(value)) {
        throw new UnsupportedOperationException(REPLACE_MESSAGE + key + ", " + value + "].");
      }
    }
  }

  /**
   * {@inheritDoc}
   * 
   * If the key is already present in the map, this will result in an
   * {@link UnsupportedOperationException}.
   */
  @Override
  public V remove(Object key) {
    if (map.containsKey(key)) {
      throw new UnsupportedOperationException(REMOVE_MESSAGE + key + "].");
    }
    return map.remove(key);
  }

  @Override
  public int size() {
    return map.size();
  }

  /**
   * {@inheritDoc}
   * 
   * Returns an unmodifiable view of the values stored in the wrapped map.
   */
  @Override
  public Collection<V> values() {
    return Collections.unmodifiableCollection(map.values());
  }

}
