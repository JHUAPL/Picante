package picante.spice.provided;

import java.util.HashMap;
import java.util.Map;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.ForwardingMap;
import picante.spice.Utilities;

/**
 * This class exists simply to canonicalize the string keys on input to the map according to the
 * SPICE name ID binding conventions.
 * 
 * @param <V> the type of the values stored in the map
 */
class CanonicalizedStringKeyMap<V> extends ForwardingMap<String, V> {

  private final Map<String, V> map;

  public static <V> CanonicalizedStringKeyMap<V> create() {
    return new CanonicalizedStringKeyMap<V>(new HashMap<String, V>());
  }

  @VisibleForTesting
  CanonicalizedStringKeyMap(Map<String, V> mapToWrap) {
    this.map = mapToWrap;
  }

  @Override
  protected Map<String, V> delegate() {
    return map;
  }

  @Override
  public V put(String key, V value) {
    return super.put(Utilities.canonicalizeSpiceName(key), value);
  }

  @Override
  public void putAll(Map<? extends String, ? extends V> map) {
    for (Entry<? extends String, ? extends V> entry : map.entrySet()) {
      super.put(Utilities.canonicalizeSpiceName(entry.getKey()), entry.getValue());
    }
  }

}
