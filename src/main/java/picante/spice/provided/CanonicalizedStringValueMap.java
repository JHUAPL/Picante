package picante.spice.provided;

import java.util.HashMap;
import java.util.Map;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.ForwardingMap;
import picante.spice.Utilities;

/**
 * This class exists to support storing ephemeris and frame name values in a map that are
 * canonicalized according to the standard NAIF equivalence classes for names.
 * 
 * @param <K>
 */
public class CanonicalizedStringValueMap<K> extends ForwardingMap<K, String> {

  private final Map<K, String> map;

  public static <K> CanonicalizedStringValueMap<K> create() {
    return new CanonicalizedStringValueMap<K>(new HashMap<K, String>());
  }

  @VisibleForTesting
  CanonicalizedStringValueMap(Map<K, String> mapToWrap) {
    this.map = mapToWrap;
  }

  @Override
  protected Map<K, String> delegate() {
    return map;
  }

  @Override
  public String put(K key, String value) {
    return super.put(key, Utilities.canonicalizeSpiceName(value));
  }

  @Override
  public void putAll(Map<? extends K, ? extends String> map) {
    for (Entry<? extends K, ? extends String> entry : map.entrySet()) {
      super.put(entry.getKey(), Utilities.canonicalizeSpiceName(entry.getValue()));
    }
  }

}
