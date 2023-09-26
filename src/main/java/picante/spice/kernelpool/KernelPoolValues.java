package picante.spice.kernelpool;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Kernel pool values class that aggregates keyword state information and data content. This class
 * is used to track the state of a particular keywords addition to the kernel pool, along with its
 * list of values.
 * 
 * @param <T> either String or Double, depending on the supported data type.
 */
class KernelPoolValues<T> {

  /**
   * Append state, indicating whether in this kernel pool instance, the corresponding keyword was
   * appended to the pool.
   */
  boolean appendState;

  /**
   * The data content associated with the keyword.
   */
  List<T> values;

  /**
   * Simple constructor that just sets the state and data content.
   * 
   * @param appendState keyword append state, true if keyword is appended to the pool, false
   *        otherwise.
   * @param values data content associated with the keyword state.
   */
  public KernelPoolValues(boolean appendState, List<T> values) {
    this.appendState = appendState;
    this.values = Collections.unmodifiableList(new ArrayList<T>(values));
  }

  /**
   * A copy constructor of sorts, it takes the existing value and appends the ones in the supplied
   * list to it.
   * 
   * @param poolValue
   * @param valuesToAppend
   */
  public KernelPoolValues(KernelPoolValues<T> poolValue, List<T> valuesToAppend) {
    this.appendState = poolValue.appendState;
    ArrayList<T> values = new ArrayList<T>(poolValue.values);
    values.addAll(valuesToAppend);
    this.values = Collections.unmodifiableList(values);
  }

  /**
   * Copy constructor.
   * 
   * @param poolValue poolValue to copy.
   */
  public KernelPoolValues(KernelPoolValues<T> poolValue) {
    this.appendState = poolValue.appendState;
    this.values = Collections.unmodifiableList(new ArrayList<T>(poolValue.values));
  }

}
