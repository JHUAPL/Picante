package picante.collections;

import java.util.Iterator;
import java.util.NoSuchElementException;

import com.google.common.collect.UnmodifiableIterator;

/**
 * Simple abstract class that implements the {@link Iterator} interface from a source that is
 * organized by index. Simply supply implementations for the two abstract methods and you'll have an
 * {@link Iterator} that works properly.
 * 
 * @param <E> the element over which iteration is to be performed
 */
public abstract class AbstractIndexedIterator<E> extends UnmodifiableIterator<E> {

  private int index = 0;

  @Override
  public boolean hasNext() {
    return index < elements();
  }

  @Override
  public E next() {
    if (index < elements()) {
      /*
       * Post-increment index after retrieval.
       */
      return element(index++);
    }
    throw new NoSuchElementException("Unable to access element at index: " + index
        + " from indexed elements of length: " + elements());
  }

  /**
   * Provide the number of elements in the indexed list.
   * 
   * @return the number
   */
  protected abstract int elements();

  /**
   * Supplies the element associated with a particular index in the range [0, elements()-1]
   * respectively
   * 
   * @param index the index of interest
   * 
   * @return the element at index
   */
  protected abstract E element(int index);

}
