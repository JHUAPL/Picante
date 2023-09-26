package picante.spice.kernelpool;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import picante.spice.kernelpool.parser.TextKernelParser;

/**
 * Implementation of the NAIF text kernel pool. This implementation, unlike its parent class is a
 * fully featured kernel pool with a change listener mechanism that permits views of this model to
 * receive change notifications.
 * 
 * For details regarding the specifics of NAIF's text kernel format, refer to the companion parser
 * class. Some important points to note, this class provides an API for mutating and accessing the
 * contents of a text kernel. That having been said, it is entirely possible through the API
 * presented here to modify the contents of the kernel pool in such a way that it would not be
 * possible to arrive at via loading actual NAIF text kernels. For example, the following code would
 * result in a successful keyword value assignment:
 * 
 * <pre>
 * KernelPool pool = new KernelPool();
 * List&lt;String&gt; values = new ArrayList&lt;String&gt;();
 * values.add(&quot;Value&quot;);
 * pool.appendString(&quot;A=&quot;, values);
 * </pre>
 * 
 * Though, the parser explicitly forbids keywords that end in "=". In a sense this is just a
 * registry that associates keywords with their lists of values. It makes little or no attempt at
 * validating the content against what the actual parser would expect.
 * 
 * There are a few things that this class does attempt to prevent from happening:
 * 
 * <ul>
 * <li>Assignment of values to a null keyword is not allowed.</li>
 * <li>Assignment of a null list to a keyword is not allowed.</li>
 * <li>Assigning empty lists to keywords is not allowed.</li>
 * <li>Appending empty lists to keywords is not allowed.</li>
 * <li>Appending one data type to an existing keyword of the other is not allowed.</li>
 * </ul>
 * 
 * The listener mechanism utilizes the WeakReference class, so when listeners are registered and the
 * only remaining reference is the weak reference in this listener class, the garbage collector
 * collects the listener. This allows simplified implementation of the KernelPoolChangeListener
 * interface. It does however create one small caveat, if you register listeners using an anonymous
 * inner class in the usual:
 * 
 * <pre>
 *     pool.addListener(new KernelPoolChangeListener() { ... } );
 * </pre>
 * 
 * fashion, these listeners will be collected by the garbage collector and gracefully removed from
 * the list. This happens due to the fact that the listeners are wrapped in WeakReference objects.
 * Simply hold onto a reference to the anonymous inner class in your code, and this will not happen:
 * 
 * <pre>
 *     KernelPoolChangeListener myListener = new KernelPoolChangeListener() { ... } );
 *     pool.addListener(myListener);
 * </pre>
 * 
 * @see TextKernelParser
 */
public class KernelPool extends BasicKernelPool {

  /**
   * Change event handler that manages the listeners and their firing.
   */
  protected final KernelPoolChangeEventHandler handler = new KernelPoolChangeEventHandler();

  /**
   * Default constructor that builds an empty pool.
   */
  public KernelPool() {
    super();
  }

  /**
   * Copy constructor that copies an existing pool's contents into a newly created KernelPool.
   * 
   * @param pool the pool to copy contents of
   */
  public KernelPool(BasicKernelPool pool) {
    super(pool);
  }

  /**
   * Add a change listener to the pool
   * 
   * @param listener the listener to add
   */
  public void addListener(KernelPoolChangeListener listener) {
    handler.addChangeListener(listener);
  }

  /**
   * Remove the first occurrence of a change listener from the pool
   * 
   * @param listener the listener to remove
   */
  public void removeListener(KernelPoolChangeListener listener) {
    handler.removeChangeListener(listener);
  }

  @Override
  public void addDoubles(String key, List<Double> values) {

    /*
     * Just check to see if key is already in the pool.
     */
    boolean modified = super.isStringValued(key) || super.isDoubleValued(key);

    super.addDoubles(key, values);

    handler.fireAdditionEvent(key, modified);
  }

  @Override
  public void addStrings(String key, List<String> values) {

    /*
     * Just check to see if key is already in the pool.
     */
    boolean modified = super.isStringValued(key) || super.isDoubleValued(key);

    super.addStrings(key, values);

    handler.fireAdditionEvent(key, modified);
  }

  @Override
  public void appendDoubles(String key, List<Double> values) {

    /*
     * Just check to see if key is already present in either of the maps. Note: we're relying on the
     * fact that an append to strings won't take place without throwing an exception.
     */
    boolean modified = super.isDoubleValued(key);

    super.appendDoubles(key, values);

    handler.fireAppendingEvent(key, modified);
  }

  @Override
  public void appendStrings(String key, List<String> values) {

    /*
     * Just check to see if key is already present in either of the maps. Note, we are relying on
     * the fact that appending to a double valued keyword will generate an exception.
     */
    boolean modified = super.isStringValued(key);

    super.appendStrings(key, values);

    /*
     * Fire the appropriate event.
     */
    handler.fireAppendingEvent(key, modified);

  }

  @Override
  public void clear() {

    /*
     * Build up a list of keywords that are about to go away. We have to build up a copy of the
     * keySets because they will change after invoking the clear().
     */
    Set<String> deleted = new HashSet<String>(super.getKeywords());

    super.clear();

    /*
     * Fire the event listener.
     */
    handler.fireClearingEvent(deleted);
  }

  @Override
  public void load(UnwritableKernelPool pool) {

    /*
     * Determine the list of keywords who will have their values change as a result of a successful
     * merge of pool into this pool.
     */
    Set<String> modified = new HashSet<String>();
    Set<String> added = new HashSet<String>();

    for (String key : pool.getKeywords()) {
      if (super.isStringValued(key) || super.isDoubleValued(key)) {
        modified.add(key);
      } else {
        added.add(key);
      }
    }

    super.load(pool);

    /*
     * Fire the loading change event.
     */
    handler.fireLoadingEvent(modified, added);
  }

  @Override
  public void removeKeywords(List<String> keywords) {

    /*
     * Build up the list of keywords that are about to be removed.
     */
    Set<String> deleted = new HashSet<String>();

    for (String key : keywords) {
      if (super.isStringValued(key) || super.isDoubleValued(key)) {
        deleted.add(key);
      }
    }

    super.removeKeywords(keywords);

    /*
     * Fire the listener if keywords were actually removed.
     */
    if (deleted.size() > 0) {
      handler.fireRemovalEvent(deleted);
    }
  }

  @Override
  public void removeKeyword(String keyword) {

    /*
     * See if the keyword is present in the pool; process the removal and fire the event handler.
     */
    if (super.isStringValued(keyword) || super.isDoubleValued(keyword)) {
      super.removeKeyword(keyword);
      Set<String> deleted = new HashSet<String>();
      deleted.add(keyword);
      handler.fireRemovalEvent(deleted);
    }

  }

}
