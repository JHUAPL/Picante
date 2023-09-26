package picante.spice.kernelpool;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;

/**
 * Kernel pool change event handler that manages listeners and the firing of the various events
 * generated by the kernel pool methods.
 */
class KernelPoolChangeEventHandler {

  /**
   * Create an empty set to fire off to the event constructor.
   */
  private static final Set<String> EMPTY_SET = new HashSet<String>();

  /**
   * The list of listeners that are to be fired and managed.
   */
  private List<WeakReference<KernelPoolChangeListener>> listeners =
      new ArrayList<WeakReference<KernelPoolChangeListener>>();

  /**
   * Add a kernel pool change listener to the handler.
   * 
   * @param listener the listener to add
   */
  void addChangeListener(KernelPoolChangeListener listener) {
    listeners.add(new WeakReference<KernelPoolChangeListener>(listener));
  }

  /**
   * Remove the first occurrence of a kernel pool change listener from the handler. Removing a
   * listener that does not exists does nothing.
   * 
   * @param listener the listener to remove
   */
  void removeChangeListener(KernelPoolChangeListener listener) {

    /*
     * We have no recourse but to iterate over the entire collection of listeners and remove the
     * first one that matches.
     */
    ListIterator<WeakReference<KernelPoolChangeListener>> iterator = listeners.listIterator();

    while (iterator.hasNext()) {
      WeakReference<KernelPoolChangeListener> reference = iterator.next();

      if (reference.get() == listener) {
        iterator.remove();
        return;
      }
    }

  }

  /**
   * Fire a kernel pool clearing event.
   * 
   * @param deleted the list of all keywords previously defined in the kernel pool.
   */
  void fireClearingEvent(Set<String> deleted) {
    KernelPoolChangeEvent event =
        new KernelPoolChangeEvent(KernelPoolEventType.CLEAR, deleted, EMPTY_SET, EMPTY_SET);
    fireListeners(event);
  }

  /**
   * Fire a keyword removal event.
   * 
   * @param deleted the list of all keywords that were previously defined in the kernel pool and
   *        were removed.
   */
  void fireRemovalEvent(Set<String> deleted) {
    KernelPoolChangeEvent event =
        new KernelPoolChangeEvent(KernelPoolEventType.REMOVE, deleted, EMPTY_SET, EMPTY_SET);
    fireListeners(event);
  }

  /**
   * Fire an addition event.
   * 
   * @param key the keyword whose contents have been modified or added as a result of the addition
   *        event.
   * @param modified a boolean when true indicates if key existed in the pool prior to the addition
   *        event, and false otherwise.
   */
  void fireAdditionEvent(String key, boolean modified) {

    Set<String> set = new HashSet<String>(1);

    KernelPoolChangeEvent event;

    set.add(key);

    if (modified) {
      event = new KernelPoolChangeEvent(KernelPoolEventType.ADD, EMPTY_SET, set, EMPTY_SET);
    } else {
      event = new KernelPoolChangeEvent(KernelPoolEventType.ADD, EMPTY_SET, EMPTY_SET, set);
    }

    fireListeners(event);
  }

  /**
   * Fire an appending event.
   * 
   * @param key the keyword whose contents have been modified or added as a result of the appending
   *        event.
   * @param modified a boolean when true indicates if key existed in the pool prior to the addition
   *        event, and false otherwise.
   */
  void fireAppendingEvent(String key, boolean modified) {

    Set<String> set = new HashSet<String>(1);

    KernelPoolChangeEvent event;

    set.add(key);

    if (modified) {
      event = new KernelPoolChangeEvent(KernelPoolEventType.APPEND, EMPTY_SET, set, EMPTY_SET);
    } else {
      event = new KernelPoolChangeEvent(KernelPoolEventType.APPEND, EMPTY_SET, EMPTY_SET, set);
    }

    fireListeners(event);
  }

  /**
   * Fire the event associated with loading one pool into another.
   * 
   * @param modified a list of keywords in the receiving pool that were modified as a result of the
   *        load.
   * @param added a list of keywords not present in the receiving pool that were added as a result
   *        of the load.
   */
  void fireLoadingEvent(Set<String> modified, Set<String> added) {
    KernelPoolChangeEvent event =
        new KernelPoolChangeEvent(KernelPoolEventType.LOAD, EMPTY_SET, modified, added);
    fireListeners(event);
  }

  /**
   * Fire the event to all of the change listeners registered with this handler.
   * 
   * @param event the event to fire
   */
  private void fireListeners(KernelPoolChangeEvent event) {

    ListIterator<WeakReference<KernelPoolChangeListener>> iterator = listeners.listIterator();

    while (iterator.hasNext()) {

      WeakReference<KernelPoolChangeListener> reference = iterator.next();

      KernelPoolChangeListener listener = reference.get();

      /*
       * If the reference is null, then clear it from the list.
       */
      if (listener == null) {
        iterator.remove();
      } else {
        listener.poolChanged(event);
      }
    }
  }

}
