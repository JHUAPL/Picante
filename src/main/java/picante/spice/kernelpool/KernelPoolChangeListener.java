package picante.spice.kernelpool;

/**
 * An interface to respond to kernel pool change events. Listeners interested in receiving kernel
 * pool change events should implement this interface and add themselves to the desired pools.
 * 
 * At the moment the kernel pool change event class does not contain a reference to the kernel pool
 * that generated the event. Classes interested in listening to multiple pools for change events
 * will need to implement multiple listeners or do something equally sophisticated.
 * 
 * Note: this is the subject of some debate at the moment and may change in a future release of the
 * kernel pool package.
 */
public interface KernelPoolChangeListener {

  /**
   * Receive notification of a change of the contents of a kernel pool.
   * 
   * @param event an event describing the kernel pool changes.
   */
  public void poolChanged(KernelPoolChangeEvent event);

}
