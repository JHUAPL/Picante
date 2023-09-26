package picante.spice.kernelpool;

/**
 * An enumeration defining the various kernel pool change event types.
 */
public enum KernelPoolEventType {

  /**
   * Indicates the pool has been cleared.
   */
  CLEAR,

  /**
   * Indicates the pool has had another pool loaded into it.
   */
  LOAD,

  /**
   * Indicates an add has occurred; note this means the pool has had keywords modified as a result
   * of an add method.
   */
  ADD,

  /**
   * Indicates an append has occurred; note this means the pool has had keywords modified as a
   * result of an append method.
   */
  APPEND,

  /**
   * Indicates keywords have been removed from the pool.
   */
  REMOVE;
}
