package picante.mechanics.providers.lockable;

/**
 * Interface for retrieving a lock that is to be used to synchronize various operations on an
 * instance.
 */
interface Lockable {

  /**
   * Retrieve the object on which to synchronize operations.
   */
  Object getLock();

}
