package picante.mechanics.providers.lockable;

/**
 * Defines a supplier of lock objects to use with the {@link LockableEphemerisProvider} and
 * {@link LockableFrameProvider}
 *
 */
public interface LockSupplier {

  /**
   * Supplies a lock object to be used for the given provider and function.
   * 
   * @param provider reference to the requesting provider either {@link LockableEphemerisProvider}
   *        or {@link LockableFrameProvider}
   * @param function reference to the function for which a lock is to be provided
   * 
   * @return the lock object
   */
  Object getLock(Object provider, Object function);

}
