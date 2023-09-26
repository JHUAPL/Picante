package picante.mechanics.providers.lockable;

/**
 * Enumeration that specifies the level at which the thread locking is to occur.
 */
public enum LockType implements LockSupplier {

  /**
   * Indicates that the derived functions should lock on the provider instance itself. This is the
   * most restrictive of all the locks.
   */
  PROVIDER {
    @Override
    public Object getLock(Object provider, @SuppressWarnings("unused") Object function) {
      return provider;
    }
  },

  /**
   * Indicates that the derived functions should lock on the individual functions supplied via the
   * sources to the constructor of the provider.
   */
  FUNCTION {

    @Override
    public Object getLock(@SuppressWarnings("unused") Object provider, Object function) {
      return function;
    }
  };


}
