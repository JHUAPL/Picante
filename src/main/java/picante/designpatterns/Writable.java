package picante.designpatterns;

/**
 * A interface which captures the Weak-Immutability pattern. Referred in Crucible as Writable and
 * Unwritable. The Unwritable version of the class has fields which cannot be set (protected
 * fields). Thus, it is Unwritable. The Writable version extends the Unwritable and provides setters
 * to the fields, with a basic setter defined by this interface. The implementor of the writable
 * class may and probably should write additional setters.
 * <p>
 * Note: An inherit shortcoming of this interface was noticed. If you attempt to write a class of
 * the following templated form:
 * 
 * <pre>
 * class Something<U, W extends Writable<U, W>>
 * </pre>
 * 
 * is not allowed by the compiler. This functionality seems to come up quite a bit when you are
 * trying to make generic classes that need to know simultaneously use the U and W versions of some
 * other class.
 * </p>
 * <p>
 * Another useful feature to add when creating classes that implement this pattern is the static
 * copyOf method on the unwritable class. It should look something like this:
 * 
 * <pre>
 * public static UnwritableClass copyOf(UnwritableClass instance) {
 *    if ( instance.getClass().equals(UnwritableClass.class) {
 *       return instance;
 *    }
 *    return new UnwritableClass(instance); // the copy constructor
 * }
 * </pre>
 * 
 * </p>
 * 
 * @author G.K.Stephens
 * 
 * @param <U> The unwritable version of the class that will be extending this interface.
 * @param <W> The class (writable) that will be extending this interface.
 */
public interface Writable<U, W> {

  /**
   * 
   * @param u
   * @return a reference to the instance for convenience
   */
  W setTo(U u);

  /**
   * 
   * @author stephgk1
   * 
   * @param <U>
   * @param <W>
   */
  public interface ImplementationInterface<U, W extends U> extends Writable<U, W> {
  }

}
