package picante.mechanics;

/**
 * Helper implementation of the Coverage interface that provides properly implemented
 * {@link Object#toString()}, {@link Object#equals(Object)}, and {@link Object#hashCode()} methods.
 * 
 * @see Coverages#equalsImplementation(Coverage, Object)
 * @see Coverages#toStringImplementation(Coverage)
 * @see Coverages#hashCodeImplementation(Coverage)
 * 
 */
public abstract class AbstractCoverage implements Coverage {

  @Override
  public int hashCode() {
    return Coverages.hashCodeImplementation(this);
  }

  @Override
  public boolean equals(Object obj) {
    return Coverages.equalsImplementation(this, obj);
  }

  @Override
  public String toString() {
    return Coverages.toStringImplementation(this);
  }

}
