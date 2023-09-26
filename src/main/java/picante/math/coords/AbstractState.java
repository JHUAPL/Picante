package picante.math.coords;

import static com.google.common.base.Preconditions.checkNotNull;
import picante.designpatterns.Writable;

/**
 * An abstract class that will help implementors of {@link State}. This class was made to help
 * minimize the amount of Boiler plate code implementors of afore mentioned interfaces would have to
 * write. In keeping with this spirit, it should also make the different implementations have a
 * similar outline. If the implementor is attempting to follow the Weak-Immutability pattern (
 * {@link Writable}), then you can only use this for the unwritable version, as the writable version
 * can't extend both the unwritable and this.
 * 
 * Everything that is protected in this class is meant to be exposed only for the Writable versions.
 * 
 * @author G.K.Stephens
 * 
 * @param <C> The unwritable version of a coordinate class.
 * @param <W> The writable version of a coordinate class.
 */
abstract class AbstractState<C> implements State<C> {
  /**
   * The field containing the buffer that holds the position component of the state.
   */
  private final C position;

  /**
   * The field containing the buffer that holds the velocity component of the state.
   */
  private final C velocity;

  /**
   * Creates a state.
   * 
   * @param position the position of one object relative to another.
   * 
   * @param velocity the time derivative of the supplied position.
   */
  public AbstractState(C position, C velocity) {
    this.position = checkNotNull(position);
    this.velocity = checkNotNull(velocity);
  }

  @Override
  public C getPosition() {
    return position;
  }

  @Override
  public C getVelocity() {
    return velocity;
  }

  @Override
  public String toString() {
    return "[position=" + position + ", velocity=" + velocity + "]";
  }

  /*
   * (non-Javadoc)
   * 
   * @see java.lang.Object#hashCode()
   */
  @Override
  public final int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((position == null) ? 0 : position.hashCode());
    result = prime * result + ((velocity == null) ? 0 : velocity.hashCode());
    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @SuppressWarnings("rawtypes")
  @Override
  public final boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (!this.getClass().isInstance(obj)) {
      return false;
    }
    AbstractState other = (AbstractState) obj;
    if (position == null) {
      if (other.position != null) {
        return false;
      }
    } else if (!position.equals(other.position)) {
      return false;
    }
    if (velocity == null) {
      if (other.velocity != null) {
        return false;
      }
    } else if (!velocity.equals(other.velocity)) {
      return false;
    }
    return true;
  }

}
