package picante.math.coords;

import static com.google.common.base.Preconditions.checkNotNull;

class AbstractVectorFieldValue<C> implements VectorFieldValue<C> {
  /**
   * The field containing the buffer that holds the position component of the state.
   */
  private final C position;

  /**
   * The field containing the buffer that holds the value component of the state.
   */
  private final C value;

  /**
   * Creates a state.
   * 
   * @param position the position of one object relative to another.
   * 
   * @param value the time derivative of the supplied position.
   */
  public AbstractVectorFieldValue(C position, C value) {
    this.position = checkNotNull(position);
    this.value = checkNotNull(value);
  }

  @Override
  public C getPosition() {
    return position;
  }

  @Override
  public C getValue() {
    return value;
  }

  @Override
  public String toString() {
    return "[position=" + position + ", value=" + value + "]";
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
    result = prime * result + ((value == null) ? 0 : value.hashCode());
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
    AbstractVectorFieldValue other = (AbstractVectorFieldValue) obj;
    if (position == null) {
      if (other.position != null) {
        return false;
      }
    } else if (!position.equals(other.position)) {
      return false;
    }
    if (value == null) {
      if (other.value != null) {
        return false;
      }
    } else if (!value.equals(other.value)) {
      return false;
    }
    return true;
  }

}
