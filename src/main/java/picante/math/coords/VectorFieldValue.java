package picante.math.coords;

interface VectorFieldValue<C> {

  /**
   * Get the position component.
   * 
   * @return the position component
   */
  public C getPosition();

  /**
   * The value associated with that position.
   * 
   * @return the value associated with that position
   */
  public C getValue();

}
