package picante.math.coords;

import picante.math.vectorspace.UnwritableVectorIJK;

/**
 * An interface which lays out the basic methods that should be on a state class.
 * 
 * @author G.K.Stephens
 * 
 * @param <C> should be {@link AbstractVector} or {@link UnwritableVectorIJK}
 */
interface State<C> {

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
  public C getVelocity();

}
