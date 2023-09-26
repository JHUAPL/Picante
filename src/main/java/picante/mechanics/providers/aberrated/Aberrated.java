package picante.mechanics.providers.aberrated;

/**
 * Simple interface to add the {@link AberrationCorrection} retrieval method to the extensions of
 * the vector function interfaces.
 */
public interface Aberrated {

  /**
   * Retrieves the correction that is being applied to the position and/or states returned from this
   * function.
   * 
   * @return the correction
   */
  public AberrationCorrection getCorrection();

}
