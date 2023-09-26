package picante.designpatterns;

/**
 * Interface describing a blueprint for a configuration of a builder. If you need code that applies
 * a configuration to a particular type of builder, then this interface should be utilized.
 * 
 * @param <B> the type of builder for which this blueprint applies
 */
public interface Blueprint<B extends Builder<?, ?>> {

  /**
   * Applies the blueprint to the builder.
   * 
   * @param builder the builder to have the configuration applied
   * 
   * @return a reference to the supplied builder for convenience
   */
  public B configure(B builder);

}
