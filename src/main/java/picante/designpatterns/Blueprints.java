package picante.designpatterns;


/**
 * Utility methods for creating and manipulating {@link Blueprint} instances.
 * 
 */
public class Blueprints {

  /**
   * Static utility method collection need not be instantiated.
   */
  private Blueprints() {}

  /**
   * Static final field that holds the empty blueprint implementation.
   */
  private static final Blueprint<?> EMPTY = new Blueprint<Builder<?, ?>>() {

    @Override
    public Builder<?, ?> configure(Builder<?, ?> builder) {
      return builder;
    }
  };

  /**
   * Creates a blueprint implementation that does nothing other than return a builder that it is
   * supplied.
   * 
   * @return a &quot;null&quot; configuration builder that applies no configuration
   */
  @SuppressWarnings("unchecked")
  public static <B extends Builder<?, ?>> Blueprint<B> empty() {
    return (Blueprint<B>) EMPTY;
  }

  /**
   * Creates a new blueprint that concatenates the two supplied blueprints into one.
   * <p>
   * The resultant blueprint is effectively the same as:
   * <code>second.configure(first.configure(builder);</code>
   * </p>
   * 
   * @param first the first blueprint to apply
   * @param second the second blueprint to apply
   * 
   * @return a newly created blueprint that applies the first, then second to a builder.
   */
  public static <B extends Builder<?, ?>> Blueprint<B> concat(final Blueprint<B> first,
      final Blueprint<B> second) {
    return new Blueprint<B>() {
      @Override
      public B configure(B builder) {
        return second.configure(first.configure(builder));
      }
    };
  }

  /**
   * Creates a new blueprint that concatenates the supplied blueprints.
   * <p>
   * The resultant blueprint is effectively the same as invoking first on the builder, then each of
   * the elements in the order they are listed on the argument list from left to right.
   * </p>
   * 
   * @param first the first blueprint to apply
   * @param blueprints the varargs list of blueprints to apply in order from left to right
   * 
   * @return a newly created blueprint that applies the first, then subsequent to a builder
   */
  @SafeVarargs
  public static <B extends Builder<?, ?>> Blueprint<B> concat(final Blueprint<B> first,
      final Blueprint<B>... blueprints) {
    return new Blueprint<B>() {

      @Override
      public B configure(B builder) {
        B localBuilder = first.configure(builder);
        for (Blueprint<B> blueprint : blueprints) {
          localBuilder = blueprint.configure(localBuilder);
        }
        return localBuilder;
      }
    };
  }

  /**
   * Creates a new blueprint that concatenates the supplied blueprints.
   * 
   * @param blueprints the iterable of blueprints to concatenate.
   * 
   * @return a newly created blueprint that applies supplied blueprints.
   */
  public static <B extends Builder<?, ?>> Blueprint<B> concat(
      final Iterable<? extends Blueprint<B>> blueprints) {
    return new Blueprint<B>() {

      @Override
      public B configure(B builder) {

        for (Blueprint<B> blueprint : blueprints) {
          blueprint.configure(builder);
        }
        return builder;
      }
    };
  }

}
