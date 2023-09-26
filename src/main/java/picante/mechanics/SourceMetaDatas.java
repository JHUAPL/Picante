package picante.mechanics;

public class SourceMetaDatas {
  /**
   * simplest metadata implementation (uses the given name)
   */
  public static SourceMetaData create(final String name) {
    return new SourceMetaData() {
      @Override
      public String getName() {
        return name;
      }
    };
  }
}
