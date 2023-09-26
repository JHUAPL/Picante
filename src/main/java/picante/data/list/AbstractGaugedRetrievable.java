package picante.data.list;

/**
 * An abstract class that implements both required search algorithms utilizing a simple binary
 * search with the {@link GaugedRetrievable#getGauge(int)} method.
 * <p>
 * If the efficiency of searches over an implementation of the interface is a concern, or if binary
 * search utilizing the {@link GaugedRetrievable#getGauge(int)} method is sufficient, then simply
 * subclass this abstract class and supply the standard retrieval methods.
 * </p>
 * 
 * @param <R> the type of record required by the implementation
 */
public abstract class AbstractGaugedRetrievable<R> implements GaugedRetrievableLLET<R> {

  private final Searcher searcher = new Searcher(this);

  @Override
  public int indexLastLessThanOrEqualTo(double time) {
    return searcher.indexLastLessThanOrEqualTo(time);
  }

  @Override
  public int indexLastLessThan(double time) {
    return searcher.indexLastLessThan(time);
  }

}
