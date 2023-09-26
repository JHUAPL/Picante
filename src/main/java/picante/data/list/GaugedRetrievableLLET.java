package picante.data.list;

/**
 * Marker interface extension of the {@link GaugedRetrievable} interface that provides both strictly
 * last less than and last less than or equal to searches.
 * 
 * @param <R> the type of record required by the implementation
 */
public interface GaugedRetrievableLLET<R> extends GaugedRetrievableLLE<R>, GaugedRetrievableLLT<R> {

}
