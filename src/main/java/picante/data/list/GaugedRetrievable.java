package picante.data.list;

/**
 * Extension of the list interface that requires a companion method to obtain a gauged quantity
 * (typically time) associated with a particular record. The elements of the {@link Retrievable} are
 * assumed to be sorted in non-decreasing order against the gauge (and generally strictly
 * increasing).
 * 
 * @param <R> the type of record required by the implementation
 */
public interface GaugedRetrievable<R> extends Retrievable<R>, Gauged {

}
