/**
 * Package containing code for locating when functions meet various criteria: local extrema,
 * specific values, or when a function is greater than or less than specified value.
 * <p>
 * The {@link picante.roots.RootFinder} class is the service API used to compute the periods
 * of interest.
 * </p>
 * <p>
 * The {@link picante.roots.BooleanStateFinder} class is the service API used to compute
 * periods when a {@link picante.roots.UnivariateBooleanFunction} is true or false.
 * </p>
 * 
 * 
 * @see picante.roots.RootFinder#create()
 * @see picante.roots.RootFinder#createWithAbsoluteAccuracy(double)
 * @see picante.roots.Steppers
 */
package picante.roots;
