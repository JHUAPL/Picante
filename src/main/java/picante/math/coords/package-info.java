/**
 * This package provides the ability to easily convert between Cartesian and several coordinate
 * systems (such as Spherical) and the associated states.
 * <p>
 * Key classes are:
 * <ul>
 * <li>{@link picante.math.coords.deprecated.CoordConverters}</li>
 * </ul>
 * </p>
 * 
 * <p>
 * The user should be able to get most of the desired functionality by using the
 * {@link picante.math.coords.deprecated.CoordConverters} class. This class contains static
 * methods allowing for the conversion from Cartesian coordinates to the intended coordinate and
 * vice versa. It also has the equivalent state conversion. If a user needs to convert between two
 * coordinate systems, they are forced to go through Cartesian. If this becomes a performance burden
 * for a user, the framework allows for direct coordinate conversion to be added. These static
 * methods are intended to be thread safe, although they need system tested.
 * </p>
 * 
 * <p>
 * When the user asks for a conversion, they will receive a concrete container class containing the
 * information for that coordinate e.g. SphericalCoord has three field for Radius, Colatitude, and
 * Longitude. These classes mirror the {@link picante.math.vectorspace.VectorIJK} and the
 * {@link picante.mechanics.StateVector} classes and also follow the
 * {@link picante.designpatterns.Writable} pattern.
 * </p>
 * 
 * <p>
 * The only public classes are the before mentioned container classes and the
 * {@link picante.math.coords.deprecated.CoordConverters} class.
 * </p>
 * 
 * <p>
 * Precautions were taken to make {@link picante.math.coords.deprecated.CoordConverters}
 * thread safe
 * </p>
 * 
 * <h1>for developers:</h1>>
 * 
 * <p>
 * The work is done by implementations of the {@link picante.math.coords.CoordConverter}
 * interface. This interface, along with its implementations are intended to be package private,
 * although they may be useful in other contexts. The {@link picante.math.coords.Jacobian}
 * interface, also a package private, is meant to assist the
 * {@link picante.math.coords.CoordConverter} implementations by providing a way to manipulate
 * the Jacobians.
 * </p>
 * 
 * <p>
 * If a new type is added to the CoordConverters, six classes must be added. Two Coordinate classes,
 * one unwritable and one unwritable. The unwritable should extend the abstract helper class
 * {@link picante.math.coords.Coordinate} and the writable should extend the writable, and
 * make public the setters inherited from the abstract class. Two State classes, one unwritable and
 * one unwritable. The state classes follow the same pattern, the unwritable extends an abstract
 * helper class {@link picante.math.coords.State} and the writable extends the unwritable. One
 * {@link picante.math.coords.CoordConverter} implementation, and one
 * {@link picante.math.coords.Jacobian} although this is not specifically required, it should
 * help. And then the four methods should be added to
 * {@link picante.math.coords.deprecated.CoordConverters}.
 * </p>
 */
package picante.math.coords;
