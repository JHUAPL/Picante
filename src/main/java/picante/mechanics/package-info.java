/**
 * Package containing the elemental classes and interfaces necessary to implement state querying
 * functionality with a focus on celestial bodies and spacecraft.
 * <p>
 * Key interfaces are:
 * <ul>
 * <li>{@link picante.mechanics.PositionVectorFunction}</li>
 * <li>{@link picante.mechanics.StateVectorFunction}</li>
 * <li>{@link picante.mechanics.FrameTransformFunction}</li>
 * <li>{@link picante.mechanics.StateTransformFunction}</li>
 * <li>{@link picante.mechanics.EphemerisProvider}</li>
 * <li>{@link picante.mechanics.FrameProvider}</li>
 * </ul>
 * </p>
 * <p>
 * In general the above interfaces provide users with the bulk of their needs with regards to
 * accessing information provided by the code in this package. There are other interfaces and
 * classes that may be of general use, but this documentation focuses on getting users up to speed
 * quickly.
 * </p>
 * <p>
 * This package provides the interfaces necessary to interact with ephemeris and frame data.
 * However, actual implementations of these interfaces provided by the library are found in packages
 * underneath the crucible.mechanics.implementation package. For example,
 * {@link picante.mechanics.providers.reference.ReferenceEphemerisProvider}. The details of
 * instantiating various providers are left up to the specific implementation.
 * </p>
 * <p>
 * The state transform and state vector classes violate some of the basic principles of the weak
 * immutability design pattern, in that they hand out references to their internals. This was a
 * design decision made to accomodate the fact that these classes really act as buffers to move
 * information around in the library. For convenience and performance reasons, these fields are
 * exposed through the basic accessor methods. If they were not, implementors of most (if not all)
 * of the interfaces in this package would be required to create or store local memory to inject
 * values into the buffers.
 * </p>
 * <p>
 * A quick look in the crucible.mechanics.utilities package may save you some time. There are
 * several classes there that may assist in implementing or altering these interfaces in ways that
 * are generally useful.
 * </p>
 */
package picante.mechanics;
