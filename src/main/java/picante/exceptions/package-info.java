/**
 * Package containing general exceptions presented by the crucible library.
 * <p>
 * This package contains the parent classes of exception classes created in support of the crucible
 * library. In general, exceptions contained within the library should descend from either
 * <code>CrucibleException</code> or <code>CrucibleRuntimeException</code>.
 * </p>
 * <p>
 * This will allow developers utilizing the library the ability to trap exceptions specific to
 * crucible itself without much difficulty.
 * </p>
 * 
 * @see picante.exceptions.PicanteException
 * @see picante.exceptions.PicanteRuntimeException
 */
package picante.exceptions;
