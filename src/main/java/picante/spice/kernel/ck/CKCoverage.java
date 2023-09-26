package picante.spice.kernel.ck;

import picante.mechanics.Coverage;

/**
 * Marker interface indicating that this particular implementation of the <code>Coverage</code>
 * interface is tied to C-kernels and thus works in encoded SCLK.
 * <p>
 * <b>This is NOT providing ephemeris time as the {@link Coverage} interface would normally
 * provide.</b>
 * </p>
 */
public interface CKCoverage extends Coverage {

}
