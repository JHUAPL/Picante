/**
 * The code in this package captures the fundamental implementation of the NAIF SCLK system. There
 * are a few deviations from the NAIF system that are worth pointing out:
 * <ul>
 * <li>The {@link picante.spice.kernel.tk.sclk.SCLK} class provides the equivalent of the
 * NAIF SCLK string. However, it is not internally utilizing a string, and as such may deviate from
 * future implementations of SCLKs.</li>
 * <li>The SCLK class requires that you specify a partition, and the implementation of the
 * underlying conversions do not apply any implicit partition selection. This was intentional, due
 * to the confusion that partitions generally cause. Higher level frameworks that select partitions
 * may ultimately be built on the code here.</li>
 * </ul>
 */
package picante.spice.kernel.tk.sclk;

