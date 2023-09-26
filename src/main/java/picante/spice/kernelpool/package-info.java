/**
 * Provides an implementation of the NAIF text kernel data model.
 * <p>
 * The NAIF text kernel is a keyword equals value data repository that associated with string
 * keywords a list of values, either numeric or string. In this implementation the numeric data is
 * captured by the <code>Double</code> class and the string data by the <code>String</code> class.
 * </p>
 * <p>
 * There are two basic models provided here, the first, <code>BasicKernelPool</code> simply
 * implements the data model. The immediate subclass of this basic class, <code>KernelPool</code>,
 * provides the data model along with a listener interface that allows code to respond to changes in
 * an instance of the pool through a simple listener interface and additional registration methods.
 * In practice, when developing code against this model, you will typically utilize the KernelPool
 * subclass directly. The only reason to utilize the BasicKernelPool is if you have a large number
 * of keyword = value assignments to load, in one shot, into an existing KernelPool instance via the
 * {@link picante.spice.kernelpool.KernelPool#load(BasicKernelPool)} method.
 * </p>
 * <p>
 * The actual text format is documented in the NAIF document "Kernel Required Reading". From the
 * parser's perspective, a synopsis of the text kernel format is provided in its documentation here:
 * {@link picante.spice.kernelpool.parser.TextKernelParser}.
 * </p>
 * <p>
 * Text kernels are used to capture a variety of SPICE input data, in particular leapseconds, the
 * IAU rotation models for planets, reference frame information, and instrument specific parameters.
 * This software provides a mechanism to access the data available in kernels such as:
 * <ul>
 * <li>Leapseconds Kernels (LSK)</li>
 * <li>Text-based Planetary Constants Kernels (PCK)</li>
 * <li>Frame Kernels (FK)</li>
 * <li>Spacecraft Clock Kernels (SCLK)</li>
 * <li>Instrument Kernels (IK)</li>
 * </ul>
 * While this package provides the model necessary to extract information from this particular
 * format of file, it does not provide any of the specific implementations required by these various
 * types of kernels. Such code is built upon classes in this package.
 * </p>
 * <b>Related Documentation</b>
 * <p>
 * The following are NAIF reference documents that may be of use in utilizing the text kernel
 * format. These are all distributed with the NAIF toolkit directly from their website at:
 * <a href="http://naif.jpl.nasa.gov">http://naif.jpl.nasa.gov</a>
 * </p>
 * <ul>
 * <li><a href="ftp://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/html/individual_docs/intrdctn.html">
 * Introduction to SPICE</a></li>
 * <li><a href=
 * "ftp://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/html/individual_docs/kernel.html">Kernel
 * Required Reading</a></li>
 * <li><a href="ftp://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/html/individual_docs/sclk.html">
 * Spacecraft Clock Kernel Required Reading</a></li>
 * <li><a href=
 * "ftp://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/html/individual_docs/pck.html">Planetary
 * Constants Kernel Required Reading</a></li>
 * <li><a href=
 * "ftp://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/html/individual_docs/frames.html">Frame Kernel
 * Required Reading</a></li>
 * </ul>
 */
package picante.spice.kernelpool;
