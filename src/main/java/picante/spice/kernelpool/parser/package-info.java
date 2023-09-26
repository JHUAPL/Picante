/**
 * Provides an implementation of a NAIF text kernel parser that acts on the ASCII content provided
 * by a <code>Reader</code>.
 * <p>
 * For a terse discussion of the text kernel format, refer to the parser class documentation
 * available here: {@link picante.spice.kernelpool.parser.TextKernelParser}. For full details, refer
 * to the NAIF documentation distributed with the SPICE toolkit or their website:
 * <a href="http://naif.jpl.nasa.gov">http://naif.jpl.nasa.gov</a>.
 * </p>
 * <p>
 * The classes present in this package are generated directly by JavaCC, the java compiler compiler.
 * As such, the source here should not be modified directly. IDEs will often report compiler
 * warnings, due to the way the code is generated, but it's best to just ignore them as the unit
 * test code proves it functions properly.
 * </p>
 * <p>
 * The actual source supplied to JavaCC is located off the repository trunk in the javacc directory,
 * under the crucible.spice.kernelpool.parser subdirectory. This parser is the result of a careful
 * reverse engineering of NAIF's text kernel parser by trial and error. In the process of this
 * reimplementation, several unique features of the NAIF supplied parser were uncovered. It is
 * possible to create a text kernel that NAIF's parser will accept, but this parser will not.
 * Similarly it is possible that a kernel will be successfully parsed by this parser that would
 * break NAIF's parser shipped with SPICELIB. Fortunately, in every uncovered case, these text
 * kernels are quite pathological, so hopefully it will not happen in practice.
 * </p>
 * <p>
 * This parser successfully parses, as the unit test code demonstrates, a variety of NAIF text
 * kernels:
 * <ul>
 * <li>cas_v39.tf, the Cassini project frame kernel</li>
 * <li>cas00093.tsc, the Cassini spacecraft clock kernel</li>
 * <li>messenger_154.tsc, the MESSENGER spacecraft clock kernel</li>
 * <li>MRO_SCLKSCET.00006.65536.tsc, the MRO high resolution spacecraft clock kernel</li>
 * <li>naif0008.tls, the leapseconds kernel</li>
 * <li>pck00008.tpc, the generic text planetary constants kernel</li>
 * </ul>
 * </p>
 * <p>
 * The NAIF parser is implemented by the SPICE routines: LDPOOL, ldpool_c, and cspice_ldpool in the
 * FORTRAN, C, and IDL toolkits respectively. Refer to the documentation and source for specifics.
 * </p>
 * <b>Related Documentation</b>
 * <ul>
 * <li><a href="https://javacc.dev.java.net">JavaCC Website</a></li>
 * </ul>
 */
package picante.spice.kernelpool.parser;
