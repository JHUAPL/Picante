/**
 * Provides an implementation of a NAIF text kernel time &#64;specification parser.
 * <p>
 * Essentially this class implements a restricted set of NAIF's routine: TPARSE, a generic time
 * string parser. Since the text kernel places additional restrictions on the format of strings it
 * accepts in &#64; date specifications, this parser is not as generic as what TPARSE would support.
 * It does however accept a nearly complete (practically so) implementation of a parser that
 * converts dates in SPICE text kernels into their numeric analogs.
 * </p>
 * <p>
 * The classes present in this package are generated directly by JavaCC, the java compiler compiler.
 * As such, the source here should not be modified directly. Eclipse will likely report compiler
 * warnings, due to the way the code is generated, but it's best to just ignore them as the unit
 * test code proves it functions properly.
 * </p>
 * <p>
 * The actual source supplied to JavaCC is located off the repository trunk in the javacc directory,
 * under the crucible.spice.kernelpool.parser.time subdirectory. This parser is the result of a
 * selective reverse engineering of NAIF's generic time parser routine: TPARSE. In the process of
 * this reimplementation, a few unexpected, and possibly undocumented features of their parser were
 * uncovered. Refer to {@link picante.spice.kernelpool.parser.time.TimeParser} for details.
 * </p>
 * <h2>Related Documentation</h2>
 * <p>
 * Aside from the TPARSE module header, NAIF's best single point of documentation for their generic
 * time parser is the required reading document listed below:
 * </p>
 * <ul>
 * <li><a href="ftp://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/html/individual_docs/time.html">Time
 * Required Reading</a></li>
 * </ul>
 */
package picante.spice.kernelpool.parser.time;
