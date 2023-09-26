/**
 * NAIF SPICE implementation of crucible data source interfaces. All the necessary adapters live
 * here. This package contains the basic software to allow SPICE data products to interact with
 * crucible software at a high level.
 * <p>
 * For starters, take a look at the {@link SpiceEnvironmentBuilder} and the {@link SpiceEnvironment}
 * classes. These classes provide relatively easy to use features to get SPICE kernels loaded and
 * adapted to the crucible interfaces.
 * </p>
 * <p>
 * None of these APIs are null friendly in any way. If you supply null to any of the SPICE
 * environment builder inputs, it may result in a runtime exception, incorrect results, or a dozen
 * other unexpected things. At the current time nothing is done to prevent or protect the integrity
 * of the system from user supplied null values. This is a future enhancement under consideration.
 * </p>
 * <p>
 * Currently supported SPICE kernels and features:
 * <ul>
 * <li>SPK evaluators for types: 1,2,3</li>
 * <li>CK evaluators for types: 3</li>
 * <li>Limited SCLK conversion necessary to support CK evaluation</li>
 * </ul>
 * </p>
 * <p>
 * Notes about Ephemeris ID code bindings:
 * <ul>
 * <li>You may bind any single &quot;equivalent&quot; code to an actual ephemeris object once.</li>
 * <li>You can replace the default bindings for various objects defined in the crucible core
 * library.</li>
 * <li>You may alter the default binding for objects defined in the core, but you will have to
 * replace the binding with another or end up violating the first rule.</li>
 * <li>In the absence of any particular binding, one will be created automatically for you. This
 * enables you to chain states through objects in which you are not interested safely.</li>
 * <li>Remember, multiple strings may map to a single integer code for an ephemeris object.
 * </ul>
 * </p>
 * <p>
 * This package provides implementations of various SPICE data structures and adapters for use with
 * crucible-core interfaces. In general the development goals have been to produce software that:
 * <ul>
 * <li>Recreates, to the bit, calculations from SPICE; wherever possible.</li>
 * <li>If it works with SPICE, it should work here; but the reverse may not be the case. In
 * particular the fixed length limitations with regards to the kernel pool keywords and content are
 * ignored.</li>
 * </ul>
 * As much as possible it has been designed to replicate what SPICE does. It is not, however, an
 * entirely faithful recreation of SPICE. Several deviations were intentional:
 * </p>
 * <ul>
 * <li>Fixed offset frames with left-handed matrices in their TKFRAME_#_MATRIX specifications (or
 * matrices that do not pass sufficiently close to a rotation for the constructors of
 * {@link picante.math.vectorspace.RotationMatrixIJK}) will result in instantiation errors. This may
 * be relaxed in the future.</li>
 * <li>The parser for time data values in the kernel pool deviates from the standard parser NAIF
 * utilizes. See {@link picante.spice.kernelpool.parser.TextKernelParser} for details.</li>
 * <li>The SPICE environment builder is fail-fast, and attempts to fully build every available and
 * supported SPICE data source into the appropriate crucible adapters. Unlike SPICE, which permits
 * broken data sources to be loaded so long as you do not interact with them, this system breaks
 * either when interacting with the builder or when you attempt to build the environment.</li>
 * <li>Supported SPK types: 1,2,3,5,8,9,12,13,17</li>
 * <li>Supported CK types: 2,3</li>
 * <li>Supported binary PCK types: 2</li>
 * </ul>
 */
package picante.spice;

