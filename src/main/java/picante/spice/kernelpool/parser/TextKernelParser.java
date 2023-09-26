package picante.spice.kernelpool.parser;

import java.io.Reader;
import java.io.StringReader;
import picante.spice.kernelpool.BasicKernelPool;

/**
 * A parser for the NAIF text kernel format.
 * <p>
 * This parser was generated using the JavaCC parser generator, version 5.0. It provides methods to
 * load the contents of a properly structured NAIF text kernel that is connected to a Reader into a
 * KernelPool.
 * </p>
 * <p>
 * If you are looking for the quick and dirty, parse this text kernel, look here:
 * {@link picante.spice.kernelpool.parser.TextKernelParser#parse}.
 * </p>
 * <p>
 * Despite the number of methods provided by the generated parser, there are only two methods that
 * really should be used for mainstream kernel loading: one that loads the contents of the text
 * kernel connected to the supplied Reader into a new KernelPool, and the other that adds the
 * contents of the Reader into an existing KernelPool.
 * </p>
 * <p>
 * A simple example that loads a text kernel file, "myfile.tk" into a newly created KernelPool:
 * 
 * <pre>
 * TextKernelParser parser = new TextKernelParser();
 * BasicKernelPool myPool = parser.parse(new FileReader(&quot;myfile.tk&quot;));
 * </pre>
 * 
 * Or, alternatively, if you want to load "myfile.tk" into an existing KernelPool, pool:
 * 
 * <pre>
 * TextKernelParser parser = new TextKernelParser();
 * parser.parseInto(pool, new FileReader(&quot;myfile.tk&quot;));
 * </pre>
 * 
 * The contents of "myfile.tk" will be added to pool.
 * </p>
 * <p>
 * NOTE: Though you may be tempted to invoke some of the other, JavaCC methods to access the parser,
 * your mileage may vary in this regard. The TextKernelParser is generated as a static parser, and
 * thus JavaCC generates code that only allows a single parser to exist. The two methods described
 * above manage the initialization and reinitialization of the existing static parser, so you, the
 * consumer do not have to do anything other than recognize the parser class is a singleton.
 * </p>
 * <p>
 * The NAIF text kernel format is described in detail in the "Kernel Required Reading" document
 * shipped with the generic SPICE toolkit. The relevant excerpt is included below:
 * </p>
 * <p>
 * As the name implies, SPICE text kernel files contain only ASCII text. An additional restriction
 * on the contents of SPICE text kernel files is that they contain no non-printing characters, such
 * as tabs or formfeeds.
 * 
 * We illustrate this format by way of example using an excerpt from a SPICE text planetary
 * constants kernel (PCK) file. The format description given below applies to all SPICE text
 * kernels; the specific data names shown below apply only to text PCK files.
 * 
 * Planets first. Each has quadratic expressions for the direction (RA, Dec) of the north pole and
 * the rotation of the prime meridian. Planets with satellites (except Pluto) also have linear
 * expressions for the auxiliary (phase) angles used in the nutation and libration expressions of
 * their satellites.
 * 
 * <pre>
 *        \begindata
 * 
 *        BODY399_POLE_RA        = (    0.      -0.64061614  -0.00008386  )
 *        BODY399_POLE_DEC       = (  +90.      -0.55675303  +0.00011851  )
 *        BODY399_PM             = (   10.21  +360.98562970  +0.          )
 *        BODY399_LONG_AXIS      = (    0.                                )
 * 
 *        BODY3_NUT_PREC_ANGLES  = (  125.045    -1935.53
 *                                    249.390    -3871.06
 *                                    196.694  -475263.
 *                                    176.630  +487269.65
 *                                    358.219   -36000.    )
 * 
 *        \begintext
 * </pre>
 * 
 * Each satellite has similar quadratic expressions for the pole and prime meridian. In addition,
 * some satellites have nonzero nutation and libration amplitudes. (The number of amplitudes matches
 * the number of auxiliary phase angles of the primary.)
 * 
 * <pre>
 *        \begindata
 * 
 *        BODY301_POLE_RA      = (  270.000   -0.64061614  -0.00008386   )
 *        BODY301_POLE_DEC     = (  +66.534   -0.55675303  +0.00011851   )
 *        BODY301_PM           = (   38.314  +13.1763581    0.           )
 *        BODY301_LONG_AXIS    = (    0.                                 )
 * 
 *        BODY301_NUT_PREC_RA  = (  -3.878  -0.120  +0.070  -0.017   0.     )
 *        BODY301_NUT_PREC_DEC = (  +1.543  +0.024  -0.028  +0.007   0.     )
 *        BODY301_NUT_PREC_PM  = (  +3.558  +0.121  -0.064  +0.016  +0.025  )
 * 
 *        \begintext
 * 
 *        Finally, we include the radii of the satellites and planets.
 * 
 *        \begindata
 * 
 *        BODY399_RADII    = (     6378.140    6378.140     6356.755  )
 *        BODY301_RADII    = (     1738.       1738.        1738.     )
 * </pre>
 * 
 * In this example are several comment blocks. All are introduced by the control word:
 * 
 * <pre>
 *        \begintext
 * </pre>
 * 
 * A comment block may contain any number of comment lines. Once a comment block has begun, no
 * special characters are required to introduce subsequent lines of comments within that block. A
 * comment block is terminated by the control word:
 * 
 * <pre>
 *        \begindata
 * </pre>
 * 
 * This control word also serves to introduce a block of data that will be stored in the kernel
 * pool. Each of these control words must appear on a line by itself.
 * 
 * Each variable definition consists of three components:
 * <ol>
 * <li>A variable name.</li>
 * <li>An assignment directive. This must be ``='' (direct assignment) or ``+='' (incremental
 * assignment).</li>
 * <li>A scalar or vector value.</li>
 * </ol>
 * Direct assignments supersede previous assignments, whereas incremental assignments are added to
 * previous assignments. For example, the series of assignments:
 * 
 * <pre>
 *        BODY301_NUT_PREC_RA  = -3.878
 *        BODY301_NUT_PREC_RA += -0.120
 *        BODY301_NUT_PREC_RA += +0.070
 *        BODY301_NUT_PREC_RA += -0.017
 *        BODY301_NUT_PREC_RA += 0.
 * </pre>
 * 
 * has the same effect as the single assignment:
 * 
 * <pre>
 *        BODY301_NUT_PREC_RA = (  -3.878  -0.120  +0.070  -0.017   0 )
 * </pre>
 * 
 * Dates, e.g.,
 * 
 * <pre>
 *        FOOBAR_CALIBRATION_DATES = ( &#64;31-JAN-1987,
 *                                     &#64;2/4/87,
 *                                     &#64;March-7-1987-3:10:39.221 )
 * </pre>
 * 
 * may be entered in a wide variety of formats. There are two restrictions regarding the format of
 * dates. They may not contain embedded blanks, and they must begin with the character
 * 
 * <pre>
 *        &#64;
 * </pre>
 * 
 * Internally, dates are converted to TDB seconds past J2000 as they are read. As a result, dates,
 * are treated as numeric data in the pool.
 * 
 * Strings may be supplied by quoting the string value.
 * 
 * <pre>
 *        MISSION_UNITS = ( 'KILOMETERS',
 *                          'SECONDS',
 *                          'KILOMETERS/SECOND' )
 * </pre>
 * 
 * If you need to include a quote in the string value, use the FORTRAN convention of "doubling" the
 * quote.
 * 
 * <pre>
 *        MESSAGE = ( 'You can''t always get what you want.' )
 * </pre>
 * 
 * The maximum length of as string that can be assigned as the value of a scalar kernel variable, or
 * as an element of an array-valued kernel variable, is 80 characters.
 * 
 * The types of values assigned to a kernel pool variable must all be the same. If you attempt to
 * make an assignment such as the one shown here:
 * 
 * <pre>
 *        ERROR_EXAMPLE = ( 1, 2, 'THREE', 4, 'FIVE' )
 * </pre>
 * 
 * The kernel pool reader will regard the assignment as erroneous and reject it and any subsequent
 * kernel pool assignments that appear in the text kernel.
 * </p>
 * <p>
 * <b>Parser Deviations from NAIF's Text Kernel Format:</b>
 * <ul>
 * <li>Quoted strings lacking their terminating quote, are handled properly by NAIF's parser. This
 * parser chokes on such kernels. Per a discussion with Boris Semenov at NAIF, this is acceptable,
 * because the specification itself requires the terminating string. If it becomes an issue this
 * parser can be easily modified to deviate from the specification.</li>
 * 
 * <li>Time strings of the format: YYYY-DOY.### (2006-127.5) have their fractional component handled
 * properly. The SPICE LDPOOL parser, however, truncates the fractional component and stores only
 * the YYYY-DOY portion of the string without generating any sort of error.</li>
 * </p>
 */
public class TextKernelParser {

  /**
   * Internal, JavaCC generated package private class designed to handle parsing of the text kernel
   * content. Configure it to parse an empty string for now, as we will reuse each time the parse
   * methods are called.
   */
  private final Parser parser = new Parser(new StringReader(""));

  /**
   * Parse the contents of a text kernel file attached to the supplied Reader into a newly created
   * KernelPool.
   * 
   * @param r the Reader attached to the text kernel to be parsed.
   * 
   * @return a newly created KernelPool that contains the contents of the text kernel attached to
   *         the Reader, r.
   * 
   * @throws ParseException if the text kernel connected to the Reader r does not parse properly.
   */
  public BasicKernelPool parse(Reader reader) throws ParseException {
    parser.ReInit(reader);
    BasicKernelPool pool = parser.parse();
    return pool;
  }

  /**
   * Parse the contents of a text kernel file attached to the supplied Reader into the supplied
   * KernelPool.
   * 
   * @param pool the KernelPool to receive the contents of the text kernel attached to the Reader r.
   * @param r the Reader attached to the text kernel to be parsed.
   * 
   * @return a reference to pool for convenience.
   * 
   * @throws ParseException if the text kernel connected to the Reader r does not parse properly.
   */
  public <T extends BasicKernelPool> T parseInto(T pool, Reader reader) throws ParseException {
    parser.ReInit(reader);
    BasicKernelPool contents = parser.parse();
    pool.load(contents);
    return pool;
  }

}
