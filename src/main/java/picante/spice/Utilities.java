package picante.spice;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import com.google.common.base.Charsets;
import com.google.common.collect.ImmutableList;
import com.google.common.io.CharSink;
import com.google.common.io.Files;
import picante.spice.kernel.KernelInstantiationException;
import picante.spice.kernel.tk.lsk.LSK;
import picante.spice.kernelpool.content.LSKFactory;
import picante.time.LeapsecondEntry;

/**
 * This class exists to capture static utility methods that would be useful throughout the SPICE
 * adapter mechanism.
 * <p>
 * Normally this isn't the preferred way to capture these sorts of things, however; as NAIF has
 * rigid coding standards and backwards compatibility, it seems unlikely we would ever need to
 * override these methods once implemented. This really should be used as the determining
 * characteristic for including methods into this class.
 * </p>
 */
public class Utilities {

  /**
   * Converts the supplied string into it's canonical form of an ID code.
   * <p>
   * The SPICE code that does this canonicalization is:
   * 
   * <pre>
   * CALL LJUST  ( NAME, TMPNAM ) 
   * CALL UCASE  ( TMPNAM, TMPNAM ) 
   * CALL CMPRSS ( ' ', 1, TMPNAM, TMPNAM )
   * </pre>
   * 
   * Translating this to English:
   * <ul>
   * <li>Remove any leading white space</li>
   * <li>Convert all characters to upper case</li>
   * <li>Compress out any consecutive occurrences of whitespace to a single space</li>
   * </ul>
   * 
   * While SPICE treats TABS as special characters, I think we should probably just throw an error
   * if someone inadvertently specifies tabs in the setup text kernel. This routine just performs
   * the conversion to canonical form, so it should be up to the caller of this method to check for
   * the presence of TABS.
   * </p>
   * <p>
   * This canonicalization works both for ephemeris and frame names in SPICE. Frame names are not
   * permitted to have spaces, so the replaceAll() should do nothing to them.
   * </p>
   * 
   * @param name the ephemeris name to canonicalize
   * 
   * @return a leading, trailing space removed, uppercased, whitespace compressed string
   */
  public static String canonicalizeSpiceName(String name) {
    return name.trim().toUpperCase().replaceAll(" +", " ");
  }

  /**
   * Rounds a double precision value to its closest integer, using the same logic as the FORTRAN
   * intrinsic ANINT.
   * 
   * @param value the value to round according the the ANINT specification
   * 
   * @return the &quot;closest&quot; integral valued double to the supplied value. The 0.5 issue is
   *         resolved per the behavior of ANINT
   */
  public static double round(double value) {
    return (value >= 0) ? Math.floor(value + 0.5) : -(Math.floor(Math.abs(value) + 0.5));
  }

  public static void createInternalPropertiesFile(File leapsecondsKernel, File outputFile)
      throws KernelInstantiationException, FileNotFoundException, IOException {

    SpiceEnvironmentBuilder builder = new SpiceEnvironmentBuilder();
    builder.load("lsk", leapsecondsKernel);
    SpiceEnvironment env = builder.build();
    LSKFactory factory = new LSKFactory();
    LSK lsk = factory.createLSK(env.getPool());

    ImmutableList.Builder<String> contentBuilder = ImmutableList.builder();
    contentBuilder.add("picante.time.source = " + leapsecondsKernel.getName());
    contentBuilder.add("picante.time.gps_tai_dt = 19.0");
    contentBuilder.add("picante.time.deltet.delta_t_a = " + lsk.getDeltaTa());
    contentBuilder.add("picante.time.deltet.eb = " + lsk.getEB());
    contentBuilder.add("picante.time.deltet.k = " + lsk.getK());
    double[] m = new double[2];
    lsk.getM(m);
    contentBuilder.add("picante.time.deltet.m = " + m[0] + ", " + m[1]);

    StringBuilder lskLineBuilder = new StringBuilder();
    lskLineBuilder.append("picante.time.deltet.delta_at = ");

    for (LeapsecondEntry entry : lsk.getDeltaAT()) {
      lskLineBuilder.append(entry.getDut());
      lskLineBuilder.append(", ");
      lskLineBuilder.append(entry.getFormalEpoch());
      lskLineBuilder.append(", ");
    }

    /*
     * Remove the trailing ","
     */
    lskLineBuilder.delete(lskLineBuilder.length() - 2, lskLineBuilder.length());
    contentBuilder.add(lskLineBuilder.toString());

    CharSink sink = Files.asCharSink(outputFile, Charsets.ISO_8859_1);
    sink.writeLines(contentBuilder.build());

  }
}
