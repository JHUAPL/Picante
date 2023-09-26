package picante.spice.kernelpool.content;

import java.io.StringReader;
import picante.spice.kernelpool.BasicKernelPool;
import picante.spice.kernelpool.parser.ParseException;
import picante.spice.kernelpool.parser.TextKernelParser;

/**
 * Class containing static methods useful in assembling test text kernels from within code, rather
 * than text files.
 */
class KernelPoolBuilder {

  private static final TextKernelParser PARSER = new TextKernelParser();

  /**
   * Creates a text kernel assuming data content only is supplied from the array of strings.
   * <p>
   * Note: the appropriate begindata marker is applied automatically by this method, so it <b>should
   * not</b> be included in the content buffer.
   * </p>
   * 
   * @param content an array of strings, each string constitutes a line.
   * 
   * @return a newly created BasicKernelPool containing the parsed contents of the supplied buffer.
   * 
   * @throws ParseException if the parse operation fails for any reason
   */
  static BasicKernelPool createPool(String[] content) throws ParseException {

    /*
     * Concatenate the content array into one giant string.
     */
    StringBuilder builder = new StringBuilder();
    builder.append("\\begindata \n");
    for (String s : content) {
      builder.append(s);
      builder.append("\n");
    }
    return PARSER.parse(new StringReader(builder.toString()));
  }

}
