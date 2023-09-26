package picante.spice.kernelpool.content;

import java.util.Formatter;

/**
 * Simple class that provides access to a formatter that resets the string builder each time format
 * is invoked. This gives capability similar to {@link String#format(String, Object...)} but does
 * not continually allocate memory.
 */
class KeywordFormatter {

  private final StringBuilder builder = new StringBuilder();
  private final Formatter formatter = new Formatter(builder);

  public String format(String format, Object... args) {
    String result = formatter.format(format, args).toString();
    builder.setLength(0);
    return result;
  }

}
