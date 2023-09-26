package picante.spice.daf;

import static com.google.common.base.Preconditions.checkArgument;

import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;

/**
 * Provides common DAF related parameters and functions that may be useful in implementing various
 * software interfaces present in this package.
 * <p>
 * The <code>DAF</code> and <code>DAFSegment</code> interfaces abstract the NAIF double array file
 * concept sufficiently well, that these functions and parameters here may be of no use in the most
 * general of implementations not directly connected to a file.
 * </p>
 */
public class DAFSupport {

  /**
   * The byte offset into the DAF where the file record begins.
   */
  public static final int DAF_FILERECORD_LOCATION = 0;

  /**
   * The length, in bytes of the standard 32bit DAF file record.
   */
  public static final int DAF_FILERECORD_LENGTH = 1024;

  /**
   * The byte offset into the DAF where the string ID word indicating the NAIF file type exists.
   */
  public static final int DAF_ID_LOCATION = 0;

  /**
   * The maximum length, in bytes, of the NAIF ID word.
   */
  public static final int DAF_ID_LENGTH = 8;

  /**
   * The byte offset into the DAF where the string indicating the internal file name of the DAF is
   * located.
   */
  public static final int DAF_NAME_LOCATION = 16;

  /**
   * The maximum length, in bytes, of the DAF internal file name.
   */
  public static final int DAF_NAME_LENGTH = 60;

  /**
   * The byte offset into the DAF where the string indicating the binary file format of the DAF is
   * located.
   */
  public static final int DAF_BFF_LOCATION = 88;

  /**
   * The length, in bytes, of the binary file format ID string in the DAF.
   */
  public static final int DAF_BFF_LENGTH = 8;

  /**
   * The string indicating the binary file format of the DAF is big endian, IEEE (Sun) format.
   */
  public static final String DAF_BIG_ENDIAN = "BIG-IEEE";

  /**
   * The string indicating the binary file format of the DAF is little endian, IEEE (PC) format.
   */
  public static final String DAF_LITTLE_ENDIAN = "LTL-IEEE";

  /**
   * The string indicating the binary file format of the DAF is a little endian, VAX GFLOAT format.
   * Note: this format is unsupported by this software.
   */
  public static final String DAF_VAX_GFLOAT = "VAX-GFLT";

  /**
   * The string indicating the binary file format of the DAF is a little endian, VAX DFLOAT format.
   * Note: this format is unsupported by this software.
   */
  public static final String DAF_VAX_DFLOAT = "VAX-DFLT";

  /**
   * The byte offset to the location of the integer containing the number of double precision
   * components in the segment metadata for this DAF.
   */
  public static final int DAF_ND_LOCATION = 8;

  /**
   * The byte offset to the location of the integer containing the number of integer components in
   * the segment metadata for this DAF.
   */
  public static final int DAF_NI_LOCATION = 12;

  /**
   * The byte offset to the location of the integer containing the first &quot;record&quot; of
   * descriptor, metadata, content in this DAF. This record counter is integral, in blocks of 1024
   * bytes, starting at 1 for the first record in the file.
   */
  public static final int DAF_FWARD_LOCATION = 76;

  public static final int DAF_BWARD_LOCATION = 80;

  public static final int DAF_FFA_LOCATION = 84;

  /**
   * The byte offset to the start of where the NAIF toolkit attempts to locate the FTP validation
   * string in the DAF.
   */
  public static final int DAF_FTP_STRING_SEARCH_LOCATION = 499;

  /**
   * The length of the byte block that the NAIF toolkit examines to locate the FTP validation string
   * in the DAF.
   */
  public static final int DAF_FTP_STRING_SEARCH_LENGTH = 500;

  /**
   * The byte offset to the first byte of the FTP validation string that the NAIF toolkit places
   * into the file record of their DAFs.
   */
  public static final int DAF_FTP_STRING_LOCATION = 699;

  /**
   * The string that indicates the start of the FTP validation sequence.
   */
  public static final String DAF_FTP_START = "FTPSTR";

  /**
   * The string that indicates the end of the FTP validation sequence.
   */
  public static final String DAF_FTP_END = "ENDFTP";

  /**
   * The bytes used to validate the DAFs FTP status. This string matches that which is currently
   * utilized in the N0060 version of the toolkit.
   * <p>
   * FTPSTR:<13>:<10>:<13><10>:<13><0>:<129>:<16><206>:ENDFTP
   * </p>
   * The FORTRAN compilers they support treat bytes as unsigned, thus it was required to wrap <129>
   * and <206> to their signed equivalents by subtracting 256. The array is immediately converted to
   * a String, so we have immutable content that can be accessed externally if needed.
   */
  public static final String DAF_FTP_CHECKSTRING = createStringFromBytes(
      new byte[] {58, 13, 58, 10, 58, 13, 10, 58, 13, 0, 58, -127, 58, 16, -50, 58});

  private static final String FTP_FAILURE_MESSAGE =
      "Bytes supplied from the DAF fail the FTP validation check.  "
          + "Has this file been transferred in ASCII mode with FTP?";

  /**
   * Localizes the mess associated with converting ASCII based bytes into a Java String.
   * 
   * @param bytes bytes containing ASCII encoded, single byte per character string
   * 
   * @return a <code>String</code> containing the contents of bytes.
   */
  public static String createStringFromBytes(byte[] bytes) {
    return new String(bytes, StandardCharsets.ISO_8859_1);
  }

  public static String trailingTrim(String s) {

    /*
     * Handle the trivial cases first. Null string returns null.
     */
    if (s == null) {
      return s;
    }

    /*
     * Empty string next.
     */
    if (s.length() == 0) {
      return s;
    }

    int index = s.length() - 1;
    while (s.charAt(index) == ' ') {
      index--;
      /*
       * If the string is all spaces, just return the empty string.
       */
      if (index == -1) {
        return "";
      }
    }

    return s.substring(0, index + 1);

  }

  /**
   * Creates a properly space padded string of the required length.
   * 
   * @param s the string to pad the ending with spaces to reach a desired length
   * @param requiredLength the desired length
   * 
   * @return newly created string of length requiredLength
   * 
   * @throws IllegalArgumentException if the input string s has length greater than requiredLength,
   *         or if requiredLength is not strictly positive
   */
  public static String padStringWithSpaces(String s, int requiredLength) {
    checkArgument(requiredLength > 0, "Required length must be strictly positive.");
    checkArgument(s.length() <= requiredLength, "Supplied string exceeds required length.");
    StringBuilder builder = new StringBuilder(s);
    while (builder.length() < requiredLength) {
      builder.append(" ");
    }
    return builder.toString();
  }



  /**
   * Verify that if present, the FTP validation check string is as expected. This will help minimize
   * any errors that may occur due to improper file transfers.
   * 
   * @param checkBytes a byte block possible containing an FTP validation sequence captured from a
   *        DAF
   * 
   * @throws DAFValidationFailure if the FTP check string is present and corrupted in some
   *         identifiable way
   */
  public static void checkFTPString(byte[] checkBytes) {

    String string = createStringFromBytes(checkBytes);

    int ftpStartIndex = string.indexOf(DAF_FTP_START);

    if (ftpStartIndex == -1) {
      return;
    }

    int ftpEndIndex = string.indexOf(DAF_FTP_END, ftpStartIndex + DAF_FTP_START.length());

    /*
     * If we can't locate the ending marker, then clearly something is wrong with the validation.
     */
    if (ftpEndIndex == -1) {
      throw new DAFValidationFailure(FTP_FAILURE_MESSAGE);
    }

    /*
     * FTPSTR:<13>:<10>:<13><10>:<13><0>:<129>:<16><206>:ENDFTP
     */
    String testString = string.substring(ftpStartIndex + DAF_FTP_START.length(), ftpEndIndex);

    if (!testString.contains(DAF_FTP_CHECKSTRING) && !DAF_FTP_CHECKSTRING.contains(testString)) {
      throw new DAFValidationFailure(FTP_FAILURE_MESSAGE);
    }
  }

  /**
   * Determine the byte ordering of a DAF by examining the contents of its file record.
   * <p>
   * This method inspects the file record first for the NAIF binary file format identification
   * string. DAF files created with N0050 toolkits or later contain this identification string. If
   * the string is not recognized, then it proceeds to use a deterministic algorithm, so long as new
   * binary file formats (or old, unsupported like VMS) ones are examined.
   * </p>
   * <p>
   * Note: it is possible, if one presents an older VMS built binary DAF, that this algorithm will
   * incorrectly recognize it as an IEEE little endian file. As these files are rather uncommon
   * today, this was determined to be an acceptable risk. If a file is incorrectly identified, other
   * components in the DAF system will likely fail spectacularly as records in the linked list of
   * meta data will not be located properly.
   * </p>
   * 
   * @param fileRecord the first 1024 bytes of a DAF, commonly referred to in NAIF documentation as
   *        the file record
   * 
   * @return the byte order in use within the DAF
   * 
   * @throws DAFValidationFailure if the binary file format specified by the DAF is indeterminate or
   *         otherwise unknown
   */
  public static ByteOrder determineByteOrder(byte[] fileRecord) {

    /*
     * Attempt to extract NAIF's ID word from the file record. This is definitive of the software's
     * intent. There are only two values that are appropriate at the time of this writing: BIG-IEEE
     * and LTL-IEEE.
     */
    byte[] bffidBytes = new byte[DAF_BFF_LENGTH];
    for (int i = 0; i < DAF_BFF_LENGTH; i++) {
      bffidBytes[i] = fileRecord[i + DAF_BFF_LOCATION];
    }
    String bffid = createStringFromBytes(bffidBytes);

    if (bffid.equals(DAF_BIG_ENDIAN)) {
      return ByteOrder.BIG_ENDIAN;
    }
    if (bffid.equals(DAF_LITTLE_ENDIAN)) {
      return ByteOrder.LITTLE_ENDIAN;
    }
    if (bffid.equals(DAF_VAX_DFLOAT)) {
      throw new DAFValidationFailure("VAX D-floating file format is unsupported.");
    }
    if (bffid.equals(DAF_VAX_GFLOAT)) {
      throw new DAFValidationFailure("VAX G-floating file format is unsupported.");
    }

    /*
     * As we only support two of NAIF's supported DAF binary file formats, we merely need to check
     * the value of NI. There are only two possible situations, since NI is necessarily bounded
     * between 2 and 250.
     */
    byte[] niContent = new byte[4];
    for (int i = 0; i < 4; i++) {
      niContent[i] = fileRecord[DAF_NI_LOCATION + i];
    }

    if ((niContent[0] != 0) && (niContent[1] == 0) && (niContent[2] == 0) && (niContent[3] == 0)) {
      return ByteOrder.LITTLE_ENDIAN;
    }
    if ((niContent[0] == 0) && (niContent[1] == 0) && (niContent[2] == 0) && (niContent[3] != 0)) {
      return ByteOrder.BIG_ENDIAN;
    }

    /*
     * If we reach here something has gone horribly wrong, as in either the DAF is broken or is from
     * some unsupported architecture.
     */
    throw new DAFValidationFailure(
        "Unable to load DAF from byte buffer, the" + " binary file format appears to be invalid.");

  }

}
