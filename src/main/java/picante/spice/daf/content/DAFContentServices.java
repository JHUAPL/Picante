package picante.spice.daf.content;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.common.base.CharMatcher;
import picante.exceptions.BugException;
import picante.spice.daf.DAF;
import picante.spice.daf.DAFAccessException;
import picante.spice.daf.DAFSegment;
import picante.spice.kernel.KernelType;

public class DAFContentServices {

  private static final String COMMENT_EOL = "\u0000";

  private static final String COMMENT_END = "\u0004";


  /**
   * Extracts the comments from the reserved records of a DAF. The extraction follows the rules set
   * forth in the SPC API in SPICE. Note: this applies only to SPK, CK, and binary PCK files.
   * 
   * @param daf the DAF from which to extract comments
   * @param buffer a list of <code>String</code>s to have the comment records from the DAF appended
   *        to it.
   * 
   * @return a reference to buffer for convenience
   */
  public static List<String> extractComments(DAF daf, List<String> buffer) {

    byte[] raw = daf.getReservedRecords();

    /*
     * Check to see if there are no reserved records in the DAF.
     */
    if (raw.length == 0) {
      return buffer;
    }

    String bytes;

    bytes = new String(raw, StandardCharsets.ISO_8859_1);

    /*
     * Chop off everything from the end of comment marker to the end of the buffer.
     */
    bytes = bytes.replaceAll(COMMENT_END + ".*", "");

    /*
     * Split the left on the end of line character.
     */
    for (String s : bytes.split(COMMENT_EOL)) {
      buffer.add(s);
    }

    return buffer;

  }

  public static byte[] createComments(List<String> buffer) {

    if (buffer.size() == 0) {
      return new byte[0];
    }

    try {

      /*
       * Determine if any non-printing or unicode characters outside the ASCII range 32-126 are
       * present. SPICE forbids these from being included in comments.
       */
      CharMatcher matcher = CharMatcher.inRange(' ', '~');

      ByteArrayOutputStream output = new ByteArrayOutputStream(0);

      boolean commentsWritten = false;

      for (String s : buffer) {
        if (matcher.matchesAllOf(s)) {
          if (s.length() > 0) {
            commentsWritten = true;
            output.write(s.getBytes(StandardCharsets.ISO_8859_1));
            output.write(COMMENT_EOL.getBytes(StandardCharsets.ISO_8859_1));
          }
        } else {
          throw new IllegalArgumentException(
              "Input string buffer has non-ASCII or ASCII non-printing characters.");
        }
      }

      if (commentsWritten) {
        output.write(COMMENT_END.getBytes(StandardCharsets.ISO_8859_1));
      }

      return output.toByteArray();

    } catch (IOException e) {
      throw new BugException("Writing to byte array output stream failed", e);
    }



  }

  private static final int SPK_CENTER_INDEX = 1;

  private static final int CK_ANGULAR_VELOCITY_INDEX = 3;

  private static final int CK_TYPE_INDEX = 2;

  private static final Map<String, KernelType> DAF_TYPE_MAP = new HashMap<String, KernelType>();

  static {
    DAF_TYPE_MAP.put("DAF/PCK", KernelType.PCK);
    DAF_TYPE_MAP.put("DAF/CK", KernelType.CK);
    DAF_TYPE_MAP.put("DAF/SPK", KernelType.SPK);
  }

  /**
   * Determines the kernel type of the supplied DAF.
   * <p>
   * <b>Note:</b>See the N0060 routine ZZCKSPK for details on the implementation of this method.
   * </p>
   * 
   * @param daf the DAF to determine the type of.
   * 
   * @return an instance of KernelType {CK,SPK,PCK} or null if the contents of the file is not
   *         identifiable.
   */
  public static KernelType identifyDAF(DAF daf) {

    /*
     * Start by examining the ID word. It may tell us precisely what it is. This is certainly the
     * case for PCKs.
     */
    String idWord = daf.getID();

    if (!idWord.equals("NAIF/DAF")) {
      return DAF_TYPE_MAP.containsKey(idWord) ? DAF_TYPE_MAP.get(idWord) : null;
    }

    /*
     * The old ID word indicates that we are either handling a CK or an SPK. Determine which by
     * executing Bill Taber's decision algorithm. First, check to see if there are any segments
     * within the DAF. If not, then we can't decide on its type.
     */
    if (daf.getSize() == 0) {
      return null;
    }

    /*
     * Examine the contents of the first segment in the file.
     */
    DAFSegment segment = daf.getSegment(0);

    int angularVelocityFlag = segment.getIntComponent(CK_ANGULAR_VELOCITY_INDEX);

    /*
     * The angular velocity flag in a CK may only be 0 or 1, and since this is coincident with the
     * SPK data type flag, values greater than 1 immediately imply an SPK.
     */
    if (angularVelocityFlag > 1) {
      return KernelType.SPK;
    }

    /*
     * Since type 0 is an invalid SPK type, if the angular velocity flag is 0, then it must be a CK.
     */
    if (angularVelocityFlag == 0) {
      return KernelType.CK;
    }

    /*
     * At this point we have the angular velocity flag set to 1, so it is either a type 1 SPK
     * segment, or a CK segment with angular velocity. Next see if the center is the solar system
     * barycenter. This is valid, because there is no ID code 0 supported in the CK system.
     */
    if (segment.getIntComponent(SPK_CENTER_INDEX) == 0) {
      return KernelType.SPK;
    }

    /*
     * Now we have a type 1 SPK segment that is not orbiting the solar system barycenter, or a CK
     * segment with angular velocity. Next, check to see if the size is consistent with a type 1 SPK
     * segment.
     */
    int nspkrec = computeNumberOfRecords(segment.getLength(), 72, 100, 0);

    if (nspkrec == -1) {
      return KernelType.CK;
    }

    /*
     * See if the last entry in the segment is the allowed number of MDA records. If it is not, then
     * we obviously have a CK.
     */
    double[] length = new double[1];
    segment.get(segment.getLength() - 1, length, 0, 1);

    if (length[0] != nspkrec) {
      return KernelType.CK;
    }

    /*
     * If the last entry in the segment matches the expected number of MDA records, then we only
     * have a problem if it is a type 2 CK.
     */
    if (segment.getIntComponent(CK_TYPE_INDEX) != 2) {
      return KernelType.SPK;
    }

    /*
     * Resolve whether we are dealing with a type 2 CK segment or a type 1 SPK segment. First verify
     * that the length of the segment contents could be supported by a type 2 kernel.
     */
    int nckrec = computeNumberOfRecords(segment.getLength(), 10, 100, 1);

    if (nckrec == 1) {
      return KernelType.SPK;
    }

    /*
     * At this point we have either a type 2 CK segment or a type 1 SPK segment. The ambiguity is
     * resolved by some seriously convoluted examinations of the segment content.
     */
    if (nckrec < 201) {

      double[] firstTime = new double[1];
      segment.get(segment.getLength() - nspkrec, firstTime, 0, 1);

      double[] checkTime = new double[1];
      segment.get((nspkrec - 1) * 71, checkTime, 0, 1);

      if (checkTime[0] > firstTime[0]) {
        return KernelType.SPK;
      }

      return KernelType.CK;
    }

    /*
     * In the remaining case there are at least 2 directory records if we have a CK. Read the last
     * potential tick value and the first potential directory value.
     */
    double[] times = new double[2];
    segment.get(segment.getLength() - 1 - (nckrec - 1) / 100, times, 0, 2);

    if (times[0] > times[1]) {
      return KernelType.CK;
    }

    return KernelType.SPK;

  }

  /**
   * Compute the unique number of possible records for a SPK or CK segment given a recorded segment
   * length, a packet size, and directory attributes.
   * 
   * @param segmentLength the length of the DAF segment
   * @param packetSize the number of elements in a packet
   * @param directorySize the size of the directory (every directorySize elements are captured)
   * @param directoryOffset an offset to apply to the directory
   * 
   * @return the number of records appropriate for the segment or -1 if no such number of records
   *         exist
   */
  private static int computeNumberOfRecords(int segmentLength, int packetSize, int directorySize,
      int directoryOffset) {

    int tmp = packetSize * directorySize;

    int q = segmentLength / tmp;
    int a = segmentLength - q * tmp;

    if (a < 0) {
      q--;
      a += tmp;
    }

    if (directoryOffset * packetSize > a) {
      return -1;
    }

    if (a == (a / packetSize) * packetSize) {
      return directorySize * q + a / packetSize;
    }

    return -1;
  }

  /**
   * Searches through an ordered, non-decreasing subarray in a DAF for the offset to the value that
   * is strictly last less than the specified value.
   * 
   * @param segment the segment whose subarray is to be examined
   * @param start the starting index of the subarray
   * @param finish the final index of the subarray
   * @param value the value over which to search
   * 
   * @return -1 if all of the elements in the subarray specified by segment, start, and finish are
   *         ahead of the supplied value, the offset to a value in [start,finish] that is last less
   *         than the supplied value otherwise.
   * 
   * @throws DAFAccessException
   * 
   * @throws IndexOutOfBoundsException
   */
  public static int lastLessThanSearch(DAFSegment segment, int start, int finish, double value) {

    double[] queryValue = new double[1];

    int items = finish - start + 1;
    int begin = start;
    int end = finish;
    int middle;
    int j;

    /*
     * Handle the pathological case where there aren't any items in the list.
     */
    if (items <= 0) {
      throw new DAFAccessException(
          "Invalid subarray for search specified, indices: [" + start + "," + finish + "].");
    }

    /*
     * Next handle the case where none of the elements in the array are less than the search value.
     */
    segment.get(start, queryValue, 0, 1);

    if (value <= queryValue[0]) {
      return -1;
    }

    /*
     * And the case where all of the elements in the array are less than the search value.
     */
    segment.get(finish, queryValue, 0, 1);

    if (queryValue[0] < value) {
      return finish - start;
    }

    /*
     * The boundary cases have been handled, initiate the search over the segment contents.
     */
    while (items > 2) {

      j = items / 2;
      middle = begin + j;

      segment.get(middle, queryValue, 0, 1);
      if (queryValue[0] < value) {
        begin = middle;
      } else {
        end = middle;
      }

      items = 1 + (end - begin);

    }

    return begin - start;
  }

  public static int lastLessThanOrEqualSearch(DAFSegment segment, int start, int finish,
      double value) {

    double[] queryValue = new double[1];

    int items = finish - start + 1;
    int begin = start;
    int end = finish;
    int middle;
    int j;

    /*
     * Handle the pathological case where there aren't any items in the list.
     */
    if (items <= 0) {
      throw new DAFAccessException(
          "Invalid subarray for search specified, indices: [" + start + "," + finish + "].");
    }

    /*
     * Next handle the case where none of the elements in the array are less than the search value.
     */
    segment.get(start, queryValue, 0, 1);

    if (value <= queryValue[0]) {
      return -1;
    }

    /*
     * And the case where all of the elements in the array are less than or equal to the search
     * value.
     */
    segment.get(finish, queryValue, 0, 1);

    if (value >= queryValue[0]) {
      return finish - start;
    }

    /*
     * The boundary cases have been handled, initiate the search over the segment contents.
     */
    while (items > 2) {

      j = items / 2;
      middle = begin + j;

      segment.get(middle, queryValue, 0, 1);

      if (queryValue[0] <= value) {
        begin = middle;
      } else {
        end = middle;
      }

      items = 1 + (end - begin);
    }

    return begin - start;


  }


}
