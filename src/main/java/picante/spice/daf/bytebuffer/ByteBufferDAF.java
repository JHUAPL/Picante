package picante.spice.daf.bytebuffer;

import static picante.spice.daf.DAFSupport.DAF_FILERECORD_LENGTH;
import static picante.spice.daf.DAFSupport.DAF_FTP_STRING_SEARCH_LENGTH;
import static picante.spice.daf.DAFSupport.DAF_FTP_STRING_SEARCH_LOCATION;
import static picante.spice.daf.DAFSupport.DAF_FWARD_LOCATION;
import static picante.spice.daf.DAFSupport.DAF_ID_LENGTH;
import static picante.spice.daf.DAFSupport.DAF_ID_LOCATION;
import static picante.spice.daf.DAFSupport.DAF_NAME_LENGTH;
import static picante.spice.daf.DAFSupport.DAF_NAME_LOCATION;
import static picante.spice.daf.DAFSupport.DAF_ND_LOCATION;
import static picante.spice.daf.DAFSupport.DAF_NI_LOCATION;
import static picante.spice.daf.DAFSupport.checkFTPString;
import static picante.spice.daf.DAFSupport.createStringFromBytes;
import static picante.spice.daf.DAFSupport.determineByteOrder;
import static picante.spice.daf.DAFSupport.trailingTrim;
import java.nio.ByteBuffer;
import java.nio.DoubleBuffer;
import java.nio.IntBuffer;
import java.util.ArrayList;
import java.util.List;
import picante.spice.daf.DAF;
import picante.spice.daf.DAFSegment;
import picante.spice.daf.DAFValidationFailure;

/**
 * Implementation of the DAF interface that accesses its content through a <code>ByteBuffer</code>
 * <p>
 * This implementation is designed to work with two basic input situations:
 * <ul>
 * <li>InputStream readers connected to DAFs</li>
 * <li>Memory mapped local files connected to DAFs</li>
 * </ul>
 * <p>
 * Note: The reference, NAIF, implementation of DAF reports that the number of integer components in
 * the metadata must be at least 2. This accounts for the integer addresses of the segment content
 * stored in the metadata. This implementation hides these addresses from the user, and as such the
 * number of integer metadata elements is precisely 2 less than that of the actual DAF.
 * </p>
 * <p>
 * Exception handling within this implementation is a bit strange. As the class wraps itself around
 * the supplied ByteBuffer, the only exceptions that should be generated are exceptions relating to
 * system failures. As such, only index checking is performed by the implementation.
 * </p>
 */
public class ByteBufferDAF implements DAF {

  /**
   * A read-only byte buffer containing the data associated with this DAF.
   */
  private final ByteBuffer buffer;

  /**
   * The NAIF identification string for this DAF.
   */
  private final String dafId;

  /**
   * The internal filename stored in the DAF file record.
   */
  private final String dafName;

  /**
   * A list of individual DAF segments, in order as they appear in the file. Head of the list is the
   * first segment in the file.
   */
  private final List<ByteBufferDAFSegment> segments;

  /**
   * The number of double precision components stored in the metadata for segments in the DAF.
   */
  private final int nd;

  /**
   * The actual number of integer components stored in the metadata for segments in the DAF. This is
   * necessarily two larger than that reported by the actual segments themselves. The reason for
   * this is simply that the metadata contains the start and stop addresses of the segment itself,
   * which are transparently absorbed into the class implementation for the convenience of the user.
   */
  private final int ni;

  /**
   * Marks the byte index of the final byte in the reserved record block.
   */
  private final int lastReserved;

  /**
   * Read a space terminated string of a specific length from a particular location in a supplied
   * buffer. Note: the buffer position and limit are left intact by this method.
   * 
   * @param buffer a byte buffer from which the fixed length string is to be read
   * @param location the byte offset from position zero in the buffer where the first byte of the
   *        string of interest lies
   * @param length the fixed length of the string
   * @return a white-space trimmed <code>String</code> containing the values from the requested
   *         bytes in the buffer. Note only the white space trailing the string is trimmed, per the
   *         usual way FORTRAN handles strings.
   */
  private static String readString(ByteBuffer buffer, int location, int length) {
    /*
     * Capture the current state of the supplied buffer.
     */
    int limit = buffer.limit();
    int position = buffer.position();

    /*
     * Read the bytes containing the string of interest.
     */
    buffer.limit(buffer.capacity());
    buffer.position(location);
    byte[] content = new byte[length];
    buffer.get(content);

    /*
     * Restore the previous state of the buffer.
     */
    buffer.limit(limit);
    buffer.position(position);

    return trailingTrim(createStringFromBytes(content));
  }

  /**
   * Creates a DAF from the supplied <code>ByteBuffer</code>.
   * 
   * @param buffer a buffer, of which a read-only view is captured internally by this class
   * 
   * @throws DAFValidationFailure if the contents of the DAF are not considered valid
   */
  public ByteBufferDAF(ByteBuffer buffer) {

    /*
     * Ensure we are only ever going to read from the supplied buffer.
     */
    this.buffer = buffer.asReadOnlyBuffer();

    dafId = readString(buffer, DAF_ID_LOCATION, DAF_ID_LENGTH);
    dafName = readString(buffer, DAF_NAME_LOCATION, DAF_NAME_LENGTH);

    /*
     * Check that the DAF file was not corrupted via any sort of internet or file system transfer.
     */
    /*
     * Read bytes 500 to 1000 into a string. We can't use the readString method because we want to
     * retain the bytes including any whitespace.
     */
    this.buffer.position(DAF_FTP_STRING_SEARCH_LOCATION);
    byte[] contents = new byte[DAF_FTP_STRING_SEARCH_LENGTH];
    this.buffer.get(contents);
    checkFTPString(contents);

    byte[] record = new byte[DAF_FILERECORD_LENGTH];
    this.buffer.position(0);
    this.buffer.get(record);
    this.buffer.order(determineByteOrder(record));

    nd = this.buffer.getInt(DAF_ND_LOCATION);
    ni = this.buffer.getInt(DAF_NI_LOCATION);

    segments = new ArrayList<ByteBufferDAFSegment>();
    populateSegmentList();

    int fward = this.buffer.getInt(DAF_FWARD_LOCATION);
    lastReserved = 1024 * (fward - 1) - 1;

    /*
     * Reset the byte buffer.
     */
    this.buffer.clear();
  }

  /**
   * Populate the internal list of DAF segments, creating each of the segments from the byte
   * buffers.
   */
  private void populateSegmentList() {

    /*
     * Locate the first descriptor record in the DAF. If the it indicates that there is no such
     * record, then this is clearly an error.
     */
    int fward = buffer.getInt(DAF_FWARD_LOCATION);

    if (fward == 0) {
      throw new DAFValidationFailure("Empty DAF. In order to be read out, "
          + "a DAF must have at least one descriptor record.");
    }

    while (fward != 0) {

      /*
       * Configure the buffer views into the descriptor record for convenient reading.
       */
      buffer.limit(buffer.capacity());
      buffer.position((fward - 1) * 1024);
      buffer.limit(buffer.position() + 1024);

      DoubleBuffer doubleView = buffer.asDoubleBuffer();
      IntBuffer intView = buffer.asIntBuffer();

      /*
       * Read the number of segments stored in this particular descriptor record. It is the third
       * double in the buffer.
       */
      int nseg = (int) doubleView.get(2);

      /*
       * If the number of segments present in the currently considered record is 0, then return.
       * Normal SPK files would have the character record that follows this descriptor record,
       * however; SPICE seems somewhat ambivalent to its absence if there are no segments present in
       * the last descriptor.
       */
      if (nseg == 0) {
        return;
      }

      /*
       * Configure the view of the character record that follows the descriptor record.
       */
      buffer.limit(buffer.capacity());
      buffer.position((fward) * 1024);
      buffer.limit(buffer.position() + 1024);
      ByteBuffer charView = buffer.slice();

      int nameLen = 8 * (nd + (ni + 1) / 2);
      for (int i = 0; i < nseg; i++) {

        /*
         * Create new arrays that are to be supplied to the segment constructor. This is necessary
         * as the constructor simply captures references to these arrays internally.
         */
        double[] dc = new double[nd];
        int[] ic = new int[ni - 2];

        /*
         * Read the metadata components into the buffers.
         */
        doubleView.position(3 + nd * i + (ni + 1) / 2 * i);
        doubleView.get(dc);
        intView.position(2 * (3 + nd * (i + 1) + (ni + 1) / 2 * i));
        intView.get(ic);
        int start = intView.get();
        int end = intView.get();

        /*
         * Read the name record associated with this segment.
         */
        String name = readString(charView, nameLen * i, nameLen);

        /*
         * Configure the buffer to create the necessary slice. Note, for some reason the slice
         * method resets the byte order back to big endian. Inject the requested ordering into it.
         */
        buffer.limit(end * 8);
        buffer.position((start - 1) * 8);
        segments.add(new ByteBufferDAFSegment(name, dc, ic, buffer.slice().order(buffer.order())));

      }

      /*
       * We have successfully processed the records associated with the current descriptor record.
       * Obtain the location of the next.
       */
      fward = (int) doubleView.get(0);

    }

  }

  @Override
  public String getID() {
    return dafId;
  }

  @Override
  public String getName() {
    return dafName;
  }

  @Override
  public byte[] getReservedRecords() {

    /*
     * Check to see if there are any reserved records in this DAF.
     */
    if (1024 > lastReserved) {
      return new byte[0];
    }

    /*
     * Create an array with the appropriate number of bytes. The lastReserved field points to the
     * byte offset of the last byte in the reserved records. All reserved records in the DAF start
     * at byte position 1024.
     */
    byte[] reserved = new byte[lastReserved - 1023];

    buffer.limit(buffer.capacity());
    buffer.position(1024);
    buffer.get(reserved);

    return reserved;
  }

  @Override
  public DAFSegment getSegment(int index) {
    return segments.get(index);
  }

  @Override
  public int getSize() {
    return segments.size();
  }
}
