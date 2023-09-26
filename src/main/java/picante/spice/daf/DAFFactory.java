package picante.spice.daf;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.ref.Reference;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileChannel.MapMode;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import com.google.common.base.FinalizablePhantomReference;
import com.google.common.base.FinalizableReferenceQueue;
import picante.spice.daf.bytebuffer.ByteBufferDAF;

/**
 * Simple factory class providing mechanisms for creating DAFs from a variety of common inputs.
 */
public class DAFFactory {

  // TODO: Determine if this usage of the FinalizableReferenceQueue is
  // necessary. It forces resources to be closed when the underlying
  // DAF is garbaged collected, but this will happen automatically with
  // the implementation of finalize() on the channel API.
  /**
   * Thread-safe set of references created to service open commands. These must be held, otherwise
   * the references will be reclaimed by the garbage collector before they can be utilized.
   */
  private static final Set<Reference<?>> REFERENCES =
      Collections.synchronizedSet(new HashSet<Reference<?>>());

  /**
   * The reference queue the garbage collector posts references as they are reclaimed. Using this
   * may have implications if this code is used in a web servlet container, as it could cause the
   * class loader to stick around.
   */
  private static final FinalizableReferenceQueue QUEUE = new FinalizableReferenceQueue();

  /**
   * Buffer length used to control the ingestion of a general input stream.
   */
  public static final int BUFFER_LENGTH = 10240;

  /**
   * Create a DAF from the contents of the supplied <code>InputStream</code>
   * <p>
   * Note: If you are attempting to read from a file on the disk, in general it is better that you
   * access the file directly with one of the other methods supplied in this factory. This method
   * reads the <b>entire</b> file contents into a giant byte array and wraps itself around the
   * result. For the curious, this is necessary due to the random access nature of the DAF
   * architecture.
   * </p>
   * 
   * @param stream a stream from which DAF content is to be read
   * 
   * @return a newly created DAF wrapped around a byte buffer from the consumption of the entire
   *         stream into memory
   * 
   * @throws IOException is generated if access to the contents of stream fails for any reason
   * 
   * @throws DAFValidationFailure if the FTP validation string or the binary file format contained
   *         within the DAF are invalid.
   */
  public static DAF createDAF(InputStream stream) throws IOException {

    /*
     * Check to see if the input stream is an instance of FileInputStream, if it is, then access the
     * FileChannel and pass the buck to the more efficient random access routines.
     */
    if (stream instanceof FileInputStream) {
      return createDAF(((FileInputStream) stream).getChannel());
    }

    /*
     * As we do not know the pedigree of the stream, we are left with no choice but to read the
     * contents of the input stream into a byte buffer in memory. This is our only alternative for
     * the general input stream case, as DAFs are inherently random access in nature.
     */
    byte[] buffer = readStreamFully(stream, BUFFER_LENGTH);
    return createDAF(buffer);
  }

  /**
   * Read the contents of a stream into a newly created byte buffer.
   * 
   * @param istream the stream to read from
   * 
   * @param bufferLength the length of the buffer to use when reading contents of the stream
   * 
   * @return a reference to a newly created byte array containing all of the remaining bytes of
   *         <code>istream</code>
   * 
   * @throws IOException if accessing the stream content fails for any reason
   */
  private static byte[] readStreamFully(InputStream istream, int bufferLength) throws IOException {
    byte[] buffer = new byte[bufferLength];
    int bytesRead;
    ByteArrayOutputStream byteStream = new ByteArrayOutputStream();

    /*
     * Read the contents of istream and write them to byteStream.
     */
    bytesRead = istream.read(buffer);

    while (bytesRead > 0) {
      byteStream.write(buffer, 0, bytesRead);
      bytesRead = istream.read(buffer);
    }

    return byteStream.toByteArray();
  }

  /**
   * Creates and registers the stream with the reference queue for resource release.
   * <p>
   * Note: this method posts phantom references to a queue to invoke the appropriate close()
   * function on the stream. If the invocation of close() results in an {@link IOException}, it
   * simply wraps it in a {@link Throwable} and sends it on its way. This may not be the smartest
   * approach to handling this.
   * </p>
   * 
   * @param stream the stream to close when the DAF is reclaimed by the garbage collector
   * @return the newly created DAF
   * 
   * @throws IOException if problems creating the DAF arise
   */
  static DAF createAndRegisterForResourceRelease(final FileInputStream stream) throws IOException {
    DAF daf = createDAF(stream.getChannel());

    /*
     * Add a reference to the set which will close the channel when the DAF is garbage collected.
     */
    REFERENCES.add(new FinalizablePhantomReference<DAF>(daf, QUEUE) {

      @Override
      public void finalizeReferent() {
        try {
          stream.close();
        } catch (IOException e) {

          /*
           * Not sure what to do if this happens, just propagate and hope for the best.
           */
          throw new RuntimeException(e);
        } finally {
          /*
           * We no longer need to retain this reference, remove it from the set.
           */
          REFERENCES.remove(this);
        }
      }
    });

    return daf;
  }

  /**
   * Create a DAF from the contents of the supplied file.
   * 
   * @param file the file of interest
   * 
   * @return a newly created DAF that wraps itself around the mapped byte buffer connected to the
   *         file specified
   * 
   * @throws IOException if the file is not found or if any errors occur accessing the file content
   * 
   * @throws DAFValidationFailure if the FTP validation string or the binary file format contained
   *         within the DAF are invalid.
   */
  public static DAF createDAF(File file) throws IOException {
    return createAndRegisterForResourceRelease(new FileInputStream(file));
  }

  /**
   * Creates a DAF from the contents of the file at the supplied location.
   * 
   * @param path the path to the DAF of interest
   * 
   * @return a newly created DAF that wraps itself around the mapped byte buffer connected to the
   *         file specified in the path argument.
   * 
   * @throws IOException if the file at path's location can not be found.
   * 
   * @throws DAFValidationFailure if the FTP validation string or the binary file format contained
   *         within the DAF are invalid.
   */
  public static DAF createDAF(String path) throws IOException {
    return createAndRegisterForResourceRelease(new FileInputStream(path));
  }

  /**
   * Creates a DAF from the contents of the supplied file channel.
   * 
   * @param channel the channel which is mapped into a byte buffer and supplies the DAF content
   * 
   * @return a newly created DAF that wraps itself around the mapped byte buffer connected to the
   *         supplied channel
   * 
   * @throws IOException if accessing the channel's content fails
   * 
   * @throws DAFValidationFailure if the FTP validation string or the binary file format contained
   *         within the DAF are invalid.
   */
  public static DAF createDAF(FileChannel channel) throws IOException {
    return new ByteBufferDAF(channel.map(MapMode.READ_ONLY, 0, channel.size()));
  }

  /**
   * Create a DAF that wraps itself around the supplied byte buffer.
   * 
   * @param buffer a buffer, of which a reference is held, supplying the DAF content
   * 
   * @return a newly created DAF
   * 
   * @throws DAFValidationFailure if the FTP validation string or the binary file format contained
   *         within the DAF are invalid.
   */
  public static DAF createDAF(byte[] buffer) {
    return new ByteBufferDAF(ByteBuffer.wrap(buffer));
  }

  /**
   * Create a DAF that wraps itself around the supplied byte buffer.
   * 
   * @param buffer a {@link ByteBuffer} of which a reference is held, supplying the DAF content.
   * 
   * @return a newly created DAF
   * 
   * @throws DAFValidationFailure if the FTP validation string or the binary file format contained
   *         within the DAF are invalid.
   */
  public static DAF createDAF(ByteBuffer buffer) {
    return new ByteBufferDAF(buffer);
  }

}
