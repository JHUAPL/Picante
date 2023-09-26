package picante.spice.kernel.tk.lsk;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.DoubleBuffer;
import java.util.LinkedList;
import java.util.List;

/**
 * Class encapsulating utilities that support implementation of test code within this package.
 */
class TestUtilities {

  /**
   * Retrieves the contents of a test file generated from code that links to SPICE.
   * 
   * @param filename name of the file on the class path
   * @return a list of doubles, in pairs, of source and destination times
   * 
   * @throws IOException if an IO error occurs
   */
  static List<Double> getTestFileContents(String filename) throws IOException {

    List<Double> contents = new LinkedList<Double>();

    ByteBuffer buffer =
        ByteBuffer.wrap(readStreamFully(TestUtilities.class.getResourceAsStream(filename), 1024));

    buffer.order(ByteOrder.LITTLE_ENDIAN);
    DoubleBuffer data = buffer.asDoubleBuffer();

    for (int i = 0; i < data.capacity(); i++) {
      contents.add(data.get(i));
    }

    return contents;
  }

  /**
   * Reads the contents of the supplied stream completely into a byte buffer.
   * 
   * @param istream the stream to read completely
   * 
   * @param bufferLength the length of an internal buffer to read results into, this is a chunking,
   *        not a limit to the amount of data read
   * @return a newly created byte array containing the contents of istream
   * 
   * @throws IOException if an IO error occurs
   */
  static byte[] readStreamFully(InputStream istream, int bufferLength) throws IOException {
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

}
