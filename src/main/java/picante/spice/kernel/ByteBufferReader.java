package picante.spice.kernel;

import java.io.IOException;
import java.io.Reader;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;

/**
 * Simple extension of Reader that wraps around a byte buffer using the NIO API.
 */
class ByteBufferReader extends Reader {

  private final CharBuffer charBuffer;

  public ByteBufferReader(ByteBuffer buffer, Charset charset) {
    charBuffer = charset.decode(buffer);
  }

  @Override
  public int read(char[] cbuf, int off, int len) throws IOException {

    /*
     * Check to see if we have exhausted all of the bytes available.
     */
    if (charBuffer.remaining() == 0) {
      return -1;
    }

    /*
     * If we make it this far, there clearly a bytes to be read. Check to see if the requested
     * number of bytes (len) exceeds the available amount remaining. A buffer underflow exception
     * will result otherwise.
     */
    int bytesToRead = Math.min(len, charBuffer.remaining());
    charBuffer.get(cbuf, off, bytesToRead);
    return bytesToRead;
  }

  @Override
  public void close() throws IOException {}

}
