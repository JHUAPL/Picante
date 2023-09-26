package picante.spice.kernel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;

public class ByteBufferReaderTest {

  private ByteBuffer buffer;
  private ByteBufferReader reader;
  private char[] cbuf = new char[10];

  @Before
  public void setUp() throws Exception {
    buffer = ByteBuffer.wrap("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789".getBytes("ISO-8859-1"));
    reader = new ByteBufferReader(buffer, Charset.forName("ISO-8859-1"));
  }

  @Test
  public void testReadCharArrayIntInt() throws Exception {
    assertEquals(10, reader.read(cbuf, 0, 10));
    assertTrue(Arrays.equals("ABCDEFGHIJ".toCharArray(), cbuf));
    assertEquals(10, reader.read(cbuf, 0, 10));
    assertTrue(Arrays.equals("KLMNOPQRST".toCharArray(), cbuf));
    assertEquals(10, reader.read(cbuf, 0, 10));
    assertTrue(Arrays.equals("UVWXYZ0123".toCharArray(), cbuf));
    assertEquals(6, reader.read(cbuf, 0, 10));
    char[] subCbuf = new char[6];
    System.arraycopy(cbuf, 0, subCbuf, 0, 6);
    assertTrue(Arrays.equals("456789".toCharArray(), subCbuf));
    assertEquals(-1, reader.read(cbuf, 0, 10));
  }

}
