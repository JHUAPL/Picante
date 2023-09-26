package picante.spice.daf;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static picante.spice.daf.DAFSupport.DAF_BFF_LOCATION;
import static picante.spice.daf.DAFSupport.DAF_BIG_ENDIAN;
import static picante.spice.daf.DAFSupport.DAF_FTP_CHECKSTRING;
import static picante.spice.daf.DAFSupport.DAF_FTP_END;
import static picante.spice.daf.DAFSupport.DAF_FTP_START;
import static picante.spice.daf.DAFSupport.DAF_LITTLE_ENDIAN;
import static picante.spice.daf.DAFSupport.DAF_NI_LOCATION;
import static picante.spice.daf.DAFSupport.DAF_VAX_DFLOAT;
import static picante.spice.daf.DAFSupport.DAF_VAX_GFLOAT;
import static picante.spice.daf.DAFSupport.checkFTPString;
import static picante.spice.daf.DAFSupport.determineByteOrder;
import static picante.spice.daf.DAFSupport.padStringWithSpaces;
import static picante.spice.daf.DAFSupport.trailingTrim;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;

public class DAFSupportTest {

  private static final String CHARSET = "ISO-8859-1";

  @Before
  public void setUp() throws Exception {}

  @Test
  public void testCreateStringFromBytes() throws Exception {
    /*
     * Testing this is rather simple...
     */
    String s = "TESTING..1..2...3?";
    assertEquals(s, DAFSupport.createStringFromBytes(s.getBytes(CHARSET)));
  }

  @Test
  public void testCheckFTPStringNoValidation() {
    byte[] bytes = new byte[1024];
    checkFTPString(bytes);
  }

  @Test
  public void testCheckFTPStringNominal() throws Exception {
    String s = DAF_FTP_START + DAF_FTP_CHECKSTRING + DAF_FTP_END;
    byte[] bytes = new byte[1024];
    System.arraycopy(s.getBytes(CHARSET), 0, bytes, 450, s.length());
    checkFTPString(bytes);
  }

  /**
   * This test was put here just to verify my sanity with regards to the ISO-8859-1 encoding. The
   * US-ASCII encoding is a 7-bit one, so it does odd things when the 8th bit is set.
   * 
   * @throws Exception
   */
  @Test
  public void testISO88591CharsetEncoding() throws Exception {
    byte[] bytes = new byte[256];
    for (int i = -128; i <= 127; i++) {
      bytes[i + 128] = (byte) i;
    }
    String s = new String(bytes, CHARSET);

    byte[] testBytes = s.getBytes(CHARSET);

    assertTrue(Arrays.equals(bytes, testBytes));

    /*
     * And now the other way.
     */
    String testS = new String(testBytes, CHARSET);
    assertEquals(s, testS);
  }

  @Test
  public void testCheckFTPStringOldFile() throws Exception {
    String s = DAF_FTP_START + DAF_FTP_CHECKSTRING.replaceAll("[^:]+:$", "") + DAF_FTP_END;
    byte[] bytes = new byte[1024];
    System.arraycopy(s.getBytes(CHARSET), 0, bytes, 312, s.length());
    checkFTPString(bytes);
  }

  @Test
  public void testCHeckFTPStringNewFile() throws Exception {
    String s = DAF_FTP_START + DAF_FTP_CHECKSTRING + ":AB:" + DAF_FTP_END;
    byte[] bytes = new byte[1024];
    System.arraycopy(s.getBytes(CHARSET), 0, bytes, 400, s.length());
    checkFTPString(bytes);
  }

  @Test(expected = DAFValidationFailure.class)
  public void testCheckFTPStringMissingEnd() throws Exception {
    String s = DAF_FTP_START + DAF_FTP_CHECKSTRING;
    byte[] bytes = new byte[1024];
    System.arraycopy(s.getBytes(CHARSET), 0, bytes, 1023 - s.length(), s.length());
    checkFTPString(bytes);
  }

  @Test(expected = DAFValidationFailure.class)
  public void testCheckFTPStringFailedTransfer() throws Exception {
    String s = DAF_FTP_START + DAF_FTP_CHECKSTRING.replace("\r\n", "\n") + DAF_FTP_END;
    byte[] bytes = new byte[1024];
    System.arraycopy(s.getBytes(CHARSET), 0, bytes, 500, s.length());
    checkFTPString(bytes);
  }

  @Test
  public void testDetermineByteOrderBIGIEEEBFF() throws Exception {
    byte[] bytes = new byte[1024];
    ByteBuffer buffer = ByteBuffer.wrap(bytes);
    buffer.position(DAF_BFF_LOCATION);
    buffer.put(DAF_BIG_ENDIAN.getBytes(CHARSET));
    assertEquals(ByteOrder.BIG_ENDIAN, determineByteOrder(bytes));
  }

  @Test
  public void testDetermineByteOrderLTLIEEEBFF() throws Exception {
    byte[] bytes = new byte[1024];
    ByteBuffer buffer = ByteBuffer.wrap(bytes);
    buffer.position(DAF_BFF_LOCATION);
    buffer.put(DAF_LITTLE_ENDIAN.getBytes(CHARSET));
    assertEquals(ByteOrder.LITTLE_ENDIAN, determineByteOrder(bytes));
  }

  @Test(expected = DAFValidationFailure.class)
  public void testDetermineByteOrderVAXGFLTBFF() throws Exception {
    byte[] bytes = new byte[1024];
    ByteBuffer buffer = ByteBuffer.wrap(bytes);
    buffer.position(DAF_BFF_LOCATION);
    buffer.put(DAF_VAX_GFLOAT.getBytes(CHARSET));
    determineByteOrder(bytes);
  }

  @Test(expected = DAFValidationFailure.class)
  public void testDetermineByteOrderVAXDFLTBFF() throws Exception {
    byte[] bytes = new byte[1024];
    ByteBuffer buffer = ByteBuffer.wrap(bytes);
    buffer.position(DAF_BFF_LOCATION);
    buffer.put(DAF_VAX_DFLOAT.getBytes(CHARSET));
    determineByteOrder(bytes);
  }

  @Test
  public void testDetermineByteOrderNoBFFLittle() throws Exception {
    byte[] bytes = new byte[1024];
    ByteBuffer buffer = ByteBuffer.wrap(bytes);
    buffer.position(DAF_NI_LOCATION);
    buffer.order(ByteOrder.LITTLE_ENDIAN);
    buffer.putInt(20);
    assertEquals(ByteOrder.LITTLE_ENDIAN, determineByteOrder(bytes));
  }

  @Test
  public void testDetermineByteOrderNoBFFBig() throws Exception {
    byte[] bytes = new byte[1024];
    ByteBuffer buffer = ByteBuffer.wrap(bytes);
    buffer.position(DAF_NI_LOCATION);
    buffer.order(ByteOrder.BIG_ENDIAN);
    buffer.putInt(20);
    assertEquals(ByteOrder.BIG_ENDIAN, determineByteOrder(bytes));
  }

  @Test(expected = DAFValidationFailure.class)
  public void testDetermineByteOrderEmptyDAF() throws Exception {
    byte[] bytes = new byte[1024];
    determineByteOrder(bytes);
  }

  @Test
  public void testTrailingTrim() {
    String s = "123   ";
    assertEquals("123", trailingTrim(s));

    s = "123";
    assertEquals("123", trailingTrim(s));

    s = "";
    assertEquals("", trailingTrim(s));

    s = null;
    assertNull(trailingTrim(s));

    s = "     ";
    assertEquals("", trailingTrim(s));

  }

  @Test(expected = IllegalArgumentException.class)
  public void testPadStringWithSpacesNegativeRequiredLengthException() {
    padStringWithSpaces(" ", -1);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testPadStringWithSpacesZeroRequiredLengthException() {
    padStringWithSpaces(" ", 0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testPadStringWithSpacesStringExceedsRequiredLengthException() {
    padStringWithSpaces("ABCDEF", 2);
  }


  @Test
  public void testPadStringWithSpaces() {
    String s = "123";
    assertEquals("123   ", padStringWithSpaces(s, 6));

    s = "";
    assertEquals("      ", padStringWithSpaces(s, 6));

    s = "123456";
    assertEquals("123456", padStringWithSpaces(s, 6));
  }

}
