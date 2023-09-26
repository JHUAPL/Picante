package picante.spice.daf.bytebuffer;

import static org.junit.Assert.assertEquals;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.DoubleBuffer;
import org.junit.BeforeClass;
import org.junit.Test;
import picante.spice.daf.DAFSegment;
import picante.spice.daf.DAFValidationFailure;

/**
 * Test case for the byte buffer implementation of the DAF interface.
 * <p>
 * The .daf files stored in this package are required for the test to execute properly. They were
 * created with the NAIF toolkit in order to test this reader's capabilities. Note: some of the
 * files were modified with a hex editor to exercise various pathological cases (old files, failed
 * FTP transfer, etc.)
 * </p>
 */
public class ByteBufferDAFTest {

  /**
   * Simple test DAF with a fully populated descriptor record.
   */
  private static ByteBufferDAF TESTONE;

  /**
   * Big endian version of TESTONE. Note: the comment records still need to be read with a little
   * endian byte ordering, as bingo the converison tool leaves the comment records alone.
   */
  private static ByteBufferDAF TESTTWO;

  /**
   * Identical to TESTONE, however the NAIF binary file format string has been nulled out of the
   * file to force an examination of the contents to determine endian state.
   */
  private static ByteBufferDAF TESTTHREE;

  /**
   * Identical to TESTTWO, however the NAIF binary file format string has been nulled out of the
   * file to force an examination of the contents to determine endian state.
   */
  private static ByteBufferDAF TESTFOUR;

  /**
   * Identical to TESTONE, however the NAIF FTP validation string has been nulled out of the file.
   * This should result in no FTP validation being performed.
   */
  private static ByteBufferDAF TESTFIVE;

  /**
   * Identical to TESTONE, however the NAIF FTP validation string has been modified to simulate a
   * FTP file transfer failure. This should result in an exception being generated when the file is
   * loaded.
   */
  private static ByteBufferDAF TESTSIX;

  /**
   * This DAF is designed to exercise an odd number of integer meta data components, as well as the
   * logic for multiple descriptors per segment.
   */
  private static ByteBufferDAF TESTSEVEN;

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    TESTONE = new ByteBufferDAF(createBuffer("testone.daf"));
    TESTTWO = new ByteBufferDAF(createBuffer("testtwo.daf"));
    TESTTHREE = new ByteBufferDAF(createBuffer("testthree.daf"));
    TESTFOUR = new ByteBufferDAF(createBuffer("testfour.daf"));
    TESTFIVE = new ByteBufferDAF(createBuffer("testfive.daf"));
    TESTSEVEN = new ByteBufferDAF(createBuffer("testseven.daf"));
  }

  static ByteBuffer createBuffer(String testfile) throws Exception {
    return ByteBuffer
        .wrap(readStreamFully(ByteBufferDAFTest.class.getResourceAsStream(testfile), 2048));
  }

  static byte[] readStreamFully(InputStream istream, int bufferLength) throws Exception {
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

  @Test(expected = DAFValidationFailure.class)
  public void testTESTSIXfailedFTPValidation() throws Exception {
    TESTSIX = new ByteBufferDAF(createBuffer("testsix.daf"));
    TESTSIX.getID();
  }

  @Test
  public void testTESTONEgetId() {
    assertEquals("DAF/tdaf", TESTONE.getID());
  }

  @Test
  public void testTESTONEgetName() {
    assertEquals("Standard Test DAF ND=124,NI=2", TESTONE.getName());
  }

  @Test
  public void testTESTONEgetReservedRecords() {

    DoubleBuffer buffer = ByteBuffer.wrap(TESTONE.getReservedRecords())
        .order(ByteOrder.LITTLE_ENDIAN).asDoubleBuffer();

    for (int i = 0; i < 4 * 128; i++) {
      assertEquals("At index " + i, 2.0 * (i + 1), buffer.get(), 0.0);
    }

  }

  @Test
  public void testTESTONEgetSegmentOne() {
    DAFSegment segment = TESTONE.getSegment(0);

    assertEquals(124, segment.getND());
    assertEquals(0, segment.getNI());
    assertEquals(100, segment.getLength());

    assertEquals("FIRST SEGMENT", segment.getName());

    for (int i = 0; i < 124; i++) {
      assertEquals("At index " + i, 1.0 * (i + 1), segment.getDoubleComponent(i), 0.0);
    }

    double[] values = new double[100];
    segment.get(0, values, 0, 100);

    for (int i = 0; i < 100; i++) {
      assertEquals("At index " + i, -1.0 * (i + 1), values[i], 0.0);
    }

  }

  @Test
  public void testTESTONEgetSegmentTwo() {
    DAFSegment segment = TESTONE.getSegment(1);

    assertEquals(124, segment.getND());
    assertEquals(0, segment.getNI());
    assertEquals(1024, segment.getLength());

    assertEquals("SECOND SEGMENT", segment.getName());

    for (int i = 0; i < 124; i++) {
      assertEquals("At index " + i, 2.0 * (i + 1), segment.getDoubleComponent(i), 0.0);
    }

    double[] values = new double[1024];
    segment.get(0, values, 0, 1024);

    for (int i = 0; i < 1024; i++) {
      assertEquals("At index " + i, 2.0 * (i + 1), values[i], 0.0);
    }

  }

  @Test
  public void testTESTONEgetSize() {
    assertEquals(2, TESTONE.getSize());
  }

  @Test
  public void testTESTTWOgetId() {
    assertEquals("DAF/tdaf", TESTTWO.getID());
  }

  @Test
  public void testTESTTWOgetName() {
    assertEquals("Standard Test DAF ND=124,NI=2", TESTTWO.getName());
  }

  @Test
  public void testTESTTWOgetReservedRecords() {

    DoubleBuffer buffer = ByteBuffer.wrap(TESTTWO.getReservedRecords())
        .order(ByteOrder.LITTLE_ENDIAN).asDoubleBuffer();

    for (int i = 0; i < 4 * 128; i++) {
      assertEquals("At index " + i, 2.0 * (i + 1), buffer.get(), 0.0);
    }

  }

  @Test
  public void testTESTTWOgetSegmentOne() {
    DAFSegment segment = TESTTWO.getSegment(0);

    assertEquals(124, segment.getND());
    assertEquals(0, segment.getNI());
    assertEquals(100, segment.getLength());

    assertEquals("FIRST SEGMENT", segment.getName());

    for (int i = 0; i < 124; i++) {
      assertEquals("At index " + i, 1.0 * (i + 1), segment.getDoubleComponent(i), 0.0);
    }

    double[] values = new double[100];
    segment.get(0, values, 0, 100);

    for (int i = 0; i < 100; i++) {
      assertEquals("At index " + i, -1.0 * (i + 1), values[i], 0.0);
    }

  }

  @Test
  public void testTESTTWOgetSegmentTwo() {
    DAFSegment segment = TESTTWO.getSegment(1);

    assertEquals(124, segment.getND());
    assertEquals(0, segment.getNI());
    assertEquals(1024, segment.getLength());

    assertEquals("SECOND SEGMENT", segment.getName());

    for (int i = 0; i < 124; i++) {
      assertEquals("At index " + i, 2.0 * (i + 1), segment.getDoubleComponent(i), 0.0);
    }

    double[] values = new double[1024];
    segment.get(0, values, 0, 1024);

    for (int i = 0; i < 1024; i++) {
      assertEquals("At index " + i, 2.0 * (i + 1), values[i], 0.0);
    }

  }

  @Test
  public void testTESTTWOgetSize() {
    assertEquals(2, TESTTWO.getSize());
  }

  @Test
  public void testTESTTHREEgetId() {
    assertEquals("DAF/tdaf", TESTTHREE.getID());
  }

  @Test
  public void testTESTTHREEgetName() {
    assertEquals("Standard Test DAF ND=124,NI=2", TESTTHREE.getName());
  }

  @Test
  public void testTESTTHREEgetReservedRecords() {

    DoubleBuffer buffer = ByteBuffer.wrap(TESTTHREE.getReservedRecords())
        .order(ByteOrder.LITTLE_ENDIAN).asDoubleBuffer();

    for (int i = 0; i < 4 * 128; i++) {
      assertEquals("At index " + i, 2.0 * (i + 1), buffer.get(), 0.0);
    }

  }

  @Test
  public void testTESTTHREEgetSegmentOne() {
    DAFSegment segment = TESTTHREE.getSegment(0);

    assertEquals(124, segment.getND());
    assertEquals(0, segment.getNI());
    assertEquals(100, segment.getLength());

    assertEquals("FIRST SEGMENT", segment.getName());

    for (int i = 0; i < 124; i++) {
      assertEquals("At index " + i, 1.0 * (i + 1), segment.getDoubleComponent(i), 0.0);
    }

    double[] values = new double[100];
    segment.get(0, values, 0, 100);

    for (int i = 0; i < 100; i++) {
      assertEquals("At index " + i, -1.0 * (i + 1), values[i], 0.0);
    }

  }

  @Test
  public void testTESTTHREEgetSegmentTwo() {
    DAFSegment segment = TESTTHREE.getSegment(1);

    assertEquals(124, segment.getND());
    assertEquals(0, segment.getNI());
    assertEquals(1024, segment.getLength());

    assertEquals("SECOND SEGMENT", segment.getName());

    for (int i = 0; i < 124; i++) {
      assertEquals("At index " + i, 2.0 * (i + 1), segment.getDoubleComponent(i), 0.0);
    }

    double[] values = new double[1024];
    segment.get(0, values, 0, 1024);

    for (int i = 0; i < 1024; i++) {
      assertEquals("At index " + i, 2.0 * (i + 1), values[i], 0.0);
    }

  }

  @Test
  public void testTESTTHREEgetSize() {
    assertEquals(2, TESTTHREE.getSize());
  }

  @Test
  public void testTESTFOURgetId() {
    assertEquals("DAF/tdaf", TESTFOUR.getID());
  }

  @Test
  public void testTESTFOURgetName() {
    assertEquals("Standard Test DAF ND=124,NI=2", TESTFOUR.getName());
  }

  @Test
  public void testTESTFOURgetReservedRecords() {

    DoubleBuffer buffer = ByteBuffer.wrap(TESTFOUR.getReservedRecords())
        .order(ByteOrder.LITTLE_ENDIAN).asDoubleBuffer();

    for (int i = 0; i < 4 * 128; i++) {
      assertEquals("At index " + i, 2.0 * (i + 1), buffer.get(), 0.0);
    }

  }

  @Test
  public void testTESTFOURgetSegmentOne() {
    DAFSegment segment = TESTFOUR.getSegment(0);

    assertEquals(124, segment.getND());
    assertEquals(0, segment.getNI());
    assertEquals(100, segment.getLength());

    assertEquals("FIRST SEGMENT", segment.getName());

    for (int i = 0; i < 124; i++) {
      assertEquals("At index " + i, 1.0 * (i + 1), segment.getDoubleComponent(i), 0.0);
    }

    double[] values = new double[100];
    segment.get(0, values, 0, 100);

    for (int i = 0; i < 100; i++) {
      assertEquals("At index " + i, -1.0 * (i + 1), values[i], 0.0);
    }

  }

  @Test
  public void testTESTFOURgetSegmentTwo() {
    DAFSegment segment = TESTFOUR.getSegment(1);

    assertEquals(124, segment.getND());
    assertEquals(0, segment.getNI());
    assertEquals(1024, segment.getLength());

    assertEquals("SECOND SEGMENT", segment.getName());

    for (int i = 0; i < 124; i++) {
      assertEquals("At index " + i, 2.0 * (i + 1), segment.getDoubleComponent(i), 0.0);
    }

    double[] values = new double[1024];
    segment.get(0, values, 0, 1024);

    for (int i = 0; i < 1024; i++) {
      assertEquals("At index " + i, 2.0 * (i + 1), values[i], 0.0);
    }

  }

  @Test
  public void testTESTFOURgetSize() {
    assertEquals(2, TESTFOUR.getSize());
  }

  @Test
  public void testTESTFIVEgetId() {
    assertEquals("DAF/tdaf", TESTFIVE.getID());
  }

  @Test
  public void testTESTFIVEgetName() {
    assertEquals("Standard Test DAF ND=124,NI=2", TESTFIVE.getName());
  }

  @Test
  public void testTESTFIVEgetReservedRecords() {

    DoubleBuffer buffer = ByteBuffer.wrap(TESTFIVE.getReservedRecords())
        .order(ByteOrder.LITTLE_ENDIAN).asDoubleBuffer();

    for (int i = 0; i < 4 * 128; i++) {
      assertEquals("At index " + i, 2.0 * (i + 1), buffer.get(), 0.0);
    }

  }

  @Test
  public void testTESTFIVEgetSegmentOne() {
    DAFSegment segment = TESTFIVE.getSegment(0);

    assertEquals(124, segment.getND());
    assertEquals(0, segment.getNI());
    assertEquals(100, segment.getLength());

    assertEquals("FIRST SEGMENT", segment.getName());

    for (int i = 0; i < 124; i++) {
      assertEquals("At index " + i, 1.0 * (i + 1), segment.getDoubleComponent(i), 0.0);
    }

    double[] values = new double[100];
    segment.get(0, values, 0, 100);

    for (int i = 0; i < 100; i++) {
      assertEquals("At index " + i, -1.0 * (i + 1), values[i], 0.0);
    }

  }

  @Test
  public void testTESTFIVEgetSegmentTwo() {
    DAFSegment segment = TESTFIVE.getSegment(1);

    assertEquals(124, segment.getND());
    assertEquals(0, segment.getNI());
    assertEquals(1024, segment.getLength());

    assertEquals("SECOND SEGMENT", segment.getName());

    for (int i = 0; i < 124; i++) {
      assertEquals("At index " + i, 2.0 * (i + 1), segment.getDoubleComponent(i), 0.0);
    }

    double[] values = new double[1024];
    segment.get(0, values, 0, 1024);

    for (int i = 0; i < 1024; i++) {
      assertEquals("At index " + i, 2.0 * (i + 1), values[i], 0.0);
    }

  }

  @Test
  public void testTESTFIVEgetSize() {
    assertEquals(2, TESTFIVE.getSize());
  }

  @Test
  public void testTESTSEVENgetId() {
    assertEquals("DAF/tdaf", TESTSEVEN.getID());
  }

  @Test
  public void testTESTSEVENgetName() {
    assertEquals("Standard Test DAF ND=50,NI=5", TESTSEVEN.getName());
  }

  @Test
  public void testTESTSEVENgetReservedRecords() {
    assertEquals(0, TESTSEVEN.getReservedRecords().length);
  }

  @Test
  public void testTESTSEVENgetSegmentOne() {
    DAFSegment segment = TESTSEVEN.getSegment(0);

    assertEquals(50, segment.getND());
    assertEquals(3, segment.getNI());
    assertEquals(60, segment.getLength());

    assertEquals("FIRST SEGMENT", segment.getName());

    for (int i = 0; i < 50; i++) {
      assertEquals("At index " + i, 3.0 * (i + 1), segment.getDoubleComponent(i), 0.0);
    }

    for (int i = 0; i < 3; i++) {
      assertEquals("At index " + i, (i + 1), segment.getIntComponent(i));
    }

    double[] values = new double[60];
    segment.get(0, values, 0, 60);

    for (int i = 0; i < 60; i++) {
      assertEquals("At index " + i, 5.0 * (i + 1), values[i], 0.0);
    }
  }

  @Test
  public void testTESTSEVENgetSegmentTwo() {
    DAFSegment segment = TESTSEVEN.getSegment(1);

    assertEquals(50, segment.getND());
    assertEquals(3, segment.getNI());
    assertEquals(1024, segment.getLength());

    assertEquals("SECOND SEGMENT", segment.getName());

    for (int i = 0; i < 50; i++) {
      assertEquals("At index " + i, -3.0 * (i + 1), segment.getDoubleComponent(i), 0.0);
    }

    assertEquals(-1, segment.getIntComponent(0));
    assertEquals(-2, segment.getIntComponent(1));
    assertEquals(-3, segment.getIntComponent(2));

    double[] values = new double[1024];
    segment.get(0, values, 0, 1024);

    for (int i = 0; i < 1024; i++) {
      assertEquals("At index " + i, -20.0 + (i + 1), values[i], 0.0);
    }
  }

  @Test
  public void testTESTSEVENgetSegmentThree() {
    DAFSegment segment = TESTSEVEN.getSegment(2);

    assertEquals(50, segment.getND());
    assertEquals(3, segment.getNI());
    assertEquals(512, segment.getLength());

    assertEquals("THIRD SEGMENT", segment.getName());

    for (int i = 0; i < 50; i++) {
      assertEquals("At index " + i, -1.0 * (i + 1), segment.getDoubleComponent(i), 0.0);
    }

    assertEquals(30, segment.getIntComponent(0));
    assertEquals(25, segment.getIntComponent(1));
    assertEquals(20, segment.getIntComponent(2));

    double[] values = new double[512];
    segment.get(0, values, 0, 512);

    for (int i = 0; i < 512; i++) {
      assertEquals("At index " + i, (i + 1), values[i], 0.0);
    }
  }

  @Test
  public void testTESTSEVENgetSize() {
    assertEquals(3, TESTSEVEN.getSize());
  }

}
