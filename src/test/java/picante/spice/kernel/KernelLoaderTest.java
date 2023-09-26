package picante.spice.kernel;

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expectLastCall;
import static org.easymock.EasyMock.replay;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileChannel.MapMode;
import java.util.Arrays;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import com.google.common.collect.ImmutableSet;
import com.google.common.io.ByteSource;
import com.google.common.io.Resources;
import picante.exceptions.BugException;
import picante.spice.daf.DAF;
import picante.spice.kernel.spk.SPK;
import picante.spice.kernel.tk.TextKernel;
import picante.spice.kernelpool.BasicKernelPool;
import picante.spice.kernelpool.UnwritableKernelPool;
import picante.spice.kernelpool.parser.TextKernelParser;

public class KernelLoaderTest {

  private static final int READ_BUFFER_SIZE = 10240;

  private static File spkFile;
  private static File textFile;

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    spkFile = File.createTempFile("SPK", ".bsp");
    textFile = File.createTempFile("TEXT", ".tk");

    populateFileFromStream(KernelLoaderTest.class.getResourceAsStream("combined.bsp"), spkFile);
    populateFileFromStream(KernelLoaderTest.class.getResourceAsStream("test.tls"), textFile);

    /*
     * Just in case something bad, but not horribly nasty happens which would cause this test code
     * to abort.F
     */
    spkFile.deleteOnExit();
    textFile.deleteOnExit();
  }

  private static void populateFileFromStream(InputStream stream, File file) throws IOException {

    FileOutputStream fileStream = new FileOutputStream(file);

    byte[] buffer = new byte[READ_BUFFER_SIZE];
    int bytesRead;

    /*
     * Read the contents of istream and write them to byteStream.
     */
    bytesRead = stream.read(buffer);

    while (bytesRead > 0) {
      fileStream.write(buffer, 0, bytesRead);
      bytesRead = stream.read(buffer);
    }

    fileStream.close();
  }

  @AfterClass
  public static void tearDownAfterClass() throws Exception {
    spkFile.delete();
    textFile.delete();
  }

  private KernelLoader loader;

  @Before
  public void setUp() throws Exception {
    loader = new KernelLoader();
  }

  @Test
  public void testLoadFile() throws Exception {
    Kernel kernel = loader.load(spkFile);
    assertEquals(KernelType.SPK, kernel.getType());
    assertEquals(4, ((SPK) kernel).getSize());

    kernel = loader.load(textFile);
    assertEquals(KernelType.TEXT, kernel.getType());
    assertEquals(5, ((TextKernel) kernel).getPool().getKeywords().size());
  }

  @Test
  public void testLoadStringInputStream() throws Exception {
    Kernel kernel = loader.load("TESTSTREAM",
        Resources.asByteSource(Resources.getResource(getClass(), "combined.bsp")));

    assertEquals(KernelType.SPK, kernel.getType());
    assertEquals(4, ((SPK) kernel).getSize());
  }

  @Test
  public void testLoadStringInputStreamFileCast() throws Exception {
    final TestFileInputStream testStream = new TestFileInputStream(textFile);

    /*
     * This is a complete abuse of the ByteSource API, as it returns the same stream.
     */
    Kernel kernel = loader.load("TESTSTREAM", new ByteSource() {

      @Override
      public InputStream openStream() throws IOException {
        return testStream;
      }
    });

    assertTrue(testStream.wasChannelRetrieved());
    assertEquals(KernelType.TEXT, kernel.getType());
    assertEquals(5, ((TextKernel) kernel).getPool().getKeywords().size());
  }

  @Test
  public void testLoadStringByteBufferDAF() throws Exception {
    FileInputStream stream = new FileInputStream(spkFile);
    FileChannel channel = stream.getChannel();
    ByteBuffer dafBuffer = channel.map(MapMode.READ_ONLY, 0, channel.size());
    Kernel kernel = loader.load("TESTNAME", dafBuffer);

    assertEquals("TESTNAME", kernel.getName());
    assertEquals(KernelType.SPK, kernel.getType());
    assertEquals(4, ((SPK) kernel).getSize());
    stream.close();
  }

  @Test
  public void testLoadStringByteBufferText() throws Exception {
    FileInputStream stream = new FileInputStream(textFile);
    FileChannel channel = stream.getChannel();
    ByteBuffer txtBuffer = channel.map(MapMode.READ_ONLY, 0, channel.size());
    Kernel kernel = loader.load("TESTNAME", txtBuffer);

    assertEquals("TESTNAME", kernel.getName());
    assertEquals(KernelType.TEXT, kernel.getType());
    assertEquals(5, ((TextKernel) kernel).getPool().getKeywords().size());
    stream.close();
  }

  @Test
  public void testCreateKernelFromDAFSpk() throws Exception {
    DAF mock = createNiceMock(DAF.class);

    mock.getID();
    expectLastCall().andReturn("DAF/SPK").anyTimes();
    mock.getReservedRecords();
    expectLastCall().andReturn(new byte[0]).anyTimes();

    replay(mock);
    Kernel kernel = loader.createKernelFromDAF("TESTMOCK", mock);

    assertEquals(KernelType.SPK, kernel.getType());
    assertEquals("TESTMOCK", kernel.getName());
  }

  @Test
  public void testCreateKernelFromDAFCk() throws Exception {
    DAF mock = createNiceMock(DAF.class);

    mock.getID();
    expectLastCall().andReturn("DAF/CK").anyTimes();
    mock.getReservedRecords();
    expectLastCall().andReturn(new byte[0]).anyTimes();

    replay(mock);
    Kernel kernel = loader.createKernelFromDAF("TESTMOCK", mock);

    assertEquals(KernelType.CK, kernel.getType());
    assertEquals("TESTMOCK", kernel.getName());
  }

  @Test
  public void testCreateKernelFromDAFPck() throws Exception {
    DAF mock = createNiceMock(DAF.class);

    mock.getID();
    expectLastCall().andReturn("DAF/PCK").anyTimes();
    mock.getReservedRecords();
    expectLastCall().andReturn(new byte[0]).anyTimes();

    replay(mock);
    Kernel kernel = loader.createKernelFromDAF("TESTMOCK", mock);

    assertEquals(KernelType.PCK, kernel.getType());
    assertEquals("TESTMOCK", kernel.getName());
  }

  @Test(expected = BugException.class)
  public void testCreateKernelFromDAFUnexpected() throws Exception {
    DAF mock = createNiceMock(DAF.class);

    mock.getID();
    expectLastCall().andReturn("DAF/????").anyTimes();
    mock.getReservedRecords();
    expectLastCall().andReturn(new byte[0]).anyTimes();

    replay(mock);
    loader.createKernelFromDAF("TESTMOCK", mock);
  }

  private byte[] bytes(String s) throws UnsupportedEncodingException {
    return s.getBytes("ISO-8859-1");
  }

  @Test
  public void testIsDAF() throws Exception {
    assertTrue(loader.isDAF(bytes("NAIF/DAF")));
    assertTrue(loader.isDAF(bytes("DAF/SPK ")));
    assertTrue(loader.isDAF(bytes("DAF/CK  ")));
    assertTrue(loader.isDAF(bytes("DAF/PCK ")));
    assertTrue(loader.isDAF(bytes("DAF/SPK")));
    assertTrue(loader.isDAF(bytes("DAF/CK")));
    assertTrue(loader.isDAF(bytes("DAF/PCK")));
    assertTrue(loader.isDAF(bytes("DAF/NEWT")));
    assertTrue(loader.isDAF(bytes("daf/spk ")));
    assertFalse(loader.isDAF(bytes("KPL/FK  ")));
    assertFalse(loader.isDAF(bytes("NAIF/ETF")));
    assertFalse(loader.isDAF(bytes("AbCdEfGh")));
  }

  @Test
  public void testLoadStreamFully() throws Exception {

    byte[] data = new byte[10240 * 10 + 5];
    for (int i = 0; i < data.length; i++) {
      data[i] = (byte) i;
    }

    ByteBuffer buffer = loader.loadStreamFully(new ByteArrayInputStream(data));

    assertTrue(buffer.hasArray());
    assertTrue(Arrays.equals(data, buffer.array()));
  }

  /**
   * This kernel caused a problem with the KernelLoader, this method specifically.
   * 
   * <pre>
   * \begindata
   * NAIF_BODY_NAME+='CHOPPER'
   * NAIF_BODY_CODE+=-200000
   * \begintext
   * </pre>
   * 
   * @throws Exception
   */
  @Test
  public void testCreateTextKernelBufferOffsetFailure() throws Exception {

    String contents =
        "\\begindata\nNAIF_BODY_NAME+='CHOPPER'\nNAIF_BODY_CODE+=-200000\n\\begintext\n";
    ByteBuffer buffer = ByteBuffer.wrap(contents.getBytes("ISO-8859-1"));

    byte[] idWord = new byte[8];
    buffer.get(idWord);

    TextKernel kernel = loader.createTextKernel("TEST", buffer);

    assertEquals(ImmutableSet.of("NAIF_BODY_CODE", "NAIF_BODY_NAME"),
        ImmutableSet.copyOf(kernel.getPool().getKeywords()));

  }

  @Test
  public void testCreateTextKernel() throws Exception {

    Kernel kernel =
        loader.createTextKernel("TESTTEXT", loader.loadStreamFully(new FileInputStream(textFile)));

    assertEquals("TESTTEXT", kernel.getName());
    assertEquals(KernelType.TEXT, kernel.getType());

    TextKernel tKernel = (TextKernel) kernel;
    UnwritableKernelPool actual = tKernel.getPool();

    BasicKernelPool expected = new TextKernelParser().parse(new FileReader(textFile));

    assertEquals(expected.getKeywords(), actual.getKeywords());

    for (String keyword : expected.getKeywords()) {
      if (expected.isStringValued(keyword)) {
        assertEquals(expected.getStrings(keyword), actual.getStrings(keyword));
      } else {
        assertEquals(expected.getDoubles(keyword), actual.getDoubles(keyword));
      }
    }

  }

}


class TestFileInputStream extends FileInputStream {

  private boolean channelRetrieved = false;

  public TestFileInputStream(File file) throws FileNotFoundException {
    super(file);
  }

  @Override
  public FileChannel getChannel() {
    channelRetrieved = true;
    return super.getChannel();
  }

  public boolean wasChannelRetrieved() {
    return channelRetrieved;
  }

}
