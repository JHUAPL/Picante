package picante.spice.daf.content;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import picante.spice.daf.DAF;
import picante.spice.daf.DAFSegment;
import picante.spice.daf.bytebuffer.ByteBufferDAF;
import picante.spice.kernel.KernelInstantiationException;

public class DAFBasedKernelContentTest {

  private static ByteBufferDAF testDAF;

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    testDAF = new ByteBufferDAF(createBuffer("test.daf"));
  }

  static ByteBuffer createBuffer(String testfile) throws Exception {
    return ByteBuffer
        .wrap(readStreamFully(DAFBasedKernelContent.class.getResourceAsStream(testfile), 2048));
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

  private TestDAFKernel kernel;

  @Before
  public void setUp() throws Exception {
    kernel = new TestDAFKernel(testDAF, new TestDAFContentFactory(-1));
  }

  @Test(expected = ContentFailureException.class)
  public void testContentFailure() throws Exception {
    new ContentExceptionTestDAFKernel(testDAF, new TestDAFContentFactory(-1));
  }

  @Test(expected = SegmentFailureException.class)
  public void testSegmentFailure() throws Exception {
    new SegmentExceptionTestDAFKernel(testDAF, new TestDAFContentFactory(-1));
  }

  @Test(expected = FactoryException.class)
  public void testFactoryFailure() throws Exception {
    new TestDAFKernel(testDAF, new TestDAFContentFactory(1));
  }

  @Test
  public void testGetSegment() {

    /*
     * By construction, just test that the segments returned are the segments from the actual DAF.
     * We are merely placing them into the internal list.
     */
    for (int i = 0; i < testDAF.getSize(); i++) {
      assertSame("Segment Index #" + i, testDAF.getSegment(i), kernel.getSegment(i));
    }
  }

  @Test
  public void testGetSize() {
    assertEquals(testDAF.getSize(), kernel.getSize());
  }

  @Test
  public void testGetSegments() {
    List<DAFSegment> rawTestSegments = new ArrayList<DAFSegment>(testDAF.getSize());
    for (int i = 0; i < testDAF.getSize(); i++) {
      rawTestSegments.add(testDAF.getSegment(i));
    }

    assertEquals(rawTestSegments, kernel.getSegments(new ArrayList<DAFSegment>(kernel.getSize())));
  }

  @Test
  public void testGetComments() {

    List<String> testComments =
        DAFContentServices.extractComments(testDAF, new ArrayList<String>());

    assertEquals(testComments, kernel.getComments());

  }

  @Test
  public void testGetInternalName() {
    assertEquals(testDAF.getName(), kernel.getInternalName());
  }

}


class TestDAFKernel extends
    DAFBasedKernelContent<DAFSegment, KernelInstantiationException, DAFContentFactory<DAFSegment, KernelInstantiationException>> {

  public TestDAFKernel(DAF daf, DAFContentFactory<DAFSegment, KernelInstantiationException> factory)
      throws KernelInstantiationException {
    super(daf, factory);
  }

  @Override
  void verifyContent(@SuppressWarnings("unused") String idWord)
      throws KernelInstantiationException {}

  @Override
  void verifySegment(@SuppressWarnings("unused") DAFSegment segment)
      throws KernelInstantiationException {}
}


class ContentExceptionTestDAFKernel extends
    DAFBasedKernelContent<DAFSegment, KernelInstantiationException, DAFContentFactory<DAFSegment, KernelInstantiationException>> {

  public ContentExceptionTestDAFKernel(DAF daf,
      DAFContentFactory<DAFSegment, KernelInstantiationException> factory)
      throws KernelInstantiationException {
    super(daf, factory);
  }

  @Override
  void verifyContent(@SuppressWarnings("unused") String idWord)
      throws KernelInstantiationException {
    throw new ContentFailureException();
  }

  @Override
  void verifySegment(@SuppressWarnings("unused") DAFSegment segment)
      throws KernelInstantiationException {}

}


class SegmentExceptionTestDAFKernel extends
    DAFBasedKernelContent<DAFSegment, KernelInstantiationException, DAFContentFactory<DAFSegment, KernelInstantiationException>> {

  public SegmentExceptionTestDAFKernel(DAF daf,
      DAFContentFactory<DAFSegment, KernelInstantiationException> factory)
      throws KernelInstantiationException {
    super(daf, factory);
  }

  @Override
  void verifyContent(@SuppressWarnings("unused") String idWord)
      throws KernelInstantiationException {}

  @Override
  void verifySegment(@SuppressWarnings("unused") DAFSegment segment)
      throws KernelInstantiationException {
    throw new SegmentFailureException();
  }

}


class TestDAFContentFactory implements DAFContentFactory<DAFSegment, KernelInstantiationException> {

  private final int failOnCallNumber;
  private int callNumber = -1;

  public TestDAFContentFactory(int failOnCallNumber) {
    this.failOnCallNumber = failOnCallNumber;
  }

  @Override
  public void createAndAdd(DAFSegment segment, List<DAFSegment> list)
      throws KernelInstantiationException {

    /*
     * Increment the callNumber, then perform the comparison.
     */
    if (++callNumber == failOnCallNumber) {
      throw new FactoryException();
    }

    list.add(segment);
  }
}


class ContentFailureException extends KernelInstantiationException {
  private static final long serialVersionUID = 1L;
}


class SegmentFailureException extends KernelInstantiationException {
  private static final long serialVersionUID = 1L;
}


class FactoryException extends KernelInstantiationException {
  private static final long serialVersionUID = 1L;
}
