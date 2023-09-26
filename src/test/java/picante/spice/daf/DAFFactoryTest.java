package picante.spice.daf;

import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import picante.spice.daf.bytebuffer.ByteBufferDAF;
import picante.spice.kernel.KernelLoader;

/**
 * As this code is not currently utilized by the high level SPICE adapters, I decided to ignore its
 * unit testing until I had time to revisit it. (Likely these adapters {@link KernelLoader} in
 * particular should be using it to isolate its direct dependence on {@link ByteBufferDAF}. For now,
 * what we have works, so let's just let sleeping dogs lie.
 */
@Ignore
public class DAFFactoryTest {

  @Before
  public void setUp() throws Exception {}

  @Test
  public void testCreateDAFInputStream() {
    fail("Not yet implemented");
  }

  @Test
  public void testCreateDAFFile() {
    fail("Not yet implemented");
  }

  @Test
  public void testCreateDAFString() {
    fail("Not yet implemented");
  }

  @Test
  public void testCreateDAFFileChannel() {
    fail("Not yet implemented");
  }

  @Test
  public void testCreateDAFByteArray() {
    fail("Not yet implemented");
  }

  @Test
  public void testCreateDAFByteBuffer() {
    fail("Not yet implemented");
  }

}
