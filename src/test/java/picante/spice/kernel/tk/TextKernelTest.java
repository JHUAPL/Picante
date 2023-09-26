package picante.spice.kernel.tk;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import picante.spice.kernel.KernelType;
import picante.spice.kernelpool.BasicKernelPool;
import picante.spice.kernelpool.UnwritableKernelPool;

public class TextKernelTest {

  private TextKernel kernel;
  private List<String> comments;
  private BasicKernelPool pool;

  @Before
  public void setUp() throws Exception {
    comments = new ArrayList<String>();
    comments.add("Line #1");
    comments.add("Line #2");
    pool = new BasicKernelPool();
    pool.addStrings("STRINGS", comments);
    kernel = new TextKernel("NAME", comments, pool);
  }

  @Test
  public void testGetComments() {
    assertEquals(comments, kernel.getComments());
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testGetCommentsUnmodifiableReturnValue() {
    kernel.getComments().add("TEST");
  }

  @Test
  public void testTextKernelStringBasicKernelPool() {
    TextKernel emptyComments = new TextKernel("NAME", pool);
    UnwritableKernelPool pool = kernel.getPool();
    assertEquals(1, pool.getKeywords().size());
    assertTrue(pool.getKeywords().contains("STRINGS"));
    assertEquals(comments, pool.getStrings("STRINGS"));
    assertEquals(0, emptyComments.getComments().size());
  }

  @Test
  public void testGetPool() {
    UnwritableKernelPool pool = kernel.getPool();
    assertEquals(1, pool.getKeywords().size());
    assertTrue(pool.getKeywords().contains("STRINGS"));
    assertEquals(comments, pool.getStrings("STRINGS"));
  }

  @Test
  public void testGetName() {
    assertEquals("NAME", kernel.getName());
  }

  @Test
  public void testGetType() {
    assertEquals(KernelType.TEXT, kernel.getType());
  }

}
