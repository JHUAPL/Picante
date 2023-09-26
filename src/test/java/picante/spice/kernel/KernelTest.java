package picante.spice.kernel;

import static org.junit.Assert.assertEquals;
import java.util.List;
import org.junit.Before;
import org.junit.Test;

public class KernelTest {

  private TestKernel tk;

  @Before
  public void setUp() throws Exception {
    tk = new TestKernel();
  }

  @Test
  public void testGetType() {
    assertEquals(KernelType.SPK, tk.getType());
  }

  @Test
  public void testGetName() {
    assertEquals("TestKernel", tk.getName());
  }

}


class TestKernel extends Kernel {

  public TestKernel() {
    super(KernelType.SPK, "TestKernel");
  }

  @Override
  public List<String> getComments() {
    throw new UnsupportedOperationException();
  }

}
