package picante.spice.adapters;

import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.Lists;

public class KernelPoolMetaDataTest {

  private KernelPoolMetaData metaData;

  @Before
  public void setUp() throws Exception {
    metaData = new KernelPoolMetaData();
  }

  @Test
  public void testGetComments() {
    assertEquals(Lists.newArrayList(), metaData.getComments());
  }

  @Test
  public void testGetName() {
    assertEquals("ContentDerivedFromKernelPool", metaData.getName());
  }

}
