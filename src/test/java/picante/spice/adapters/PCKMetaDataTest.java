package picante.spice.adapters;

import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.Lists;

public class PCKMetaDataTest {

  private PCKMetaData pckMetaData;

  @Before
  public void setUp() throws Exception {
    pckMetaData =
        new PCKMetaData("name", "internalName", Lists.newArrayList("some", "comments", "here"));
  }

  @Test
  public void testPCKMetaData() {
    assertEquals("some", pckMetaData.getComments().get(0));
    assertEquals("comments", pckMetaData.getComments().get(1));
    assertEquals("here", pckMetaData.getComments().get(2));

    assertEquals("name", pckMetaData.getName());

    assertEquals("internalName", pckMetaData.getInternalName());
  }

}
