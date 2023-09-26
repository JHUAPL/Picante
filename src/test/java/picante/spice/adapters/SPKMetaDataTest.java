package picante.spice.adapters;

import static org.junit.Assert.assertEquals;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.Test;

public class SPKMetaDataTest {

  private SPKMetaData metaData;
  private String name;
  private String internalName;
  private List<String> comments;

  @Before
  public void setUp() throws Exception {
    name = "NAME";
    internalName = "INTERNAL_NAME";
    comments = new ArrayList<String>();
    comments.add("A");
    comments.add("B");
    metaData = new SPKMetaData(name, internalName, comments);
  }

  @Test
  public void testSPKMetaData() {
    assertEquals("NAME", metaData.getName());
    assertEquals("INTERNAL_NAME", metaData.getInternalName());
    assertEquals(2, metaData.getComments().size());
    assertEquals("A", metaData.getComments().get(0));
    assertEquals("B", metaData.getComments().get(1));
  }

}
