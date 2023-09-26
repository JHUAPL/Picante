package picante.spice.daf.content;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.junit.Before;
import org.junit.Test;

public class DAFMetaDataTest {

  private String name;
  private String internalName;
  private List<String> comments;
  private DAFMetaData metadata;

  @Before
  public void setUp() throws Exception {
    name = "NAME";
    internalName = "INTERNALNAME";
    comments = new ArrayList<String>();
    comments.add("Line #1");
    comments.add("Line #2");
    comments = Collections.unmodifiableList(comments);
    metadata = new DAFMetaData(name, internalName, comments);
  }

  @Test
  public void testGetName() {
    assertSame(name, metadata.getName());
  }

  @Test
  public void testGetInternalName() {
    assertSame(internalName, metadata.getInternalName());
  }

  @Test
  public void testGetComments() {
    assertEquals(comments, metadata.getComments());
  }

}
