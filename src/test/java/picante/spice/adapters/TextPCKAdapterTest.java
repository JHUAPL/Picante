package picante.spice.adapters;

import static org.easymock.EasyMock.createMock;
import static org.junit.Assert.assertEquals;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.Lists;
import picante.mechanics.FrameID;
import picante.mechanics.FrameSourceIOException;
import picante.spice.kernel.tk.pck.PCKFrameFunction;

public class TextPCKAdapterTest {

  private TextPCKAdapter textPCKAdapter;

  private KernelPoolMetaData metaData;
  private List<PCKAdapter> data = Lists.newArrayList();

  private FrameID fromID;
  private FrameID toID;
  private PCKFrameFunction pff;

  @Before
  public void setUp() throws Exception {
    metaData = new KernelPoolMetaData();

    fromID = createMock(FrameID.class);
    toID = createMock(FrameID.class);
    pff = createMock(PCKFrameFunction.class);

    data.add(new PCKAdapter(fromID, toID, pff));
    data.add(new PCKAdapter(fromID, toID, pff));
    data.add(new PCKAdapter(fromID, toID, pff));

    textPCKAdapter = new TextPCKAdapter(data);
  }

  @Test
  public void testTextPCKAdapter() {
    try {
      assertEquals(data, textPCKAdapter.getData());
      assertEquals(data.size(), textPCKAdapter.getData().size());
      assertEquals(metaData.getComments(), textPCKAdapter.getMetaData().getComments());
      assertEquals(metaData.getName(), textPCKAdapter.getMetaData().getName());
    } catch (FrameSourceIOException e) {
      throw new RuntimeException(e);
    }
  }

  @Test
  public void testGetData() {
    try {
      assertEquals(data, textPCKAdapter.getData());
      assertEquals(data.size(), textPCKAdapter.getData().size());
    } catch (FrameSourceIOException e) {
      throw new RuntimeException(e);
    }
  }

  @Test
  public void testGetMetaData() {
    try {
      assertEquals(metaData.getComments(), textPCKAdapter.getMetaData().getComments());
      assertEquals(metaData.getName(), textPCKAdapter.getMetaData().getName());
    } catch (FrameSourceIOException e) {
      throw new RuntimeException(e);
    }
  }

}
