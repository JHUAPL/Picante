package picante.spice.adapters;

import static org.easymock.EasyMock.createMock;
import static org.junit.Assert.assertEquals;
import java.util.Arrays;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.Lists;
import picante.mechanics.EphemerisID;
import picante.mechanics.EphemerisSourceIOException;
import picante.mechanics.FrameID;
import picante.spice.kernel.spk.SPKSegment;

public class SPKAdapterTest {

  private SPKAdapter spkAdapter;

  private List<SPKSegmentAdapter> adapters = Lists.newArrayList();

  private SPKSegment mockSegment;
  private EphemerisID mockEID;
  private FrameID mockFID;
  private SPKMetaData metaData;

  @Before
  public void setUp() throws Exception {

    metaData =
        new SPKMetaData("name", "InternalName", Arrays.asList(new String[] {"Comments", "here"}));

    mockSegment = createMock(SPKSegment.class);
    mockEID = createMock(EphemerisID.class);
    mockFID = createMock(FrameID.class);

    adapters.add(new SPKSegmentAdapter(mockEID, mockEID, mockFID, mockSegment));
    adapters.add(new SPKSegmentAdapter(mockEID, mockEID, mockFID, mockSegment));

    spkAdapter = new SPKAdapter(adapters, metaData);
  }

  @Test
  public void testSPKAdapter() {
    try {
      assertEquals(metaData, spkAdapter.getMetaData());
      assertEquals(adapters, spkAdapter.getData());
    } catch (EphemerisSourceIOException e) {
      throw new RuntimeException(e);
    }
  }

  @Test
  public void testGetData() {
    try {
      assertEquals(adapters.size(), spkAdapter.getData().size());
      assertEquals(adapters, spkAdapter.getData());
    } catch (EphemerisSourceIOException e) {
      throw new RuntimeException(e);
    }
  }

  @Test
  public void testGetMetaData() {
    try {
      assertEquals(metaData, spkAdapter.getMetaData());
    } catch (EphemerisSourceIOException e) {
      throw new RuntimeException(e);
    }

  }

}
