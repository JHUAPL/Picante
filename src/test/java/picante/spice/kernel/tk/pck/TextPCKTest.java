package picante.spice.kernel.tk.pck;

import static org.easymock.EasyMock.createMock;
import static org.junit.Assert.assertEquals;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Lists;

public class TextPCKTest {

  private TextPCK textPCK;
  private List<PCKFrameFunction> functions = Lists.newArrayList();
  private ListMultimap<Integer, Double> radii = ArrayListMultimap.create();

  private static final int LENGTH = 6;

  @Before
  public void setUp() throws Exception {

    for (int i = 0; i < LENGTH; i++) {
      functions.add(createMock(PCKFrameFunction.class));
    }

    for (int i = 0; i < LENGTH; i++) {
      radii.put(i, i * 10.0);
      radii.put(i, i * 10 + 1.0);
      radii.put(i, i * 10 + 2.0);
    }

    textPCK = new TextPCK(functions, radii);
  }

  @Test
  public void testTextPCK() {

  }

  @Test
  public void testGetFrameFunctions() {

    int i = 0;
    for (PCKFrameFunction f : textPCK.getFrameFunctions()) {
      assertEquals(functions.get(i++), f);
    }

    assertEquals(LENGTH, textPCK.getFrameFunctions().size());

  }

  @Test
  public void testGetRadii() {
    assertEquals(radii, textPCK.getRadii());
  }

}
