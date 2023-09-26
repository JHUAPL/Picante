package picante.spice.kernel.tk.fk;

import static org.easymock.EasyMock.createMock;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;

public class FrameKernelTest {

  private FrameKernel kernel;

  private ImmutableMap<Integer, FrameInfo> definedFrames;
  private ImmutableMap<String, Integer> nameBindings;
  private ImmutableMap<Integer, Integer> sclkMap;
  private ImmutableList<TKFrameFunction> tkFrames;

  @Before
  public void setUp() throws Exception {

    definedFrames = ImmutableMap.of(0, createMock(FrameInfo.class), 1, createMock(FrameInfo.class),
        2, createMock(FrameInfo.class), 3, createMock(FrameInfo.class));
    nameBindings = ImmutableMap.of("0", 0, "1", 1, "2", 2, "3", 3);
    sclkMap = ImmutableMap.of(0, 0, 1, 1, 2, 2, 3, 3);
    tkFrames =
        ImmutableList.of(createMock(TKFrameFunction.class), createMock(TKFrameFunction.class));

    kernel = new FrameKernel(definedFrames, nameBindings, sclkMap, tkFrames, ImmutableList.of());
  }

  @Test
  public void testFrameKernel() {
    assertEquals(definedFrames, kernel.getDefinedFrames());
    assertEquals(nameBindings, kernel.getNameBindings());
    assertEquals(sclkMap, kernel.getSCLKMap());
    assertEquals(tkFrames, kernel.getTKFrameFunctions());

    assertNotSame(definedFrames, kernel.getNameBindings());
    assertNotSame(definedFrames, kernel.getSCLKMap());
    assertNotSame(sclkMap, kernel.getNameBindings());

  }

  @Test
  public void testGetDefinedFrames() {
    assertEquals(definedFrames, kernel.getDefinedFrames());
  }

  @Test
  public void testGetNameBindings() {
    assertEquals(nameBindings, kernel.getNameBindings());
  }

  @Test
  public void testGetSCLKMap() {
    assertEquals(sclkMap, kernel.getSCLKMap());
  }

  @Test
  public void testGetTKFrameFunctions() {
    assertEquals(tkFrames, kernel.getTKFrameFunctions());
  }

}
