package picante.spice.kernel.tk.fk;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import picante.spice.kernel.tk.fk.dynamic.DynamicFrameFunction;

/**
 * Implementation of the text based frame kernel.
 * 
 * TODO: Consider moving the CKIDBindingFactory outputs into the frame kernel, as this is where a
 * SPICE user would expect them.
 */
public class FrameKernel {

  private final ImmutableMap<Integer, FrameInfo> definedFrames;
  private final ImmutableMap<String, Integer> nameBindings;

  /**
   * Defines all the known, instantiated TKFRAME definitions
   */
  private final ImmutableList<TKFrameFunction> tkFrameFunctions;

  /**
   * This mapping takes C-kernel frame codes into SCLK ID codes, as defined by the frame kernel.
   */
  private final ImmutableMap<Integer, Integer> sclkMap;

  /*
   * Added for dynamic frame implementation
   */
  private final ImmutableList<DynamicFrameFunction> dynamicFrameFunctions;

  public FrameKernel(ImmutableMap<Integer, FrameInfo> definedFrames,
      ImmutableMap<String, Integer> nameBindings, ImmutableMap<Integer, Integer> sclkMap,
      ImmutableList<TKFrameFunction> tkframes,
      ImmutableList<DynamicFrameFunction> dynamicFrameFunctions) {
    super();
    this.definedFrames = definedFrames;
    this.nameBindings = nameBindings;
    this.sclkMap = sclkMap;
    this.tkFrameFunctions = tkframes;
    this.dynamicFrameFunctions = dynamicFrameFunctions;
  }

  public ImmutableMap<Integer, FrameInfo> getDefinedFrames() {
    return definedFrames;
  }

  public ImmutableMap<String, Integer> getNameBindings() {
    return nameBindings;
  }

  public ImmutableMap<Integer, Integer> getSCLKMap() {
    return sclkMap;
  }

  public ImmutableList<TKFrameFunction> getTKFrameFunctions() {
    return tkFrameFunctions;
  }

  public ImmutableList<DynamicFrameFunction> getDynamicFrameFunctions() {
    return dynamicFrameFunctions;
  }
}
