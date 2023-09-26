package picante.spice.adapters;

import com.google.common.collect.ImmutableList;
import picante.mechanics.FrameTransformFunction;

class FKAdapter {

  private final KernelPoolMetaData metaData = new KernelPoolMetaData();

  private final ImmutableList<FrameTransformFunction> data;

  public FKAdapter(ImmutableList<TKFrameFunctionAdapter> data) {
    this.data = ImmutableList.copyOf(data);
  }

  public ImmutableList<FrameTransformFunction> getData() {
    return data;
  }

  public KernelPoolMetaData getMetaData() {
    return metaData;
  }

}
