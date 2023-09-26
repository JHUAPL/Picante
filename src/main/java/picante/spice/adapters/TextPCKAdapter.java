package picante.spice.adapters;

import java.util.List;

import com.google.common.collect.ImmutableList;
import picante.mechanics.FrameTransformFunction;

class TextPCKAdapter {

  private final KernelPoolMetaData metaData = new KernelPoolMetaData();

  private final ImmutableList<FrameTransformFunction> data;

  TextPCKAdapter(List<PCKAdapter> data) {
    this.data = ImmutableList.copyOf(data);
  }

  public ImmutableList<FrameTransformFunction> getData() {
    return this.data;
  }

  public KernelPoolMetaData getMetaData() {
    return metaData;
  }

}
