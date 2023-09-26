package picante.spice.adapters;

import java.util.List;

import com.google.common.collect.ImmutableList;
import picante.mechanics.FrameTransformFunction;

class BinaryPCKAdapter {

  /**
   * The list of adapted PCK segments.
   */
  private final ImmutableList<FrameTransformFunction> adapters;

  /**
   * The PCK meta data.
   */
  private final PCKMetaData metadata;

  protected BinaryPCKAdapter(List<BinaryPCKSegmentAdapter> adapters, PCKMetaData metadata) {
    this.adapters = ImmutableList.copyOf(adapters);
    this.metadata = metadata;
  }

  public ImmutableList<FrameTransformFunction> getData() {
    return adapters;
  }

  public PCKMetaData getMetaData() {
    return metadata;
  }
}
