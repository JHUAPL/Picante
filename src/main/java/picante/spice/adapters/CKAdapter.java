package picante.spice.adapters;

import java.util.List;

import com.google.common.collect.ImmutableList;
import picante.mechanics.FrameTransformFunction;

/**
 * Adapter class that adapts CK files to the <code>FrameSource</code> interface.
 */
class CKAdapter {

  /**
   * The list of adapted CK segments.
   */
  private final ImmutableList<FrameTransformFunction> adapters;

  /**
   * The CK meta data.
   */
  private final CKMetaData metadata;

  /**
   * Creates an adapter.
   * 
   * @param adapters a list of CK segment adapters in priority order derived from the CK file. The
   *        instance stores a reference to the supplied list through an unmodifiable wrapper.
   * 
   * @param metadata the metadata associated with the CK. The instance stores a reference to the
   *        supplied object.
   */
  protected CKAdapter(List<CKSegmentAdapter> adapters, CKMetaData metadata) {
    this.adapters = ImmutableList.copyOf(adapters);
    this.metadata = metadata;
  }

  public ImmutableList<FrameTransformFunction> getData() {
    return adapters;
  }

  public CKMetaData getMetaData() {
    return metadata;
  }

}
