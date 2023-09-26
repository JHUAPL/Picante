package picante.spice.adapters;

import java.util.List;

import com.google.common.collect.ImmutableList;
import picante.mechanics.PositionVectorFunction;

/**
 * Adapter class that adapts SPK files to the <code>EphemerisSource</code> interface.
 */
class SPKAdapter {

  /**
   * The list of adapted SPK segments.
   */
  private final ImmutableList<PositionVectorFunction> adapters;

  /**
   * The SPK meta data.
   */
  private final SPKMetaData metadata;

  /**
   * Creates an adapter.
   * 
   * @param adapters a list of SPK segment adapters in priority order derived from the SPK file. The
   *        instance stores a reference to the supplied list through an unmodifiable wrapper.
   * @param metadata the metadata associated with the SPK. The instance stores a reference to the
   *        supplied object.
   */
  protected SPKAdapter(List<SPKSegmentAdapter> adapters, SPKMetaData metadata) {
    this.adapters = ImmutableList.copyOf(adapters);
    this.metadata = metadata;
  }

  public ImmutableList<PositionVectorFunction> getData() {
    return adapters;
  }

  public SPKMetaData getMetaData() {
    return metadata;
  }

}
