package picante.spice.daf.content;

import java.util.List;
import picante.spice.daf.DAFSegment;
import picante.spice.kernel.KernelInstantiationException;

/**
 * Parent interface of the individual DAF content creation factories.
 * <p>
 * This interface is merely an implementation detail about how DAF based kernels are managed. It
 * acts as the parent of all segment creation factory interfaces utilized by the DAF system.
 * </p>
 * <p>
 * This interface is public simply due to choices about package management in this particular set of
 * packages in the library. It can be safely ignored as merely an exposed implementation detail.
 * </p>
 * 
 * @param <T> the actual segment derived content type
 * @param <E> a kernel creation exception that is specific to the type implementing the interface
 */
public interface DAFContentFactory<T, E extends KernelInstantiationException> {

  /**
   * Create content derived from an arbitrary DAF segment, and add it to the end of the list of
   * supplied content.
   * 
   * @param segment the DAF segment from which to create derived content
   * @param list a list of content derived from preceding segments in the DAF
   * 
   * @throws E an exception that derives from {@link KernelInstantiationException} in the event that
   *         anything fails in the creation of the derived content
   */
  public void createAndAdd(DAFSegment segment, List<T> list) throws E;

}
