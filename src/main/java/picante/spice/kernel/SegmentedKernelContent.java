package picante.spice.kernel;

import java.util.List;

/**
 * Defines methods necessary for traversing the content of a segment based kernel.
 * 
 * @param <S> the type of segment around which the kernel content is built
 */
public interface SegmentedKernelContent<S> {

  /**
   * Retrieve a segment from the kernel.
   * 
   * @param index the index of the segment of interest
   * 
   * @return the requested segment
   * 
   * @throws IndexOutOfBoundsException if the supplied index lies outside the range [0,
   *         {@link #getSize()}-1]
   */
  public S getSegment(int index);

  /**
   * Returns the number of segments in the kernel.
   * 
   * @return the number of segments present
   */
  public int getSize();

  /**
   * Add the list of segments to the supplied buffer in the order they appear.
   * 
   * @param buffer buffer to receive additional SPK segments.
   * 
   * @return a reference to buffer for convenience
   */
  public List<S> getSegments(List<S> buffer);

  /**
   * Retrieves the comment records contained within the kernel.
   * 
   * @return a reference to an unmodifiable list of <code>String</code> containing each line of the
   *         comments
   */
  public List<String> getComments();

  /**
   * Retrieves the internal name assigned to the kernel at creation time.
   * 
   * @return the internal name
   */
  public String getInternalName();

}
