package picante.spice.daf.content;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import picante.spice.daf.DAF;
import picante.spice.daf.DAFSegment;
import picante.spice.kernel.KernelInstantiationException;
import picante.spice.kernel.SegmentedKernelContent;

/**
 * Provides the basic implementation of a NAIF/DAF based kernel.
 * <p>
 * All DAF based kernels can be described as an ordered container of segments and the associated
 * metadata. This abstract class encapsulates the features of these kernels that are common across
 * all kernels. It utilizes generics to allow concrete extensions of the class the flexibility
 * necessary to supply their own implementations.
 * </p>
 * <p>
 * The workhorse of this abstract class is the private
 * {@link #populateSegmentList(DAFContentFactory)} method. This method utilizes the supplied factory
 * to instantiate the templated segment classes based on the details associated with a concrete
 * subclass.
 * </p>
 * 
 * @param <S> the interface describing the specific type of segment the kernel contains
 * @param <E> the specific exception that the concrete subclass will generate if anything goes wrong
 *        in the kernel instantiation process
 * @param <F> the content factory that will create instances of S from individual
 *        <code>DAFSegment</code>s
 */
abstract class DAFBasedKernelContent<S, E extends KernelInstantiationException, F extends DAFContentFactory<S, E>>
    implements SegmentedKernelContent<S> {

  /**
   * The ordered list of segments captured by the DAF based kernel.
   */
  private final List<S> segments;

  /**
   * A reference to the DAF supplied to the constructor.
   */
  private final DAF daf;

  /**
   * The comments, if any, contained within the DAF. Note: this is wrapped with an unmodifiable list
   * at creation time, so references may be handed out to it directly.
   */
  private final List<String> comments;

  /**
   * The internal file name captured in the DAF file record.
   */
  private final String internalName;

  /**
   * Creates a DAF based kernel from the specified factory
   * 
   * @param daf the DAF containing the data to be managed by this instance
   * @param factory the factory used to create type S from individual DAF segments
   * 
   * @throws E if anything goes wrong in the kernel instantiation process
   */
  public DAFBasedKernelContent(DAF daf, F factory) throws E {
    this.daf = daf;
    this.internalName = daf.getName();
    verifyContent(daf.getID());
    this.segments = new ArrayList<S>(daf.getSize());
    populateSegmentList(factory);
    this.comments = Collections
        .unmodifiableList(DAFContentServices.extractComments(daf, new ArrayList<String>()));
  }

  /**
   * Examines the idWord from the DAF to determine if it meets expectations for the specific,
   * concrete type implementing this method.
   * 
   * @param idWord the ID word from the DAF. It is passed in to prevent having to hand a reference
   *        to the DAF directly to the concrete implementing class
   * 
   * @throws E if the ID word does not match the expectations of the implementing type
   */
  abstract void verifyContent(String idWord) throws E;

  /**
   * Examines the contents of a particular DAF segment to determine if it meets the expectations of
   * the concrete implementing subclass.
   * <p>
   * This method is invoked in the constructor of this class prior to handing the segment off to the
   * factory.
   * </p>
   * 
   * @param segment the segment of interest in the DAF.
   * 
   * @throws E if anything about the segment structure isn't as expected. In general only the number
   *         of metadata components are examined to determine if they meet what the factory is
   *         expecting
   */
  abstract void verifySegment(DAFSegment segment) throws E;

  /**
   * Populates the segment list with the resultant specific data content created from the DAF
   * segments by the supplied factory.
   * 
   * @param factory the factory, F, that instantiates S from a generic DAF segment.
   * 
   * @throws E if anything goes wrong in the instantiation process.
   */
  private void populateSegmentList(F factory) throws E {
    for (int i = 0; i < daf.getSize(); i++) {
      DAFSegment segment = daf.getSegment(i);
      verifySegment(segment);
      factory.createAndAdd(segment, segments);
    }
  }

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
  @Override
  public S getSegment(int index) {
    return segments.get(index);
  }

  /**
   * Returns the number of segments in the kernel.
   * 
   * @return the number of segments present
   */
  @Override
  public int getSize() {
    return segments.size();
  }

  /**
   * Add the list of segments to the supplied buffer in the order they appear.
   * 
   * @param buffer buffer to receive additional SPK segments.
   * 
   * @return a reference to buffer for convenience
   */
  @Override
  public List<S> getSegments(List<S> buffer) {
    buffer.addAll(segments);
    return buffer;
  }

  /**
   * Retrieves the comment records contained within the kernel.
   * 
   * @return a reference to an unmodifiable list of <code>String</code> containing each line of the
   *         comments
   */
  @Override
  public List<String> getComments() {
    return comments;
  }

  /**
   * Retrieves the internal name assigned to the kernel at creation time.
   * 
   * @return the internal name
   */
  @Override
  public String getInternalName() {
    return internalName;
  }
}
