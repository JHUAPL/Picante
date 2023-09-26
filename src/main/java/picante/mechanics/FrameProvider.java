package picante.mechanics;

import java.util.List;
import java.util.Set;

/**
 * An interface describing a source of derived frame and state transform functions.
 * 
 * <p>
 * <b>Implementor Notes:</b> Choices available: ignore coverage argument supplied to the create
 * functions. Level of thread safety (none, accessing create methods, accessing transform methods)
 * </p>
 */
public interface FrameProvider {

  /**
   * Retrieves the list of frame sources in load order from the provider.
   * 
   * @param buffer a list to which the sources of the frame provider are to be appended. The first
   *        entry added to this list is the lowest priority, the last the highest.
   * 
   * @return a reference to buffer for convenience
   */
  public List<FrameTransformFunction> getFrameSourcesInLoadOrder();

  /**
   * Adds the list of frame ID codes currently known by the provider to the supplied set. This
   * includes both ID codes that have defining functions in the provider, and those that do
   * not--which are, in a sense, root nodes of the tree structure
   * 
   * @param buffer the buffer to receive the additional frame ID codes
   * 
   * @return a reference to buffer for convenience
   */
  public Set<FrameID> getKnownFrames(Set<FrameID> buffer);

  /**
   * Answers the question, does this provider know about a particular ID? This could be answered by
   * asking the provider for its set of known frames, but this convenience method could be
   * implemented more efficiently.
   * <p>
   * <b>Note:</b>All this implies is that a frame source contains some data connecting to this
   * particular frame. It may not be possible to connect two arbitrary frames at any time of which
   * the provider is aware.
   * </p>
   * 
   * @param id the frame ID of interest
   * 
   * @return true if there exists a source with data for <code>id</code> loaded into the provider
   */
  public boolean isAwareOf(FrameID id);

  /**
   * Create a derived frame transform function that produces rotations from the supplied fromID to
   * the toID which is valid over the supplied domain.
   * 
   * @param fromID the frame ID of the originating frame of the transform. Vectors in this frame can
   *        be rotated directly to the toID frame by application of the transforms produced by this
   *        function.
   * @param toID the frame ID of the destination frame of the transform. Vectors in the fromID frame
   *        are rotated into this frame by application of the transforms produced by the requested
   *        function
   * @param domain the time domain over which transforms are expected to be computed. Note:
   *        Implementors may opt to ignore this argument entirely, so as such consider it a
   *        suggestion. If you are working with a strict, construction time checking implementation,
   *        you must supply a subset of the available coverage loaded into the provider or obtain a
   *        resultant exception.
   * 
   * @return a newly created, derived implementation of a frame transform function connecting fromID
   *         to toID over times specified by domain.
   * 
   */
  public FrameTransformFunction createFrameTransformFunction(FrameID fromID, FrameID toID,
      Coverage domain);

  /**
   * Create a derived state transform function that produces state transformations from the supplied
   * fromID to the toID which is valid over the supplied domain.
   * 
   * @param fromID the frame ID of the originating frame of the transform. States in this frame can
   *        be transformed directly to the toID frame by application of the transforms produced by
   *        this function.
   * @param toID the frame ID of the destination frame of the transform. States in the fromID frame
   *        are transformed into this frame by application of the transforms produced by the
   *        requested function
   * @param domain the time domain over which transforms are expected to be computed. Note:
   *        Implementors may opt to ignore this argument entirely, so as such consider it a
   *        suggestion. If you are working with a strict, construction time checking implementation,
   *        you must supply a subset of the available coverage loaded into the provider or obtain a
   *        resultant exception.
   * 
   * @return a newly created, derived implementation of a state transform function connecting fromID
   *         to toID over times specified by domain.
   * 
   */
  public StateTransformFunction createStateTransformFunction(FrameID fromID, FrameID toID,
      Coverage domain);
}
