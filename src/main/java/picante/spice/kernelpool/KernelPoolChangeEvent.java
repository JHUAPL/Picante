package picante.spice.kernelpool;

import java.util.Collections;
import java.util.Set;
import java.util.TreeSet;

/**
 * Class that captures a kernel pool change event, including the type of event and the keywords
 * involved.
 * 
 * The keywords whose contents were altered are broken into three separate categories:
 * 
 * <ul>
 * <li>Deleted, the keyword existed in the pool prior to the change event, but as a result of the
 * event no longer exists.</li>
 * <li>Modified, the keyword existed in the pool prior to the change event, but as a result of the
 * event its contents have been modified. This includes a change in data type from numeric to string
 * or vice versa.</li>
 * <li>Added, the keyword did not exists in the pool prior to the change event, but as a result of
 * the event exists in the pool now.</li>
 * </ul>
 * 
 * The consumer of this class can rely on the following: if a key exists in the set returned by one
 * of the three get added, get modified, or get deleted keyword methods then that keyword will not
 * be present in the other two. The constructors of these events should take care to make certain
 * that this behavior is maintained.
 * 
 * @see KernelPoolEventType
 */
public class KernelPoolChangeEvent {

  private KernelPoolEventType type;

  private Set<String> deleted;

  private Set<String> added;

  private Set<String> modified;

  private Set<String> totalChanged;

  /**
   * Construct a change event of a particular type with the three basic keyword sets. When creating
   * a change event, the constructor must make certain that each changed keyword lives in only one
   * of the three input lists: deleted, modified, or added. No checking is done to validate that
   * this is the case.
   * 
   * @param type the member of the KernelPoolEventType enumeration that defines the specific action
   *        that created this event
   * @param deleted a set containing a list of keywords that were removed from the pool
   * @param modified a set containing a list of keywords that existed in the pool, but have had
   *        their values changed
   * @param added a set containing a list of keywords that did not exist previously but do as a
   *        result of the action that generated this event
   */
  public KernelPoolChangeEvent(KernelPoolEventType type, Set<String> deleted, Set<String> modified,
      Set<String> added) {
    this.type = type;
    this.deleted = deleted;
    this.added = added;
    this.modified = modified;

    TreeSet<String> set = new TreeSet<String>(deleted);
    set.addAll(added);
    set.addAll(modified);
    this.totalChanged = set;
  }

  /**
   * Retrieve an umodifiable view of the set of keywords that were added to pool as a result of the
   * action that generated this event.
   * 
   * @return the set of added keywords
   */
  public Set<String> getAddedKeywords() {
    return Collections.unmodifiableSet(added);
  }

  /**
   * Retrieve an unmodifiable view of the set of keywords that were deleted from pool as a result of
   * the action that generated this event.
   * 
   * @return the set of deleted keywords
   */
  public Set<String> getDeletedKeywords() {
    return Collections.unmodifiableSet(deleted);
  }

  /**
   * Retrieve an unmodifiable view of the set of keywords that were modified in pool as a result of
   * the action that generated this event. This includes keywords whose underlying data type was
   * altered as a result of the event.
   * 
   * @return the set of modified keywords.
   */
  public Set<String> getModifiedKeywords() {
    return Collections.unmodifiableSet(modified);
  }

  /**
   * Retrieve the set of all keywords that changed as a result of the action that generated this
   * event.
   * 
   * @return the set of all keywords that were changed
   */
  public Set<String> getAllChangedKeywords() {
    return Collections.unmodifiableSet(totalChanged);
  }

  /**
   * Retrieve the type of action that generated this event.
   * 
   * @see KernelPoolEventType
   * 
   * @return the member of the enumeration describing the event type
   */
  public KernelPoolEventType getType() {
    return type;
  }

}
