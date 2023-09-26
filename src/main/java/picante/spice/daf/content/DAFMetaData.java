package picante.spice.daf.content;

import java.util.Collections;
import java.util.List;
import picante.spice.SpiceMetaData;

/**
 * Class providing basic meta data services for DAF based kernels.
 * <p>
 * All specific DAF based kernels must do is subclass this and provide their own specific features
 * if required.
 * </p>
 */
public class DAFMetaData extends SpiceMetaData {

  private final String name;
  private final String internalName;
  private final List<String> comments;

  /**
   * Creates an instance of the DAF meta data.
   * 
   * @param name the name of the kernel.
   * @param internalName the internal name captured by the DAF at creation time
   * @param comments an unmodifiable list of comments. <b>Note:</b> this constructor holds onto a
   *        direct reference to the supplied list of strings
   */
  public DAFMetaData(String name, String internalName, List<String> comments) {
    this.name = name;
    this.internalName = internalName;
    this.comments = Collections.unmodifiableList(comments);
  }

  /**
   * Retrieve the name assigned to the kernel.
   * 
   * @return a string containing the name
   */
  @Override
  public String getName() {
    return name;
  }

  /**
   * Retrieve the name stored internally in the kernel's data.
   * 
   * @return a string containing the name
   */
  public String getInternalName() {
    return internalName;
  }

  /**
   * Retrieve the comment data from the kernel.
   * 
   * @return a list of strings, one element captures one line of data from the comment content of
   *         the kernel. This list may be empty in the event the kernel has no comments.
   */
  @Override
  public List<String> getComments() {
    return comments;
  }

}
