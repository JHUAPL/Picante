package picante.spice.adapters;

import java.util.List;
import picante.spice.daf.content.DAFMetaData;

/**
 * This class captures meta data provided by a CK.
 * <p>
 * CKs may have a substantial amount of comment data describing the intended usage or purpose for
 * the data contained within. In addition, as CK is built upon the <code>DAF</code> interface, they
 * carry an internal filename that is independent of the mechanism used to access the data. This
 * class captures both of these elements along with the assigned kernel name.
 * </p>
 */
public class CKMetaData extends DAFMetaData {

  /**
   * Creates an instance of the CK meta data.
   * 
   * @param name the name of the kernel
   * @param internalName the internal name captured in the kernel
   * @param comments the comment contents of a CK. This should be an unmodifiable list of
   *        <code>String</code>s, as the get method will simply return the reference retained.
   */
  CKMetaData(String name, String internalName, List<String> comments) {
    super(name, internalName, comments);
  }

}
