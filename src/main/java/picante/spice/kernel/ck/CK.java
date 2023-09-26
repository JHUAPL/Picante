package picante.spice.kernel.ck;

import java.util.List;
import picante.spice.kernel.Kernel;
import picante.spice.kernel.KernelType;
import picante.spice.kernel.SegmentedKernelContent;

/**
 * Implementation of the NAIF CK file.
 * <p>
 * CK files contain orientation and angular rate information in a collection of segments. Ordering
 * within the CK file determines precedence, in that segments that occur at the end of the file
 * (higher index) are to be considered first when evaluating a request for pointing.
 * </p>
 * <p>
 * CK segments come in a variety of types.
 * </p>
 */
public class CK extends Kernel implements SegmentedKernelContent<CKSegment> {

  private final SegmentedKernelContent<CKSegment> content;

  public CK(String name, SegmentedKernelContent<CKSegment> content) {
    super(KernelType.CK, name);
    this.content = content;
  }

  @Override
  public List<String> getComments() {
    return content.getComments();
  }

  @Override
  public String getInternalName() {
    return content.getInternalName();
  }

  @Override
  public CKSegment getSegment(int index) {
    return content.getSegment(index);
  }

  @Override
  public List<CKSegment> getSegments(List<CKSegment> buffer) {
    return content.getSegments(buffer);
  }

  @Override
  public int getSize() {
    return content.getSize();
  }

}
