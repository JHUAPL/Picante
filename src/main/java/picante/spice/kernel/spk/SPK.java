package picante.spice.kernel.spk;

import java.util.List;
import picante.spice.kernel.Kernel;
import picante.spice.kernel.KernelType;
import picante.spice.kernel.SegmentedKernelContent;

/**
 * Implementation of the NAIF SPK file.
 * <p>
 * SPK files contain ephemeris information in a collection of segments. Ordering within the SPK file
 * determines precedence, in that segments that occur at the end of the file (higher index) are to
 * be considered first when evaluating an ephemeris request.
 * </p>
 * <p>
 * SPK segments come in a variety of different types.
 * </p>
 */
public class SPK extends Kernel implements SegmentedKernelContent<SPKSegment> {

  private final SegmentedKernelContent<SPKSegment> content;

  public SPK(String name, SegmentedKernelContent<SPKSegment> content) {
    super(KernelType.SPK, name);
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
  public SPKSegment getSegment(int index) {
    return content.getSegment(index);
  }

  @Override
  public List<SPKSegment> getSegments(List<SPKSegment> buffer) {
    return content.getSegments(buffer);
  }

  @Override
  public int getSize() {
    return content.getSize();
  }

}
