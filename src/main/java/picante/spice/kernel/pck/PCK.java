package picante.spice.kernel.pck;

import java.util.List;
import picante.spice.kernel.Kernel;
import picante.spice.kernel.KernelType;
import picante.spice.kernel.SegmentedKernelContent;

/**
 * Implementation of the NAIF binary PCK file.
 * <p>
 * Binary PCK files contain high precision rotation models for celestial bodies in a collection of
 * segments. Ordering within the PCK file determines precedence, in that segments that occur at the
 * end of the file (higher index) are to be considered first when evaluating a state transformation
 * request.
 * </p>
 * <p>
 * PCK segments come in a variety of different types.
 * </p>
 */
public class PCK extends Kernel implements SegmentedKernelContent<PCKSegment> {

  private final SegmentedKernelContent<PCKSegment> content;

  public PCK(String name, SegmentedKernelContent<PCKSegment> content) {
    super(KernelType.PCK, name);
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
  public PCKSegment getSegment(int index) {
    return content.getSegment(index);
  }

  @Override
  public List<PCKSegment> getSegments(List<PCKSegment> buffer) {
    return content.getSegments(buffer);
  }

  @Override
  public int getSize() {
    return content.getSize();
  }

}
