package picante.spice.kernel;

import java.util.List;
import picante.exceptions.BugException;

/**
 * Abstract class capturing general features of all SPICE kernels.
 */
public abstract class Kernel {

  /**
   * The type of the kernel.
   */
  protected KernelType type;

  /**
   * Name of the kernel.
   */
  protected String name;

  /**
   * Creates a kernel of a specified type.
   * 
   * @param type
   */
  protected Kernel(KernelType type, String name) {
    this.type = type;
    this.name = name;
  }

  /**
   * Retrieves the type of the kernel.
   * 
   * @return the element of the <code>KernelType</code> enumeration describing this kernel's type
   */
  public KernelType getType() {
    return type;
  }

  /**
   * Retrieves the name of the kernel.
   * 
   * @return the name assigned to the kernel, typically a filename, but other aspects may be
   *         utilized
   */
  public String getName() {
    return name;
  }

  @Override
  public String toString() {

    switch (type) {
      case TEXT:
        return "TK[" + name + "]";
      case SPK:
        return "SPK[" + name + "]";
      case PCK:
        return "PCK[" + name + "]";
      case CK:
        return "CK[" + name + "]";
    }

    throw new BugException("Unknown kernel type.");
  }

  /**
   * Retrieves an unmodifiable list of comments from the kernel.
   * 
   * @return a possibly empty, unmodifiable list of strings.
   */
  public abstract List<String> getComments();

}
