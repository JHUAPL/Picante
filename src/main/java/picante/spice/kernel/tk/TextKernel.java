package picante.spice.kernel.tk;

import java.util.Collections;
import java.util.List;
import picante.spice.kernel.Kernel;
import picante.spice.kernel.KernelType;
import picante.spice.kernelpool.BasicKernelPool;
import picante.spice.kernelpool.UnwritableKernelPool;

/**
 * Implementation of the NAIF text kernel. Each text kernel contains its own set of comments and a
 * pool that contains the parse content.
 * <p>
 * Note: the pool and comment fields of this class are populated
 */
public class TextKernel extends Kernel {

  /**
   * Reference to an unmodifiable view of the pool supplied to the constructor.
   */
  private final UnwritableKernelPool pool;

  /**
   * A list of strings containing the kernel comments.
   */
  private final List<String> comments;

  /**
   * Constructs a new text kernel.
   * 
   * @param name the name of the kernel
   * @param comments the comments associated with the kernel. This class retains a reference to this
   *        list and passes unmodifiable views out to consumers.
   * @param pool the pool capturing the text kernel content. This class retains a reference to this
   *        list and passes unmodifiable views out to consumers.
   */
  public TextKernel(String name, List<String> comments, BasicKernelPool pool) {
    super(KernelType.TEXT, name);
    this.pool = new UnwritableKernelPool(pool);
    this.comments = Collections.unmodifiableList(comments);
  }

  /**
   * Constructs a new text kernel with no comments content.
   * 
   * @param name the name to associate with the kernel
   * @param pool the pool capturing the text kernel content. This class retains a reference to this
   *        list and passes unmodifiable views out to consumers.
   */
  @SuppressWarnings("unchecked")
  // Allows the method to use the immutable empty list.
  public TextKernel(String name, BasicKernelPool pool) {
    this(name, Collections.EMPTY_LIST, pool);
  }

  @Override
  public List<String> getComments() {
    return comments;
  }

  public UnwritableKernelPool getPool() {
    return pool;
  }

}
