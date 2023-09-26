package picante.spice;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.ListIterator;
import picante.spice.kernel.Kernel;
import picante.spice.kernel.KernelType;
import picante.spice.kernel.ck.CK;
import picante.spice.kernel.pck.PCK;
import picante.spice.kernel.spk.SPK;
import picante.spice.kernel.tk.TextKernel;

/**
 * This class tracks SPICE kernels loaded into the system and the associated priority of the loaded
 * kernels.
 */
class KernelManager {

  private final LinkedList<StringKernelPair> kernels = new LinkedList<StringKernelPair>();

  /**
   * Associates the supplied SPK with the identifier and places it at the end of the list of
   * available SPKs.
   * 
   * @param identifier an identifier to associate with the SPK, later used to remove the kernel
   * 
   * @param kernel the SPK of interest
   */
  public void add(String identifier, Kernel kernel) {
    kernels.add(new StringKernelPair(identifier, kernel));
  }

  /**
   * Removes the last loaded instance of the identifier from the set of kernels under management. If
   * identifier is not found in the list of loaded kernels, nothing is done.
   * 
   * @param identifier the identifier associated with a kernel to remove.
   */
  public void remove(String identifier) {
    ListIterator<StringKernelPair> iterator = kernels.listIterator(kernels.size());

    while (iterator.hasPrevious()) {
      if (iterator.previous().identifier.equals(identifier)) {
        iterator.remove();
        return;
      }
    }
  }

  /**
   * Retrieves a collection of SPKs when iterated over preserves the load order. The first loaded
   * file is listed first, the last last.
   * 
   * @return a collection of SPKs loaded into the kernel manager
   */
  public Collection<SPK> getSpks() {
    LinkedList<SPK> result = new LinkedList<SPK>();
    ListIterator<StringKernelPair> iterator = kernels.listIterator();
    while (iterator.hasNext()) {
      StringKernelPair pair = iterator.next();
      if (pair.kernel.getType().equals(KernelType.SPK)) {
        result.add((SPK) pair.kernel);
      }
    }
    return result;
  }

  /**
   * Retrieves a collection of CKs when iterated over preserves the load order. The first loaded
   * file is listed first, the last last.
   * 
   * @return a collection of CKs loaded into the kernel manager
   */
  public Collection<CK> getCks() {
    LinkedList<CK> result = new LinkedList<CK>();
    ListIterator<StringKernelPair> iterator = kernels.listIterator();
    while (iterator.hasNext()) {
      StringKernelPair pair = iterator.next();
      if (pair.kernel.getType().equals(KernelType.CK)) {
        result.add((CK) pair.kernel);
      }
    }
    return result;
  }

  /**
   * Retrieves a collection of PCKs when iterated over preserves the load order. The first loaded
   * file is listed first, the last last.
   * 
   * @return a collection of PCKs loaded into the kernel manager
   */
  public Collection<PCK> getPCKs() {
    LinkedList<PCK> result = new LinkedList<PCK>();
    ListIterator<StringKernelPair> iterator = kernels.listIterator();
    while (iterator.hasNext()) {
      StringKernelPair pair = iterator.next();
      if (pair.kernel.getType().equals(KernelType.PCK)) {
        result.add((PCK) pair.kernel);
      }
    }
    return result;
  }

  /**
   * Retrieves a collection of text kernel content when iterated over preserves the load order. The
   * first loaded file is listed first, the last last.
   * 
   * @return a collection of text kernel content loaded into the kernel manager
   */
  public Collection<TextKernel> getTextKernels() {
    LinkedList<TextKernel> result = new LinkedList<TextKernel>();
    ListIterator<StringKernelPair> iterator = kernels.listIterator();
    while (iterator.hasNext()) {
      StringKernelPair pair = iterator.next();
      if (pair.kernel.getType().equals(KernelType.TEXT)) {
        result.add((TextKernel) pair.kernel);
      }
    }
    return result;
  }

  /**
   * Retrieves a collection of <code>String</code> identifiers for each kernel loaded into the
   * manager when iterated over preserves the load order.
   * 
   * @return the collection of identifiers used identify each loaded kernel
   */
  public Collection<String> getIdentifiers() {
    ArrayList<String> result = new ArrayList<String>(kernels.size());
    for (StringKernelPair pair : kernels) {
      result.add(pair.identifier);
    }
    return result;
  }

  /**
   * Private inner class supporting the implementation of the kernel manager. This class pairs a
   * sorted map of loaded file keys and kernels with a particular candidate loaded file key for
   * removal.
   */
  private class StringKernelPair {
    final String identifier;
    final Kernel kernel;

    public StringKernelPair(String identifier, Kernel kernel) {
      this.identifier = identifier;
      this.kernel = kernel;
    }
  }

}
