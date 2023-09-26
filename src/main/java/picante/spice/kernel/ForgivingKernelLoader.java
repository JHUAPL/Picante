package picante.spice.kernel;

import picante.exceptions.BugException;
import picante.spice.daf.DAF;
import picante.spice.daf.content.CKSegmentFactory;
import picante.spice.daf.content.DAFBackedCKContent;
import picante.spice.daf.content.DAFBackedPCKContent;
import picante.spice.daf.content.DAFBackedSPKContent;
import picante.spice.daf.content.DAFContentServices;
import picante.spice.daf.content.PCKSegmentFactory;
import picante.spice.daf.content.SPKSegmentFactory;
import picante.spice.kernel.ck.CK;
import picante.spice.kernel.pck.PCK;
import picante.spice.kernel.spk.SPK;

public class ForgivingKernelLoader extends KernelLoader {

  @Override
  Kernel createKernelFromDAF(String name, DAF daf) throws KernelInstantiationException {

    KernelType type = DAFContentServices.identifyDAF(daf);

    if (type == null) {
      throw new BugException("Unsupported DAF based kernel type. ID word was: " + daf.getID());
    }

    switch (type) {

      case CK:
        return new CK(name, new DAFBackedCKContent(daf, CKSegmentFactory.DISREGARDING));
      case SPK:
        return new SPK(name, new DAFBackedSPKContent(daf, SPKSegmentFactory.DISREGARDING));
      case PCK:
        return new PCK(name, new DAFBackedPCKContent(daf, PCKSegmentFactory.DISREGARDING));

      default:
        throw new BugException("Unsupported DAF based kernel type: " + type);
    }
  }

}
