package picante.spice.daf.content;

import picante.spice.daf.DAF;
import picante.spice.daf.DAFSegment;
import picante.spice.kernel.pck.BinaryPCKInstantiationException;
import picante.spice.kernel.pck.PCKSegment;

public class DAFBackedPCKContent
    extends DAFBasedKernelContent<PCKSegment, BinaryPCKInstantiationException, PCKSegmentFactory> {

  /**
   * Creates a PCK file from the supplied DAF.
   * 
   * @param daf the DAF containing the PCK data.
   * @param factory a segment creation factory
   * 
   * @throws BinaryPCKInstantiationException if anything fails in the instantiation of the PCK file.
   */
  public DAFBackedPCKContent(DAF daf, PCKSegmentFactory factory)
      throws BinaryPCKInstantiationException {
    super(daf, factory);
  }

  /**
   * {@inheritDoc}
   * 
   * Simply checks to see that the ID word is DAF/PCK.
   */
  @Override
  void verifyContent(String idWord) throws BinaryPCKInstantiationException {
    if (!idWord.equals("DAF/PCK")) {
      throw new BinaryPCKInstantiationException("Supporting DAF ID word is [" + idWord
          + "] which does not indicate a DAF with PCK content.");
    }
  }

  /**
   * {@inheritDoc}
   * 
   * Simply checks to see that ND and NI are 2 and 3 respectively.
   */
  @Override
  void verifySegment(DAFSegment segment) throws BinaryPCKInstantiationException {
    if (segment.getND() != 2) {
      throw new BinaryPCKInstantiationException(
          "The number of double precision metadata components " + "in the underlying DAF ["
              + segment.getND() + "] should be 2.");
    }

    if (segment.getNI() != 3) {
      throw new BinaryPCKInstantiationException(
          "The number of integer metadata components in the underly DAF [" + segment.getNI()
              + "] should be 3.");
    }
  }

}
