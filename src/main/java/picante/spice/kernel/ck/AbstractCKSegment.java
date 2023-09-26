package picante.spice.kernel.ck;

/**
 * Simple abstract class that manages retrieval of attributes common to all C-kernel segments.
 */
abstract class AbstractCKSegment implements CKSegment {

  /**
   * The integer code associated with the reference frame to which the segment transforms vectors.
   */
  protected final int instrumentID;

  /**
   * The integer code associated with the reference frame from which the segment transforms vectors.
   */
  protected final int referenceID;

  /**
   * A brief, name, used to identify the segment.
   */
  protected final String name;

  /**
   * The smallest possible encoded SCLK that can be used to query data from the segment.
   */
  protected final double initialEncodedSCLK;

  /**
   * The largest possible encoded SCLK that can be used to query data from the segment.
   */
  protected final double finalEncodedSCLK;

  /**
   * Constructs the abstract segment from the required inputs.
   * 
   * @param name the name of the segment
   * @param instrumentID the frame to which vectors are transformed
   * @param referenceID the frame from which vectors are transformed
   * @param initialEncodedSCLK the first supported encoded SCLK
   * @param finalEncodedSCLK the last supported encoded SCLK
   */
  public AbstractCKSegment(String name, int instrumentID, int referenceID,
      double initialEncodedSCLK, double finalEncodedSCLK) {
    this.name = name;
    this.instrumentID = instrumentID;
    this.referenceID = referenceID;
    this.initialEncodedSCLK = initialEncodedSCLK;
    this.finalEncodedSCLK = finalEncodedSCLK;
  }

  @Override
  public int getInstrumentID() {
    return instrumentID;
  }

  @Override
  public int getReferenceID() {
    return referenceID;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public double getInitialEncodedSCLK() {
    return initialEncodedSCLK;
  }

  @Override
  public double getFinalEncodedSCLK() {
    return finalEncodedSCLK;
  }

}
