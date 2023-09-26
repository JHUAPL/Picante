package picante.spice.fov;

import java.util.Collections;
import java.util.List;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.mechanics.FrameID;
import picante.spice.fov.FOVFactory.Shape;

/**
 * Field of view object. Use {@link FOVFactory} to create this.
 * 
 * @author nairah1
 *
 */
public class FOVSpice {

  private int instrumentID;

  private Shape shape;
  private FrameID frame;
  private UnwritableVectorIJK boresight;
  private List<UnwritableVectorIJK> bounds;

  /**
   * The SPICE instrument ID
   * 
   * @return
   */
  public int getInstrumentID() {
    return instrumentID;
  }

  /**
   * The FOV {@link Shape}, contained in the kernel pool variable INS-XXXXX_FOV_SHAPE
   * 
   * @return
   */
  public Shape getShape() {
    return shape;
  }

  /**
   * The instrument reference frame, contained in the kernel pool variable INS-XXXXX_FOV_FRAME
   * 
   * @return
   */
  public FrameID getFrame() {
    return frame;
  }

  /**
   * The instrument boresight, contained in the kernel pool variable INS-XXXXX_BORESIGHT
   * 
   * @return
   */
  public UnwritableVectorIJK getBoresight() {
    return boresight;
  }

  /**
   * The instrument FOV boundary vectors in the instrument reference frame.
   * 
   * @return
   */
  public List<UnwritableVectorIJK> getBounds() {
    return bounds;
  }

  /**
   * Package private constructor, called from {@link FOVFactory}
   * 
   * @param instrumentID
   * @param shape
   * @param frame
   * @param boresight
   * @param bounds
   */
  FOVSpice(int instrumentID, Shape shape, FrameID frame, UnwritableVectorIJK boresight,
      List<UnwritableVectorIJK> bounds) {
    this.instrumentID = instrumentID;
    this.shape = shape;
    this.frame = frame;
    this.boresight = boresight;
    this.bounds = Collections.unmodifiableList(bounds);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append(String.format("Instrument ID: %d\n", getInstrumentID()));
    sb.append(String.format("    FOV shape: %s\n", getShape().name()));
    sb.append(String.format("    FOV frame: %s\n", getFrame().getName()));
    sb.append(String.format("FOV boresight: %s\n", getBoresight().toString()));
    sb.append("  FOV corners:\n");
    for (UnwritableVectorIJK b : getBounds()) {
      sb.append(String.format("               %s\n", b.toString()));
    }
    return sb.toString();
  }

}
