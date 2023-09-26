package picante.spice.fov;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import picante.math.intervals.UnwritableInterval;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.FrameID;
import picante.mechanics.rotations.AxisAndAngle;
import picante.mechanics.utilities.SimpleFrameID;
import picante.spice.SpiceEnvironmentBuilder;
import picante.spice.adapters.SpiceFrameID;
import picante.spice.kernel.KernelInstantiationException;
import picante.spice.kernelpool.UnwritableKernelPool;
import picante.spice.kernelpool.parser.ParseException;
import picante.surfaces.Plane;

/**
 * This class creates an {@link FOVSpice} object from the kernel pool given an instrument ID.
 * 
 * @author nairah1
 *
 */
public class FOVFactory {

  /**
   * Shapes supported by SPICE
   */
  public enum Shape {
    POLYGON, RECTANGLE, CIRCLE, ELLIPSE;
  }

  private UnwritableKernelPool kp;

  public FOVFactory(UnwritableKernelPool kp) {
    this.kp = kp;
  }

  /**
   * This method is modeled after getfov() in SPICE.
   * 
   * @param instrID
   * @return
   */
  public FOV create(int instrID) {

    String frameKeyword = String.format("INS%d_FOV_FRAME", instrID);
    FrameID frame = null;
    if (kp.hasKeyword(frameKeyword)) {
      String frameName = kp.getStrings(frameKeyword).get(0);
      frameKeyword = String.format("FRAME_%s", frameName);
      if (kp.hasKeyword(frameKeyword)) {
        Integer frameID = kp.getIntegers(frameKeyword).get(0);
        frame = new SpiceFrameID(frameID);
      } else {
        // this may not be connected to anything
        frame = new SimpleFrameID(frameName);
      }
    }

    String shapeKeyword = String.format("INS%d_FOV_SHAPE", instrID);
    Shape shape = null;
    if (kp.hasKeyword(shapeKeyword)) {
      shape = Shape.valueOf(kp.getStrings(shapeKeyword).get(0).toUpperCase().trim());
    }

    List<Double> boresightVector = kp.getDoubles(String.format("INS%d_BORESIGHT", instrID));
    UnwritableVectorIJK boresight = new UnwritableVectorIJK(boresightVector.get(0),
        boresightVector.get(1), boresightVector.get(2));

    List<UnwritableVectorIJK> bounds = new ArrayList<>();

    String classKeyword = String.format("INS%d_FOV_CLASS_SPEC", instrID);
    boolean corners = true;
    if (kp.hasKeyword(classKeyword)) {
      if (kp.getStrings(classKeyword).get(0).toUpperCase().trim().equals("ANGLES")) {
        corners = false;
      }
    }

    if (corners) {
      String boundaryKeyword = String.format("INS%d_FOV_BOUNDARY", instrID);
      boolean hasBoundary = kp.hasKeyword(boundaryKeyword);
      if (!hasBoundary) {
        boundaryKeyword = String.format("INS%d_FOV_BOUNDARY_CORNERS", instrID);
        hasBoundary = kp.hasKeyword(boundaryKeyword);
      }

      if (hasBoundary) {
        List<Double> doubles = kp.getDoubles(boundaryKeyword);
        for (int i = 0; i < doubles.size(); i += 3) {
          bounds
              .add(new UnwritableVectorIJK(doubles.get(i), doubles.get(i + 1), doubles.get(i + 2)));
        }
        FOVSpice fovSpice = new FOVSpice(instrID, shape, frame, boresight, bounds);
        return new FOV(frame, fovSpice);
      }

    } else {
      // CLASS_SPEC is ANGLES - only supported for CIRCLE, ELLIPSE, or RECTANGLE

      String refKeyword = String.format("INS%d_FOV_REF_VECTOR", instrID);
      UnwritableVectorIJK refVector = null;
      if (kp.hasKeyword(refKeyword)) {
        List<Double> doubles = kp.getDoubles(refKeyword);
        refVector = new UnwritableVectorIJK(doubles.get(0), doubles.get(1), doubles.get(2));
      }

      refKeyword = String.format("INS%d_FOV_REF_ANGLE", instrID);
      double refAngle = Double.NaN;
      if (kp.hasKeyword(refKeyword)) {
        List<Double> doubles = kp.getDoubles(refKeyword);
        refAngle = doubles.get(0);
      }

      String units = kp.getStrings(String.format("INS%d_FOV_ANGLE_UNITS", instrID)).get(0);
      boolean unitsInDegrees = units.equalsIgnoreCase("DEGREES");
      if (unitsInDegrees) {
        refAngle = Math.toRadians(refAngle);
      }

      if (shape == Shape.CIRCLE) {
        // get the boundary vector by rotating boresight by refAngle towards refVector
        VectorIJK axis = VectorIJK.cross(boresight, refVector);

        AxisAndAngle aaa = new AxisAndAngle(axis, refAngle);
        bounds.add(aaa.getRotation(new RotationMatrixIJK()).mxv(boresight));

      } else {
        String crsKeyword = String.format("INS%d_FOV_CROSS_ANGLE", instrID);
        double crsAngle = Double.NaN;
        if (kp.hasKeyword(crsKeyword)) {
          List<Double> doubles = kp.getDoubles(crsKeyword);
          crsAngle = doubles.get(0);
          if (unitsInDegrees) {
            crsAngle = Math.toRadians(crsAngle);
          }
        }

        if (shape == Shape.ELLIPSE) {
          // get the first boundary vector by rotating boresight by refAngle towards refVector
          VectorIJK axis = VectorIJK.cross(boresight, refVector);

          AxisAndAngle aaa = new AxisAndAngle(axis, refAngle);
          bounds.add(aaa.getRotation(new RotationMatrixIJK()).mxv(boresight));

          VectorIJK axis2 = VectorIJK.cross(boresight, axis);
          aaa = new AxisAndAngle(axis2, crsAngle);
          bounds.add(aaa.getRotation(new RotationMatrixIJK()).mxv(boresight));

        } else if (shape == Shape.RECTANGLE) {
          UnwritableVectorIJK b = boresight.createUnitized();
          Plane p = new Plane(b, VectorIJK.ZERO);
          VectorIJK b1 = p.projectOnto(refVector).unitize();
          VectorIJK b2 = VectorIJK.cross(b, b1);

          double cosRef = Math.cos(refAngle);
          double sinRef = Math.sin(refAngle);
          double cosCrs = Math.cos(crsAngle);
          double sinCrs = Math.sin(crsAngle);

          VectorIJK normal1 = VectorIJK.combine(-cosRef, b1, sinRef, b);
          VectorIJK normal2 = VectorIJK.combine(-cosCrs, b2, sinCrs, b);
          VectorIJK normal3 = VectorIJK.combine(cosRef, b1, sinRef, b);
          VectorIJK normal4 = VectorIJK.combine(cosCrs, b2, sinCrs, b);

          bounds.add(VectorIJK.cross(normal1, normal2));
          bounds.add(VectorIJK.cross(normal2, normal3));
          bounds.add(VectorIJK.cross(normal3, normal4));
          bounds.add(VectorIJK.cross(normal4, normal1));
        }
      }
      FOVSpice fovSpice = new FOVSpice(instrID, shape, frame, boresight, bounds);
      return new FOV(frame, fovSpice);
    }
    return null;
  }

  /*-
  SPICE Example:
  
  
   KPL/IK
  
   The keywords below define a circular, 10-degree wide FOV with
   the boresight along the +Z axis of the 'SC999_INST001' frame
   for an instrument with ID -999001 using the "angles"-class
   specification.
  
   \begindata
      INS-999001_FOV_CLASS_SPEC       = 'ANGLES'
      INS-999001_FOV_SHAPE            = 'CIRCLE'
      INS-999001_FOV_FRAME            = 'SC999_INST001'
      INS-999001_BORESIGHT            = ( 0.0, 0.0, 1.0 )
      INS-999001_FOV_REF_VECTOR       = ( 1.0, 0.0, 0.0 )
      INS-999001_FOV_REF_ANGLE        = ( 5.0 )
      INS-999001_FOV_ANGLE_UNITS      = ( 'DEGREES' )
   \begintext
  
   The keywords below define an elliptical FOV with 2- and
   4-degree angular extents in the XZ and XY planes and the
   boresight along the +X axis of the 'SC999_INST002' frame for
   an instrument with ID -999002 using the "corners"-class
   specification.
  
   \begindata
      INS-999002_FOV_SHAPE            = 'ELLIPSE'
      INS-999002_FOV_FRAME            = 'SC999_INST002'
      INS-999002_BORESIGHT            = ( 1.0, 0.0, 0.0 )
      INS-999002_FOV_BOUNDARY_CORNERS = ( 1.0, 0.0, 0.01745506,
                                          1.0, 0.03492077, 0.0 )
   \begintext
  
   The keywords below define a rectangular FOV with 1.2- and
   0.2-degree angular extents in the ZX and ZY planes and the
   boresight along the +Z axis of the 'SC999_INST003' frame for
   an instrument with ID -999003 using the "angles"-class
   specification.
  
   \begindata
      INS-999003_FOV_CLASS_SPEC       = 'ANGLES'
      INS-999003_FOV_SHAPE            = 'RECTANGLE'
      INS-999003_FOV_FRAME            = 'SC999_INST003'
      INS-999003_BORESIGHT            = ( 0.0, 0.0, 1.0 )
      INS-999003_FOV_REF_VECTOR       = ( 1.0, 0.0, 0.0 )
      INS-999003_FOV_REF_ANGLE        = ( 0.6 )
      INS-999003_FOV_CROSS_ANGLE      = ( 0.1 )
      INS-999003_FOV_ANGLE_UNITS      = ( 'DEGREES' )
   \begintext
  
   The keywords below define a triangular FOV with the boresight
   along the +Y axis of the 'SC999_INST004' frame for an
   instrument with ID -999004 using the "corners"-class
   specification.
  
   \begindata
      INS-999004_FOV_SHAPE            = 'POLYGON'
      INS-999004_FOV_FRAME            = 'SC999_INST004'
      INS-999004_BORESIGHT            = (  0.0,  1.0,  0.0 )
      INS-999004_FOV_BOUNDARY_CORNERS = (  0.0,  0.8,  0.5,
                                           0.4,  0.8, -0.2,
                                          -0.4,  0.8, -0.2 )
   \begintext
  
  Output of this program is 
  
  Instrument ID: -999001
    FOV shape: CIRCLE
    FOV frame: SC999_INST001
  FOV boresight: [0.0,0.0,1.0]
  FOV corners:
               [0.08715574274765814,0.0,0.9961946980917455]
  
  Instrument ID: -999002
    FOV shape: ELLIPSE
    FOV frame: SC999_INST002
  FOV boresight: [1.0,0.0,0.0]
  FOV corners:
               [1.0,0.0,0.01745506]
               [1.0,0.03492077,0.0]
  
  Instrument ID: -999003
    FOV shape: RECTANGLE
    FOV frame: SC999_INST003
  FOV boresight: [0.0,0.0,1.0]
  FOV corners:
               [0.01047176816681055,0.001745232668436617,0.9999436463623117]
               [-0.01047176816681055,0.001745232668436617,0.9999436463623117]
               [-0.01047176816681055,-0.001745232668436617,0.9999436463623117]
               [0.01047176816681055,-0.001745232668436617,0.9999436463623117]
  
  Instrument ID: -999004
    FOV shape: POLYGON
    FOV frame: SC999_INST004
  FOV boresight: [0.0,1.0,0.0]
  FOV corners:
               [0.0,0.8,0.5]
               [0.4,0.8,-0.2]
               [-0.4,0.8,-0.2]
               
               
  Compare with SPICE getfov() documentation.
   */
  public static void main(String[] args)
      throws ParseException, KernelInstantiationException, IOException {

    SpiceEnvironmentBuilder builder = new SpiceEnvironmentBuilder();
    builder.load("IK", new File("/Users/nairah1/local/src/spice/example.ti"));
    builder.load("LSK", new File("/project/spice/lsk/naif0012.tls"));

    FOVFactory factory = new FOVFactory(builder.build().getPool());
    /*-
    // alternatively
    TextKernelParser parser = new TextKernelParser();
    KernelPool kernelPool =
        new KernelPool(parser.parse(new FileReader("/Users/nairah1/local/src/spice/example.ti")));
    FOVFactory factory = new FOVFactory(kernelPool);
    */

    for (int i = 1; i <= 4; i++) {
      FOV fov = factory.create(-999000 - i);
      System.out.println("SPICE check:");
      System.out.println(fov.getFovSpice().toString());

      System.out.println(
          "Cone check (note boundary vectors are unitized so magnitudes may not agree with SPICE boundary vectors):");
      System.out.printf("vertex %s\n", fov.getCone().getVertex());
      UnwritableInterval domain = fov.getCone().getParameterDomain();
      System.out.printf("parameter domain %s\n", domain);
      for (int j = 0; j < fov.getFovSpice().getBounds().size(); j++) {
        double parameter =
            domain.getBegin() + domain.getLength() / fov.getFovSpice().getBounds().size() * j;
        if (fov.getFovSpice().getShape() == Shape.ELLIPSE) {
          parameter /= 2;
        }
        System.out.printf("boundary %d (%f) %s\n", j, parameter, fov.getCone().getEdge(parameter));
      }
      System.out.println();
    }
  }
}
