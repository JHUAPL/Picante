package picante.math.cones;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.CoordinateXY;
import org.locationtech.jts.geom.CoordinateXYZM;
import org.locationtech.jts.geom.impl.PackedCoordinateSequence;

/**
 * Class enclosing two simple extensions of {@link PackedCoordinateSequence.Double} and
 * {@link PackedCoordinateSequence.Float}.
 * <p>
 * The purpose of this class is to work around the assignment problem, when a packed coordinate
 * sequence of higher dimension has been requested to set one of lower dimension. The current JTS
 * code fails, but this method will allow it--setting the higher dimensional component to
 * {@link java.lang.Double#NaN}.
 * </p>
 */
class ModifiedPackedCoordinateSequence {

  public static class Double extends PackedCoordinateSequence.Double {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /**
     * Safely converts an array of {@link Coordinate} to a double array, if the dimension of all
     * input coordinates is less than that of the data store.
     * 
     * @param coordinates
     * @param dimension
     * 
     * @return
     */
    static double[] convert(Coordinate[] coordinates, int dimension) {
      if (coordinates == null) {
        coordinates = new Coordinate[0];
      }

      double[] coords = new double[coordinates.length * dimension];
      for (int i = 0; i < coordinates.length; i++) {
        coords[i * dimension] = coordinates[i].x;
        if (dimension >= 2) {
          coords[i * dimension + 1] = coordinates[i].y;
        }
        if (dimension >= 3) {
          coords[i * dimension + 2] = getOrdinate(coordinates[i], 2);
        }
        if (dimension >= 4) {
          coords[i * dimension + 3] = getOrdinate(coordinates[i], 3);
        }
      }
      return coords;

    }

    /**
     * Retrieves the ordinate of the supplied coordinate, or NaN if it can not be provided.
     * 
     * @param coord
     * @param ordinate
     * 
     * @return the value at ordinate in coord, or {@link java.lang.Double#NaN} if not available
     */
    static double getOrdinate(Coordinate coord, int ordinate) {

      if (coord instanceof CoordinateXY) {
        return java.lang.Double.NaN;
      }

      if (ordinate == 2) {
        return coord.getOrdinate(2);
      }

      if (ordinate == 3) {
        if (coord instanceof CoordinateXYZM) {
          return coord.getOrdinate(3);
        }
      }

      return java.lang.Double.NaN;

    }

    Double(Coordinate[] coordinates, int dimension, int measures) {
      this(convert(coordinates, dimension), dimension, measures);
    }

    Double(Coordinate[] coordinates, int dimension) {
      this(coordinates, dimension, 0);
    }

    Double(Coordinate[] coordinates) {
      this(coordinates, 3);
    }

    Double(double[] coords, int dimension, int measures) {
      super(coords, dimension, measures);
    }

    Double(float[] coordinates, int dimension, int measures) {
      super(coordinates, dimension, measures);
    }

    Double(int size, int dimension, int measures) {
      super(size, dimension, measures);
    }



  }

  public static class Float extends PackedCoordinateSequence.Float {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /**
     * Safely converts an array of {@link Coordinate} to a float array, if the dimension of all
     * input coordinates is less than that of the data store.
     * 
     * @param coordinates
     * @param dimension
     * 
     * @return
     */
    static float[] convert(Coordinate[] coordinates, int dimension) {
      if (coordinates == null) {
        coordinates = new Coordinate[0];
      }

      float[] coords = new float[coordinates.length * dimension];
      for (int i = 0; i < coordinates.length; i++) {
        coords[i * dimension] = (float) coordinates[i].x;
        if (dimension >= 2) {
          coords[i * dimension + 1] = (float) coordinates[i].y;
        }
        if (dimension >= 3) {
          coords[i * dimension + 2] = getOrdinate(coordinates[i], 2);
        }
        if (dimension >= 4) {
          coords[i * dimension + 3] = getOrdinate(coordinates[i], 3);
        }
      }
      return coords;

    }

    /**
     * Retrieves the ordinate of the supplied coordinate, or NaN if it can not be provided.
     * 
     * @param coord
     * @param ordinate
     * 
     * @return the value at ordinate in coord cast to a float, or {@link java.lang.Float#NaN} if not
     *         available
     */
    static float getOrdinate(Coordinate coord, int ordinate) {

      if (coord instanceof CoordinateXY) {
        return java.lang.Float.NaN;
      }

      if (ordinate == 2) {
        return (float) coord.getOrdinate(2);
      }

      if (ordinate == 3) {
        if (coord instanceof CoordinateXYZM) {
          return (float) coord.getOrdinate(3);
        }
      }

      return java.lang.Float.NaN;

    }


    Float(Coordinate[] coordinates, int dimension, int measures) {
      this(convert(coordinates, dimension), dimension, measures);
    }

    Float(Coordinate[] coordinates, int dimension) {
      this(coordinates, dimension, 0);
    }

    Float(Coordinate[] coordinates) {
      this(coordinates, 3);
    }

    Float(double[] coords, int dimension, int measures) {
      super(coords, dimension, measures);
    }

    Float(float[] coordinates, int dimension, int measures) {
      super(coordinates, dimension, measures);
    }

    Float(int size, int dimension, int measures) {
      super(size, dimension, measures);
    }



  }

}
