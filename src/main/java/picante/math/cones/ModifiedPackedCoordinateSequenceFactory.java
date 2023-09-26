package picante.math.cones;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.CoordinateSequence;
import org.locationtech.jts.geom.Coordinates;
import org.locationtech.jts.geom.impl.PackedCoordinateSequenceFactory;

/**
 * Implementation of {@link PackedCoordinateSequenceFactory} that provides
 * {@link ModifiedPackedCoordinateSequence}s
 */
class ModifiedPackedCoordinateSequenceFactory extends PackedCoordinateSequenceFactory {

  private static final long serialVersionUID = 1L;

  public static final int DOUBLE = 0;
  public static final int FLOAT = 1;

  public static final ModifiedPackedCoordinateSequenceFactory DOUBLE_FACTORY =
      new ModifiedPackedCoordinateSequenceFactory(DOUBLE);

  public static final ModifiedPackedCoordinateSequenceFactory FLOAT_FACTORY =
      new ModifiedPackedCoordinateSequenceFactory(FLOAT);

  private int type = DOUBLE;

  /**
   * Creates a new PackedCoordinateSequenceFactory of type DOUBLE.
   */
  public ModifiedPackedCoordinateSequenceFactory() {
    this(DOUBLE);
  }

  /**
   * Creates a new PackedCoordinateSequenceFactory of the given type. Acceptable type values are
   * {@linkplain PackedCoordinateSequenceFactory#Float}or
   * {@linkplain PackedCoordinateSequenceFactory#Double}
   */
  public ModifiedPackedCoordinateSequenceFactory(int type) {
    this.type = type;
  }

  @Override
  public int getType() {
    return super.getType();
  }

  @Override
  public CoordinateSequence create(Coordinate[] coordinates) {
    int dimension = 3;
    int measures = 0;
    if (coordinates != null && coordinates.length > 1 && coordinates[0] != null) {
      Coordinate first = coordinates[0];
      dimension = Coordinates.dimension(first);
      measures = Coordinates.measures(first);
    }
    if (type == DOUBLE) {
      return new ModifiedPackedCoordinateSequence.Double(coordinates, dimension, measures);
    } else {
      return new ModifiedPackedCoordinateSequence.Float(coordinates, dimension, measures);
    }
  }

  @Override
  public CoordinateSequence create(CoordinateSequence coordSeq) {
    int dimension = coordSeq.getDimension();
    int measures = coordSeq.getMeasures();
    if (type == DOUBLE) {
      return new ModifiedPackedCoordinateSequence.Double(coordSeq.toCoordinateArray(), dimension,
          measures);
    } else {
      return new ModifiedPackedCoordinateSequence.Float(coordSeq.toCoordinateArray(), dimension,
          measures);
    }
  }

  @Override
  public CoordinateSequence create(double[] packedCoordinates, int dimension) {
    return create(packedCoordinates, dimension, 0);
  }

  @Override
  public CoordinateSequence create(double[] packedCoordinates, int dimension, int measures) {
    if (type == DOUBLE) {
      return new ModifiedPackedCoordinateSequence.Double(packedCoordinates, dimension, measures);
    } else {
      return new ModifiedPackedCoordinateSequence.Float(packedCoordinates, dimension, measures);
    }
  }

  @Override
  public CoordinateSequence create(float[] packedCoordinates, int dimension) {
    return create(packedCoordinates, dimension, 0);
  }

  @Override
  public CoordinateSequence create(float[] packedCoordinates, int dimension, int measures) {
    if (type == DOUBLE) {
      return new ModifiedPackedCoordinateSequence.Double(packedCoordinates, dimension, measures);
    } else {
      return new ModifiedPackedCoordinateSequence.Float(packedCoordinates, dimension, measures);
    }
  }

  @Override
  public CoordinateSequence create(int size, int dimension) {
    if (type == DOUBLE) {
      return new ModifiedPackedCoordinateSequence.Double(size, dimension, 0);
    } else {
      return new ModifiedPackedCoordinateSequence.Float(size, dimension, 0);
    }
  }

  @Override
  public CoordinateSequence create(int size, int dimension, int measures) {
    if (type == DOUBLE) {
      return new ModifiedPackedCoordinateSequence.Double(size, dimension, measures);
    } else {
      return new ModifiedPackedCoordinateSequence.Float(size, dimension, measures);
    }
  }



}
