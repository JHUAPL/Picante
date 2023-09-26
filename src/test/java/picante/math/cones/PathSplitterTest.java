package picante.math.cones;

import static org.junit.Assert.assertEquals;
import java.awt.geom.Path2D;
import java.awt.geom.PathIterator;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.CoordinateSequence;
import org.locationtech.jts.geom.impl.PackedCoordinateSequenceFactory;
import com.google.common.collect.ImmutableList;

public class PathSplitterTest {

  private PathSplitter converter;
  private Path2D.Double path;

  @Before
  public void setUp() throws Exception {
    converter = new PathSplitter(PackedCoordinateSequenceFactory.DOUBLE_FACTORY);
    path = new Path2D.Double();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateIndividualPathsUnsupportedOperationExceptionQuadTo() {
    path.moveTo(0, 0);
    path.quadTo(1, 1, 2, 2);
    converter.apply(path.getPathIterator(null));
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateIndividualPathsUnsupportedOperationExceptionCubicTo() {
    path.moveTo(0, 0);
    path.curveTo(1, 1, 2, 2, 3, 3);
    converter.apply(path.getPathIterator(null));
  }

  @Test
  public void testCreateIndividualPathsOnePointMoveTo() {

    path.moveTo(0, 0);

    List<CoordinateSequence> sequences = converter.apply(path.getPathIterator(null));

    assertEquals(1, sequences.size());
    CoordinateSequence sequence = sequences.get(0);

    assertEquals(1, sequence.size());
    assertEquals(new Coordinate(0, 0), sequence.getCoordinate(0));

  }

  @Test
  public void testCreateIndividualPathsMultiPointLineToThenMoveTo() {

    path.moveTo(0, 0);
    path.lineTo(1, 1);
    path.moveTo(2, 2);

    List<CoordinateSequence> sequences = converter.apply(path.getPathIterator(null));

    assertEquals(2, sequences.size());

    CoordinateSequence sequence = sequences.get(0);
    assertEquals(2, sequence.size());
    assertEquals(new Coordinate(0, 0), sequence.getCoordinate(0));
    assertEquals(new Coordinate(1, 1), sequence.getCoordinate(1));

    sequence = sequences.get(1);
    assertEquals(1, sequence.size());
    assertEquals(new Coordinate(2, 2), sequence.getCoordinate(0));

  }


  @Test
  public void testCreateIndividualPathsSingleClosedPath() {

    path.moveTo(0, 0);
    path.lineTo(1, 0);
    path.lineTo(0.5, 2);
    path.closePath();

    List<CoordinateSequence> sequences = converter.apply(path.getPathIterator(null));

    assertEquals(1, sequences.size());

    CoordinateSequence sequence = sequences.get(0);
    assertEquals(4, sequence.size());
    assertEquals(new Coordinate(0, 0), sequence.getCoordinate(0));
    assertEquals(new Coordinate(1, 0), sequence.getCoordinate(1));
    assertEquals(new Coordinate(0.5, 2), sequence.getCoordinate(2));
    assertEquals(new Coordinate(0, 0), sequence.getCoordinate(3));

  }

  @Test
  public void testCreateIndividualPathsMultiClosedPaths() {

    path.moveTo(0, 0);
    path.lineTo(1, 0);
    path.lineTo(0.5, 2);
    path.closePath();

    path.moveTo(10, 10);
    path.lineTo(11, 10);
    path.lineTo(10.5, 12);
    path.closePath();

    path.moveTo(20, 20);
    path.lineTo(21, 20);
    path.lineTo(20.5, 22);
    path.closePath();

    List<CoordinateSequence> sequences = converter.apply(path.getPathIterator(null));

    assertEquals(3, sequences.size());

    for (int i = 0; i < 3; i++) {
      CoordinateSequence sequence = sequences.get(i);
      assertEquals(4, sequence.size());
      assertEquals(new Coordinate(10 * i + 0, 10 * i + 0), sequence.getCoordinate(0));
      assertEquals(new Coordinate(10 * i + 1, 10 * i + 0), sequence.getCoordinate(1));
      assertEquals(new Coordinate(10 * i + 0.5, 10 * i + 2), sequence.getCoordinate(2));
      assertEquals(new Coordinate(10 * i + 0, 10 * i + 0), sequence.getCoordinate(3));
    }

  }

  @Test
  public void testCreateIndividualPathsMixedClosedOpenPaths() {

    path.moveTo(0, 0);
    path.lineTo(1, 0);
    path.lineTo(0.5, 2);
    path.lineTo(0, 0);
    path.closePath();

    path.moveTo(10, 10);
    path.lineTo(11, 11);

    List<CoordinateSequence> sequences = converter.apply(path.getPathIterator(null));

    assertEquals(2, sequences.size());

    CoordinateSequence sequence = sequences.get(0);
    assertEquals(4, sequence.size());
    assertEquals(new Coordinate(0, 0), sequence.getCoordinate(0));
    assertEquals(new Coordinate(1, 0), sequence.getCoordinate(1));
    assertEquals(new Coordinate(0.5, 2), sequence.getCoordinate(2));
    assertEquals(new Coordinate(0, 0), sequence.getCoordinate(3));

    sequence = sequences.get(1);
    assertEquals(2, sequence.size());
    assertEquals(new Coordinate(10, 10), sequence.getCoordinate(0));
    assertEquals(new Coordinate(11, 11), sequence.getCoordinate(1));

  }

  @Test
  public void testRemoveDuplicateLineTo() {

    path.moveTo(0, 0);
    path.lineTo(1, 1);
    path.lineTo(1, 1);
    path.lineTo(1, 2);
    path.lineTo(1, 2);
    path.closePath();

    List<CoordinateSequence> sequences = converter.apply(path.getPathIterator(null));

    assertEquals(1, sequences.size());

    CoordinateSequence sequence = sequences.get(0);
    assertEquals(4, sequence.size());
    assertEquals(new Coordinate(0, 0), sequence.getCoordinate(0));
    assertEquals(new Coordinate(1, 1), sequence.getCoordinate(1));
    assertEquals(new Coordinate(1, 2), sequence.getCoordinate(2));
    assertEquals(new Coordinate(0, 0), sequence.getCoordinate(3));

  }

  @Test
  public void testRemoveDuplicateMoveTo() {

    List<Integer> types = ImmutableList.of(PathIterator.SEG_MOVETO, PathIterator.SEG_MOVETO,
        PathIterator.SEG_LINETO, PathIterator.SEG_MOVETO, PathIterator.SEG_MOVETO);
    List<double[]> coords = ImmutableList.of(new double[] {0, 0}, new double[] {0, 0},
        new double[] {1, 1}, new double[] {2, 2}, new double[] {2, 2});

    BasicPathIterator iterator = new BasicPathIterator(types, coords);

    List<CoordinateSequence> sequences = converter.apply(iterator);

    assertEquals(2, sequences.size());

    CoordinateSequence sequence = sequences.get(0);
    assertEquals(2, sequence.size());
    assertEquals(new Coordinate(0, 0), sequence.getCoordinate(0));
    assertEquals(new Coordinate(1, 1), sequence.getCoordinate(1));

    sequence = sequences.get(1);
    assertEquals(1, sequence.size());
    assertEquals(new Coordinate(2, 2), sequence.getCoordinate(0));

  }

  @Test(expected = IllegalArgumentException.class)
  public void testInvalidFirstType() {

    List<Integer> types = ImmutableList.of(PathIterator.SEG_LINETO);
    List<double[]> coords = ImmutableList.of(new double[] {0, 0});

    BasicPathIterator iterator = new BasicPathIterator(types, coords);

    converter.apply(iterator);

  }

}
