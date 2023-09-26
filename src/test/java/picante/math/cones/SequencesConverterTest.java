package picante.math.cones;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static picante.math.cones.JtsUtilities.createPackedCoordinateSequence;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.CoordinateSequence;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LineString;
import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.PrecisionModel;
import org.locationtech.jts.geom.impl.PackedCoordinateSequenceFactory;
import com.google.common.collect.ImmutableList;

public class SequencesConverterTest {

  private SequencesConverter converter;

  @Before
  public void setUp() throws Exception {
    converter =
        new SequencesConverter(new GeometryFactory(new PrecisionModel(PrecisionModel.FLOATING), 0,
            PackedCoordinateSequenceFactory.DOUBLE_FACTORY));
  }

  @Test
  public void testApplyPoint() {

    List<CoordinateSequence> sequences = ImmutableList.of(createPackedCoordinateSequence(0, 0),
        createPackedCoordinateSequence(1, 1));
    List<Geometry> result = converter.apply(sequences);

    testSequences(sequences, result);
    assertEquals(Point.class, result.get(0).getClass());
    assertEquals(Point.class, result.get(1).getClass());

  }

  @Test
  public void testApplyLineString() {

    List<CoordinateSequence> sequences =
        ImmutableList.of(createPackedCoordinateSequence(0, 0, 1, 1, 2, 2),
            createPackedCoordinateSequence(3, 3, 4, 4));
    List<Geometry> result = converter.apply(sequences);

    testSequences(sequences, result);
    assertEquals(LineString.class, result.get(0).getClass());
    assertEquals(LineString.class, result.get(1).getClass());

  }

  @Test
  public void testApplyClosedLineStrings() {

    List<CoordinateSequence> sequences =
        ImmutableList.of(createPackedCoordinateSequence(0, 0, 1, 1, 2, 1, 0, 0),
            createPackedCoordinateSequence(3, 3, 4, 4, 5, 4, 3, 3));
    List<Geometry> result = converter.apply(sequences);

    testSequences(sequences, result);
    assertEquals(LinearRing.class, result.get(0).getClass());
    assertEquals(LinearRing.class, result.get(0).getClass());

  }

  @Test
  public void testApplyMixed() {

    List<CoordinateSequence> sequences = ImmutableList.of(createPackedCoordinateSequence(0, 0),
        createPackedCoordinateSequence(1, 1, 2, 2, 3, 3),
        createPackedCoordinateSequence(4, 4, 5, 5, 6, 4, 4, 3),
        createPackedCoordinateSequence(8, 8, 9, 9, 10, 8, 8, 8));
    List<Geometry> result = converter.apply(sequences);

    testSequences(sequences, result);
    assertEquals(Point.class, result.get(0).getClass());
    assertEquals(LineString.class, result.get(1).getClass());
    assertEquals(LineString.class, result.get(2).getClass());
    assertEquals(LinearRing.class, result.get(3).getClass());

  }

  void testSequences(List<CoordinateSequence> sequences, List<Geometry> geometries) {

    assertTrue(sequences.size() == geometries.size());

    for (int i = 0; i < sequences.size(); i++) {
      assertArrayEquals(sequences.get(i).toCoordinateArray(), geometries.get(i).getCoordinates());
    }

  }

  @Test
  public void testIsLinearRing() {

    Coordinate a = new Coordinate();
    Coordinate b = new Coordinate();
    assertFalse(converter.isLinearRing(createPackedCoordinateSequence(0, 0), a, b));
    assertFalse(converter.isLinearRing(createPackedCoordinateSequence(0, 0, 1, 1), a, b));
    assertFalse(converter.isLinearRing(createPackedCoordinateSequence(0, 0, 1, 1, 2, 2), a, b));
    assertFalse(
        converter.isLinearRing(createPackedCoordinateSequence(0, 0, 1, 1, 2, 2, 3, 3), a, b));
    assertTrue(
        converter.isLinearRing(createPackedCoordinateSequence(0, 0, 1, 1, 2, 2, 0, 0), a, b));

  }

}
