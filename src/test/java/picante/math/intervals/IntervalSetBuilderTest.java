package picante.math.intervals;

import static com.google.common.base.Preconditions.checkArgument;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static picante.math.intervals.IntervalSet.EMPTY;
import static picante.math.intervals.IntervalSet.LINE;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import com.google.common.base.Predicate;
import com.google.common.collect.Lists;
import picante.math.intervals.IntervalSet.Builder.Content;

public class IntervalSetBuilderTest {

  private IntervalSet.Builder builder;

  private Content intervalListNoSingletons;
  private Content intervalList;
  private Content emptyList;

  private List<UnwritableInterval> disjointIntervals;
  private List<UnwritableInterval> intervals;
  private List<UnwritableInterval> intervalsReduced;
  private List<UnwritableInterval> disjointAndIntervalsMerged;
  private List<UnwritableInterval> variedGaps;

  @Before
  public void setUp() throws Exception {
    builder = new IntervalSet.Builder(1);
    intervalListNoSingletons = new Content();
    intervalListNoSingletons.array = new double[] {1.0, 2.0, 3.0, 4.0, 5.0, 6.0};
    intervalListNoSingletons.committed = intervalListNoSingletons.array.length;
    intervalList = new Content();
    intervalList.array = new double[] {1.0, 2.0, 3.0, 3.0, 4.0, 4.0, 5.0, 6.0};
    intervalList.committed = intervalList.array.length;
    emptyList = new Content();
    emptyList.array = new double[0];
    emptyList.committed = emptyList.array.length;

    disjointIntervals = Lists.newArrayList(new UnwritableInterval(10, 20),
        new UnwritableInterval(30, 40), new UnwritableInterval(50, 60));
    intervals = Lists.newArrayList(new UnwritableInterval(10, 20), new UnwritableInterval(30, 40),
        new UnwritableInterval(15, 35), new UnwritableInterval(40, 45));
    intervalsReduced = Lists.newArrayList(new UnwritableInterval(10, 45));
    disjointAndIntervalsMerged =
        Lists.newArrayList(new UnwritableInterval(10, 45), new UnwritableInterval(50, 60));
    variedGaps = Lists.newArrayList(new UnwritableInterval(-10, 0), new UnwritableInterval(10, 20),
        new UnwritableInterval(31, 40), new UnwritableInterval(52, 60));
  }

  private static IntervalSet create(double... startStops) {
    checkArgument(startStops.length % 2 == 0);

    List<UnwritableInterval> list = Lists.newArrayList();
    for (int i = 0; i < startStops.length; i += 2) {
      list.add(new UnwritableInterval(startStops[i], startStops[i + 1]));
    }

    return IntervalSet.builder().addAll(list).build();
  }

  private static IntervalSet create(List<UnwritableInterval> intervals) {
    return IntervalSet.builder().addAll(intervals).build();
  }

  @Test
  public void testIndexOfIntervalEndJustAfter() {

    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(emptyList, -1.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(emptyList, -1.0));

    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalList, -1.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalList, -1.0));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalList, 1.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalList, 1.0));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalList, 1.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalList, 1.5));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalList, 2.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalList, 2.0));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalList, 2.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalList, 2.5));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalList, 3.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalList, 3.0));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalList, 3.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalList, 3.5));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalList, 4.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalList, 4.0));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalList, 4.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalList, 4.5));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalList, 5.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalList, 5.0));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalList, 5.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalList, 5.5));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalList, 6.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalList, 6.0));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalList, 7.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalList, 7.0));

    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, -1.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, -1.0));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 1.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 1.0));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 1.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 1.5));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 2.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 2.0));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 2.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 2.5));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 3.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 3.0));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 3.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 3.5));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 4.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 4.0));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 4.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 4.5));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 5.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 5.0));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 5.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 5.5));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 6.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 6.0));
    assertEquals(computeIndexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 7.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfterOrEqualTo(intervalListNoSingletons, 7.0));
  }

  private int computeIndexOfIntervalEndJustAfterOrEqualTo(Content intervalList, double value) {
    int index = 1;
    while ((index <= intervalList.array.length - 1) && (intervalList.array[index] < value)) {
      index += 2;
    }
    return index;
  }

  @Test
  public void testIndexOfIntervalEndJustAfterOrEqual() {

    assertEquals(indexOfIntervalEndJustAfter(emptyList, -1.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(emptyList, -1.0));

    assertEquals(indexOfIntervalEndJustAfter(intervalList, -1.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalList, -1.0));
    assertEquals(indexOfIntervalEndJustAfter(intervalList, 1.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalList, 1.0));
    assertEquals(indexOfIntervalEndJustAfter(intervalList, 1.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalList, 1.5));
    assertEquals(indexOfIntervalEndJustAfter(intervalList, 2.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalList, 2.0));
    assertEquals(indexOfIntervalEndJustAfter(intervalList, 2.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalList, 2.5));
    assertEquals(indexOfIntervalEndJustAfter(intervalList, 3.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalList, 3.0));
    assertEquals(indexOfIntervalEndJustAfter(intervalList, 3.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalList, 3.5));
    assertEquals(indexOfIntervalEndJustAfter(intervalList, 4.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalList, 4.0));
    assertEquals(indexOfIntervalEndJustAfter(intervalList, 4.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalList, 4.5));
    assertEquals(indexOfIntervalEndJustAfter(intervalList, 5.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalList, 5.0));
    assertEquals(indexOfIntervalEndJustAfter(intervalList, 5.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalList, 5.5));
    assertEquals(indexOfIntervalEndJustAfter(intervalList, 6.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalList, 6.0));
    assertEquals(indexOfIntervalEndJustAfter(intervalList, 7.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalList, 7.0));

    assertEquals(indexOfIntervalEndJustAfter(intervalListNoSingletons, -1.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalListNoSingletons, -1.0));
    assertEquals(indexOfIntervalEndJustAfter(intervalListNoSingletons, 1.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalListNoSingletons, 1.0));
    assertEquals(indexOfIntervalEndJustAfter(intervalListNoSingletons, 1.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalListNoSingletons, 1.5));
    assertEquals(indexOfIntervalEndJustAfter(intervalListNoSingletons, 2.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalListNoSingletons, 2.0));
    assertEquals(indexOfIntervalEndJustAfter(intervalListNoSingletons, 2.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalListNoSingletons, 2.5));
    assertEquals(indexOfIntervalEndJustAfter(intervalListNoSingletons, 3.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalListNoSingletons, 3.0));
    assertEquals(indexOfIntervalEndJustAfter(intervalListNoSingletons, 3.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalListNoSingletons, 3.5));
    assertEquals(indexOfIntervalEndJustAfter(intervalListNoSingletons, 4.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalListNoSingletons, 4.0));
    assertEquals(indexOfIntervalEndJustAfter(intervalListNoSingletons, 4.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalListNoSingletons, 4.5));
    assertEquals(indexOfIntervalEndJustAfter(intervalListNoSingletons, 5.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalListNoSingletons, 5.0));
    assertEquals(indexOfIntervalEndJustAfter(intervalListNoSingletons, 5.5),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalListNoSingletons, 5.5));
    assertEquals(indexOfIntervalEndJustAfter(intervalListNoSingletons, 6.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalListNoSingletons, 6.0));
    assertEquals(indexOfIntervalEndJustAfter(intervalListNoSingletons, 7.0),
        IntervalSet.Builder.indexOfIntervalEndJustAfter(intervalListNoSingletons, 7.0));
  }

  private int indexOfIntervalEndJustAfter(Content intervalList, double value) {
    int index = 1;
    while ((index <= intervalList.array.length - 1) && (intervalList.array[index] <= value)) {
      index += 2;
    }
    return index;
  }

  @Test
  public void testSetToIntervalSet() {
    IntervalSet.Builder otherBuilder = new IntervalSet.Builder();
    IntervalSet set = otherBuilder.add(10, 20).add(30, 40).add(50, 60).build();

    assertEquals(create(disjointIntervals), builder.setTo(set).build());

    /*
     * Verify it dumps the contents first.
     */
    builder.empty();
    builder.add(-10, 20);

    assertEquals(create(disjointIntervals), builder.setTo(set).build());
  }

  @Test
  public void testSetToIterableUnwritableInterval() {
    builder.setTo(disjointIntervals);
    assertEquals(create(disjointIntervals), builder.build());

    builder.setTo(intervals);
    assertEquals(create(intervalsReduced), builder.build());
  }

  @Test
  public void testEmpty() {
    assertEquals(EMPTY, builder.build());
    assertEquals(EMPTY, builder.add(10, 20).empty().build());
  }

  @Test
  public void testAddAll() {
    builder.addAll(intervals);
    assertEquals(create(intervalsReduced), builder.build());

    builder.addAll(new ArrayList<UnwritableInterval>());
    assertEquals(create(intervalsReduced), builder.build());

    builder.addAll(disjointIntervals);
    assertEquals(create(disjointAndIntervalsMerged), builder.build());
  }

  @Test
  public void testUnion() {
    builder.union(EMPTY);
    assertEquals(EMPTY, builder.build());

    builder.empty().union(EMPTY);
    assertEquals(EMPTY, builder.build());

    builder.empty().addAll(create(10, 20, 30, 40)).union(EMPTY);
    assertEquals(create(10, 20, 30, 40), builder.build());

    builder.empty().union(create(10, 20, 30, 40));
    assertEquals(create(10, 20, 30, 40), builder.build());

    builder.empty().addAll(create(0, 100)).union(create(1, 2, 3, 4, 5, 6, 7, 8));
    assertEquals(create(0, 100), builder.build());

    builder.empty().addAll(create(1, 2, 3, 4, 5, 6, 7, 8)).union(create(0, 100));
    assertEquals(create(0, 100), builder.build());

    builder.empty().addAll(create(0, 10, 90, 100)).union(create(20, 30, 40, 50));
    assertEquals(create(0, 10, 20, 30, 40, 50, 90, 100), builder.build());

    builder.empty().addAll(create(0, 10)).union(create(-5, 1, 9, 15));
    assertEquals(create(-5, 15), builder.build());

    builder.empty().addAll(create(-5, 1, 9, 15)).union(create(0, 10));
    assertEquals(create(-5, 15), builder.build());
  }

  @Test
  public void testUnionInterval() {
    builder.empty().union(new Interval(10, 20));
    assertEquals(create(10, 20), builder.build());
  }


  @Test
  public void testIntersect() {
    builder.intersect(EMPTY);
    assertEquals(EMPTY, builder.build());

    builder.empty().intersect(EMPTY);
    assertEquals(EMPTY, builder.build());

    builder.empty().addAll(create(10, 20, 30, 40)).intersect(EMPTY);
    assertEquals(EMPTY, builder.build());

    builder.empty().intersect(create(10, 20, 30, 40));
    assertEquals(EMPTY, builder.build());

    builder.empty().addAll(create(0, 100)).intersect(create(1, 2, 3, 4, 5, 6, 7, 8));
    assertEquals(create(1, 2, 3, 4, 5, 6, 7, 8), builder.build());

    builder.empty().addAll(create(1, 2, 3, 4, 5, 6, 7, 8)).intersect(create(0, 100));
    assertEquals(create(1, 2, 3, 4, 5, 6, 7, 8), builder.build());

    builder.empty().addAll(create(0, 10, 90, 100)).intersect(create(20, 30, 40, 50));
    assertEquals(EMPTY, builder.build());

    builder.empty().addAll(create(0, 10)).intersect(create(-5, 1, 9, 15));
    assertEquals(create(0, 1, 9, 10), builder.build());

    builder.empty().addAll(create(-5, 1, 9, 15)).intersect(create(0, 10));
    assertEquals(create(0, 1, 9, 10), builder.build());
  }

  @Test
  public void testIntersectInterval() {
    builder.empty().addAll(create(0, 100)).intersect(new Interval(1, 2));
    assertEquals(create(1, 2), builder.build());
  }

  @Test
  public void testDifference() {

    builder.difference(EMPTY);
    assertEquals(EMPTY, builder.build());

    builder.empty().addAll(create(0, 10)).difference(EMPTY);
    assertEquals(create(0, 10), builder.build());

    builder.empty().difference(create(0, 10));
    assertEquals(EMPTY, builder.build());

    builder.empty().addAll(create(0, 100)).difference(create(2, 4, 6, 8));
    assertEquals(create(0, 2, 4, 6, 8, 100), builder.build());

    builder.empty().addAll(create(2, 4, 6, 8)).difference(create(0, 100));
    assertEquals(EMPTY, builder.build());

    builder.empty().addAll(create(0, 10)).difference(create(10, 30));
    assertEquals(create(0, 10), builder.build());

    builder.empty().addAll(create(10, 30)).difference(create(0, 10));
    assertEquals(create(10, 30), builder.build());

    builder.empty().addAll(create(0, 10)).difference(create(-10, 0));
    assertEquals(create(0, 10), builder.build());

    builder.empty().addAll(create(-10, 0)).difference(create(0, 10));
    assertEquals(create(-10, 0), builder.build());

    builder.empty().addAll(create(0, 10)).difference(create(-5, 5));
    assertEquals(create(5, 10), builder.build());

    builder.empty().addAll(create(-5, 5)).difference(create(0, 10));
    assertEquals(create(-5, 0), builder.build());

    builder.empty().addAll(create(0, 5, 10, 15, 20, 25, 30, 35))
        .difference(create(2, 3, 12, 13, 14, 15, 24, 25));
    assertEquals(create(0, 2, 3, 5, 10, 12, 13, 14, 20, 24, 30, 35), builder.build());

    builder.empty().addAll(create(0, 10, 15, 15, 20, 30)).difference(create(13, 16));
    assertEquals(create(0, 10, 20, 30), builder.build());

    builder.empty().addAll(create(0, 10, 20, 30)).difference(create(15, 15));
    assertEquals(create(0, 10, 20, 30), builder.build());

    builder.empty().addAll(create(0, 10, 20, 30)).difference(create(0, 0));
    assertEquals(create(0, 10, 20, 30), builder.build());

    builder.empty().addAll(create(0, 10, 20, 30)).difference(create(10, 10));
    assertEquals(create(0, 10, 20, 30), builder.build());

    builder.empty().addAll(create(15, 15)).difference(create(15, 15));
    assertEquals(EMPTY, builder.build());

  }

  @Test
  public void testDifferenceInterval() {
    builder.empty().addAll(create(-5, 5)).difference(new Interval(0, 10));
    assertEquals(create(-5, 0), builder.build());
  }

  @Test
  public void testRemoveFrom() {

    builder.removeFrom(EMPTY);
    assertEquals(EMPTY, builder.build());

    builder.empty().removeFrom(create(0, 10));
    assertEquals(create(0, 10), builder.build());

    builder.empty().addAll(create(0, 10)).removeFrom(EMPTY);
    assertEquals(EMPTY, builder.build());

    builder.empty().addAll(create(2, 4, 6, 8)).removeFrom(create(0, 100));
    assertEquals(create(0, 2, 4, 6, 8, 100), builder.build());

    builder.empty().addAll(create(0, 100)).removeFrom(create(2, 4, 6, 8));
    assertEquals(EMPTY, builder.build());

    builder.empty().addAll(create(10, 30)).removeFrom(create(0, 10));
    assertEquals(create(0, 10), builder.build());

    builder.empty().addAll(create(0, 10)).removeFrom(create(10, 30));
    assertEquals(create(10, 30), builder.build());

    builder.empty().addAll(create(-10, 0)).removeFrom(create(0, 10));
    assertEquals(create(0, 10), builder.build());

    builder.empty().addAll(create(0, 10)).removeFrom(create(-10, 0));
    assertEquals(create(-10, 0), builder.build());

    builder.empty().addAll(create(-5, 5)).removeFrom(create(0, 10));
    assertEquals(create(5, 10), builder.build());

    builder.empty().addAll(create(0, 10)).removeFrom(create(-5, 5));
    assertEquals(create(-5, 0), builder.build());

    builder.empty().addAll(create(2, 3, 12, 13, 14, 15, 24, 25))
        .removeFrom(create(0, 5, 10, 15, 20, 25, 30, 35));
    assertEquals(create(0, 2, 3, 5, 10, 12, 13, 14, 20, 24, 30, 35), builder.build());

    builder.empty().addAll(create(13, 16)).removeFrom(create(0, 10, 15, 15, 20, 30));
    assertEquals(create(0, 10, 20, 30), builder.build());

    builder.empty().addAll(create(15, 15)).removeFrom(create(0, 10, 20, 30));
    assertEquals(create(0, 10, 20, 30), builder.build());

    builder.empty().addAll(create(0, 0)).removeFrom(create(0, 10, 20, 30));
    assertEquals(create(0, 10, 20, 30), builder.build());

    builder.empty().addAll(create(10, 10)).removeFrom(create(0, 10, 20, 30));
    assertEquals(create(0, 10, 20, 30), builder.build());

    builder.empty().addAll(create(15, 15)).removeFrom(create(15, 15));
    assertEquals(EMPTY, builder.build());

  }

  @Test
  public void testFilter() {

    builder.addAll(disjointIntervals).filter(new Predicate<UnwritableInterval>() {

      @Override
      public boolean apply(@SuppressWarnings("unused") UnwritableInterval input) {
        return false;
      }
    });
    assertEquals(EMPTY, builder.build());

    builder.empty().addAll(disjointIntervals).filter(new Predicate<UnwritableInterval>() {

      @Override
      public boolean apply(UnwritableInterval input) {
        return !input.closedContains(34, 36);
      }
    });
    assertEquals(create(10, 20, 50, 60), builder.build());

  }

  @Test
  public void testComplement() {

    assertEquals(LINE, builder.empty().complement().build());
    assertEquals(EMPTY, builder.setTo(LINE).complement().build());

    assertEquals(create(-Double.MAX_VALUE, 10, 20, Double.MAX_VALUE),
        builder.empty().add(10, 20).complement().build());

    /*
     * Now some more complicated cases involving multiple intervals.
     */
    builder.empty().add(10, 20).add(30, 40).add(50, 60).complement();

    assertEquals(create(-Double.MAX_VALUE, 10, 20, 30, 40, 50, 60, Double.MAX_VALUE),
        builder.build());

  }

  @Test
  public void testComplementAgainstDoubleDouble() {

    /*
     * Test that the complement of the empty set is the entire line.
     */
    assertEquals(LINE,
        builder.empty().complementAgainst(-Double.MAX_VALUE, Double.MAX_VALUE).build());

    /*
     * And that the complement of the entire line is the empty set.
     */
    assertEquals(EMPTY,
        builder.setTo(LINE).complementAgainst(-Double.MAX_VALUE, Double.MAX_VALUE).build());

    /*
     * Test complementing the empty set against the interval [10,20].
     */
    assertEquals(create(10, 20), builder.empty().complementAgainst(10, 20).build());

    /*
     * Complement the interval against itself, which should result in the empty set.
     */
    assertEquals(EMPTY, builder.empty().add(10, 20).complementAgainst(10, 20).build());

    assertEquals(EMPTY, builder.empty().add(0, 20).complementAgainst(10, 20).build());

    assertEquals(EMPTY, builder.empty().add(10, 30).complementAgainst(10, 20).build());

    assertEquals(EMPTY, builder.empty().add(0, 30).complementAgainst(10, 20).build());

    /*
     * Now some more complicated cases involving multiple intervals.
     */
    builder.empty().add(10, 20).add(30, 40).add(50, 60).complementAgainst(0, 70);

    assertEquals(create(0, 10, 20, 30, 40, 50, 60, 70), builder.build());

    builder.empty().add(10, 20).add(30, 40).add(50, 60).complementAgainst(10, 70);

    assertEquals(create(20, 30, 40, 50, 60, 70), builder.build());

    builder.empty().add(10, 20).add(30, 40).add(50, 60).complementAgainst(0, 60);

    assertEquals(create(0, 10, 20, 30, 40, 50), builder.build());

    builder.empty().add(10, 20).add(30, 40).add(50, 60).complementAgainst(10, 60);

    assertEquals(create(20, 30, 40, 50), builder.build());

    builder.empty().add(10, 20).add(30, 40).add(50, 60).complementAgainst(15, 55);

    assertEquals(create(20, 30, 40, 50), builder.build());

  }

  @Test
  public void testComplementAgainstUnwritableInterval() {

    /*
     * Test that the complement of the empty set is the entire line.
     */
    assertEquals(LINE, builder.empty()
        .complementAgainst(new UnwritableInterval(-Double.MAX_VALUE, Double.MAX_VALUE)).build());

    /*
     * And that the complement of the entire line is the empty set.
     */
    assertEquals(EMPTY, builder.setTo(LINE)
        .complementAgainst(new UnwritableInterval(-Double.MAX_VALUE, Double.MAX_VALUE)).build());

    /*
     * Test complementing the empty set against the interval [10,20].
     */
    assertEquals(create(10, 20),
        builder.empty().complementAgainst(new UnwritableInterval(10, 20)).build());

    /*
     * Complement the interval against itself, which should result in the empty set.
     */
    assertEquals(EMPTY,
        builder.empty().add(10, 20).complementAgainst(new UnwritableInterval(10, 20)).build());

    assertEquals(EMPTY,
        builder.empty().add(0, 20).complementAgainst(new UnwritableInterval(10, 20)).build());

    assertEquals(EMPTY,
        builder.empty().add(10, 30).complementAgainst(new UnwritableInterval(10, 20)).build());

    assertEquals(EMPTY,
        builder.empty().add(0, 30).complementAgainst(new UnwritableInterval(10, 20)).build());

    /*
     * Now some more complicated cases involving multiple intervals.
     */
    builder.empty().add(10, 20).add(30, 40).add(50, 60)
        .complementAgainst(new UnwritableInterval(0, 70));

    assertEquals(create(0, 10, 20, 30, 40, 50, 60, 70), builder.build());

    builder.empty().add(10, 20).add(30, 40).add(50, 60)
        .complementAgainst(new UnwritableInterval(10, 70));

    assertEquals(create(20, 30, 40, 50, 60, 70), builder.build());

    builder.empty().add(10, 20).add(30, 40).add(50, 60)
        .complementAgainst(new UnwritableInterval(0, 60));

    assertEquals(create(0, 10, 20, 30, 40, 50), builder.build());

    builder.empty().add(10, 20).add(30, 40).add(50, 60)
        .complementAgainst(new UnwritableInterval(10, 60));

    assertEquals(create(20, 30, 40, 50), builder.build());

    builder.empty().add(10, 20).add(30, 40).add(50, 60)
        .complementAgainst(new UnwritableInterval(15, 55));

    assertEquals(create(20, 30, 40, 50), builder.build());

  }

  @Test
  public void testComplementAgainstLargeOffsetIntervalBug() {

    builder.add(-1000, -500);
    builder.add(500, 1000);
    builder.add(2000, 4000);
    builder.complementAgainst(0, 200);

    assertEquals(create(0, 200), builder.build());

  }

  @Test
  public void testExpandDouble() {

    /*
     * We don't need to exercise this as much due to the fact per the implementation of the builder
     * this just calls expand(begin,end).
     */
    builder.addAll(disjointIntervals).expand(2);
    assertEquals(create(9, 21, 29, 41, 49, 61), builder.build());

  }

  @Test
  public void testExpandDoubleDouble() {

    /*
     * We don't need to exercise this as much due to the fact per the builder implementation
     * contract(begin,end) simply calls this inverting the sign of the inputs.
     */
    builder.addAll(disjointIntervals).expand(1, 2);
    assertEquals(create(9, 22, 29, 42, 49, 62), builder.build());

  }

  @Test
  public void testContractDouble() {

    /*
     * We don't need to exercise this as much due to the fact per the implementation of the builder
     * this just calls contract(begin,end).
     */
    builder.addAll(disjointIntervals).contract(2);
    assertEquals(create(11, 19, 31, 39, 51, 59), builder.build());

  }

  @Test
  public void testContractDoubleDouble() {

    builder.contract(10, 5);
    assertEquals(EMPTY, builder.build());

    builder.empty().add(0, 10).contract(1, 2);
    assertEquals(create(1, 8), builder.build());

    builder.empty().add(0, 10).contract(-1, -2);
    assertEquals(create(-1, 12), builder.build());

    builder.empty().add(0, 10).contract(0, 9);
    assertEquals(create(0, 1), builder.build());

    builder.empty().add(0, 10).contract(9, 0);
    assertEquals(create(9, 10), builder.build());

    builder.empty().add(0, 10).contract(0, 10);
    assertEquals(create(0, 0), builder.build());

    builder.empty().add(0, 10).contract(10, 0);
    assertEquals(create(10, 10), builder.build());

    builder.empty().add(0, 10).contract(11, 0);
    assertEquals(EMPTY, builder.build());

    builder.empty().add(0, 10).contract(0, 11);
    assertEquals(EMPTY, builder.build());

    builder.empty().addAll(variedGaps).contract(0, 5);
    assertEquals(create(-10, -5, 10, 15, 31, 35, 52, 55), builder.build());

    builder.empty().addAll(variedGaps).contract(5, 0);
    assertEquals(create(-5, 0, 15, 20, 36, 40, 57, 60), builder.build());

    builder.empty().addAll(variedGaps).contract(1, 2);
    assertEquals(create(-9, -2, 11, 18, 32, 38, 53, 58), builder.build());

    builder.empty().addAll(variedGaps).contract(-1, -2);
    assertEquals(create(-11, 2, 9, 22, 30, 42, 51, 62), builder.build());

    builder.empty().addAll(variedGaps).contract(3, 3);
    assertEquals(create(-7, -3, 13, 17, 34, 37, 55, 57), builder.build());

    builder.empty().addAll(variedGaps).contract(3, 6);
    assertEquals(create(-7, -6, 13, 14, 34, 34), builder.build());

    builder.empty().addAll(variedGaps).contract(4, 6);
    assertEquals(create(-6, -6, 14, 14), builder.build());

    builder.empty().addAll(variedGaps).contract(5, 6);
    assertEquals(EMPTY, builder.build());

    builder.empty().addAll(variedGaps).contract(0, -5);
    assertEquals(create(-10, 5, 10, 25, 31, 45, 52, 65), builder.build());

    builder.empty().addAll(variedGaps).contract(-5, 0);
    assertEquals(create(-15, 0, 5, 20, 26, 40, 47, 60), builder.build());

    builder.empty().addAll(variedGaps).contract(-5, -5);
    assertEquals(create(-15, 25, 26, 45, 47, 65), builder.build());

    builder.empty().addAll(variedGaps).contract(-6, -6);
    assertEquals(create(-16, 66), builder.build());

  }

  @Test
  public void testFillGaps() {

    builder.fillGaps(100);
    assertEquals(EMPTY, builder.build());

    builder.empty().addAll(disjointIntervals).fillGaps(1.0);
    assertEquals(create(disjointIntervals), builder.build());

    builder.empty().addAll(disjointIntervals).fillGaps(10.0);
    assertEquals(create(10, 60), builder.build());

    builder.empty().addAll(disjointIntervals).fillGaps(11.0);
    assertEquals(create(10, 60), builder.build());

    builder.empty().addAll(variedGaps).fillGaps(8);
    assertEquals(create(variedGaps), builder.build());

    builder.empty().addAll(variedGaps).fillGaps(10);
    assertEquals(create(-10, 20, 31, 40, 52, 60), builder.build());

    builder.empty().addAll(variedGaps).fillGaps(11);
    assertEquals(create(-10, 40, 52, 60), builder.build());

    builder.empty().addAll(variedGaps).fillGaps(12);
    assertEquals(create(-10, 60), builder.build());

    builder.empty().addAll(variedGaps).fillGaps(15);
    assertEquals(create(-10, 60), builder.build());

  }

  @Test
  public void testRemoveIntervals() {

    builder.add(10, 20).add(30, 40).add(50, 60).removeIntervals(11);
    assertEquals(EMPTY, builder.build());

    builder.empty().add(10, 20).add(30, 41).add(50, 62).removeIntervals(11);
    assertEquals(create(50, 62), builder.build());

  }

  @Test
  public void testAddUnwritableInterval() {

    /*
     * Add to an empty list.
     */
    IntervalSet set = builder.add(new UnwritableInterval(1.0, 2.0)).build();
    assertEquals(create(1.0, 2.0), set);

    /*
     * Add to the end of a non-empty list.
     */
    set = builder.add(new UnwritableInterval(3.0, 4.0)).build();
    assertEquals(create(1.0, 2.0, 3.0, 4.0), set);

    /*
     * Add to the beginning of a non-empty list.
     */
    set = builder.add(new UnwritableInterval(-1.0, 0.0)).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.0, 3.0, 4.0), set);

    /*
     * Insert an interval between two existing intervals.
     */
    set = builder.add(new UnwritableInterval(2.5, 2.6)).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.0, 2.5, 2.6, 3.0, 4.0), set);

    /*
     * Insert an interval that overlaps one in the middle.
     */
    set = builder.add(new UnwritableInterval(2.4, 2.5)).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.0, 2.4, 2.6, 3.0, 4.0), set);

    /*
     * Insert an interval that overlaps the final interval on the front side.
     */
    set = builder.add(new UnwritableInterval(2.9, 3.2)).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.0, 2.4, 2.6, 2.9, 4.0), set);

    /*
     * Insert an interval that overlaps the final interval completely.
     */
    set = builder.add(new UnwritableInterval(3.0, 3.2)).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.0, 2.4, 2.6, 2.9, 4.0), set);

    /*
     * Insert an interval that overlaps the final interval exactly.
     */
    set = builder.add(new UnwritableInterval(2.9, 4.0)).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.0, 2.4, 2.6, 2.9, 4.0), set);

    /*
     * Insert an interval that includes, but runs past the last interval.
     */
    set = builder.add(new UnwritableInterval(2.9, 4.2)).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.0, 2.4, 2.6, 2.9, 4.2), set);

    /*
     * Insert an interval that includes, but runs past the last interval.
     */
    set = builder.add(new UnwritableInterval(2.8, 4.2)).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.0, 2.4, 2.6, 2.8, 4.2), set);

    /*
     * Collapse an interior window.
     */
    set = builder.add(new UnwritableInterval(2.3, 2.9)).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.0, 2.3, 4.2), set);

    /*
     * Add to the back of an interior interval.
     */
    set = builder.add(new UnwritableInterval(2.0, 2.1)).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.1, 2.3, 4.2), set);

    /*
     * Wipe out the entire list replacing it with a single interval.
     */
    set = builder.add(new UnwritableInterval(-100, 100)).build();
    assertEquals(create(-100.0, 100.0), set);
  }

  @Test
  public void testAddDoubleDouble() {

    /*
     * Add to an empty list.
     */
    IntervalSet set = builder.add(1.0, 2.0).build();
    assertEquals(create(1.0, 2.0), set);

    /*
     * Add to the end of a non-empty list.
     */
    set = builder.add(3.0, 4.0).build();
    assertEquals(create(1.0, 2.0, 3.0, 4.0), set);

    /*
     * Add to the beginning of a non-empty list.
     */
    set = builder.add(-1.0, 0.0).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.0, 3.0, 4.0), set);

    /*
     * Insert an interval between two existing intervals.
     */
    set = builder.add(2.5, 2.6).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.0, 2.5, 2.6, 3.0, 4.0), set);

    /*
     * Insert an interval that overlaps one in the middle.
     */
    set = builder.add(2.4, 2.5).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.0, 2.4, 2.6, 3.0, 4.0), set);

    /*
     * Insert an interval that overlaps the final interval on the front side.
     */
    set = builder.add(2.9, 3.2).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.0, 2.4, 2.6, 2.9, 4.0), set);

    /*
     * Insert an interval that overlaps the final interval completely.
     */
    set = builder.add(3.0, 3.2).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.0, 2.4, 2.6, 2.9, 4.0), set);

    /*
     * Insert an interval that overlaps the final interval exactly.
     */
    set = builder.add(2.9, 4.0).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.0, 2.4, 2.6, 2.9, 4.0), set);

    /*
     * Insert an interval that includes, but runs past the last interval.
     */
    set = builder.add(2.9, 4.2).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.0, 2.4, 2.6, 2.9, 4.2), set);

    /*
     * Insert an interval that includes, but runs past the last interval.
     */
    set = builder.add(2.8, 4.2).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.0, 2.4, 2.6, 2.8, 4.2), set);

    /*
     * Collapse an interior window.
     */
    set = builder.add(2.3, 2.9).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.0, 2.3, 4.2), set);

    /*
     * Add to the back of an interior interval.
     */
    set = builder.add(2.0, 2.1).build();
    assertEquals(create(-1.0, 0.0, 1.0, 2.1, 2.3, 4.2), set);

    /*
     * Wipe out the entire list replacing it with a single interval.
     */
    set = builder.add(-100, 100).build();
    assertEquals(create(-100.0, 100.0), set);

  }

  @Test
  public void testSize() {
    builder.addAll(create(0, 1, 2, 3, 4, 4, 5, 6));
    assertEquals(4, builder.size());

    builder.empty();
    assertEquals(0, builder.size());
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetIntUwritableIntervalLessThanZeroIndexException() {
    builder.addAll(create(0, 1, 2, 3, 4, 4, 5, 6));
    builder.get(-1, new Interval());
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetIntUnwritableIntervalGreaterThanEndIndexTightException() {
    builder = new IntervalSet.Builder(0);
    builder.addAll(create(0, 1, 2, 3, 4, 4, 5, 6));
    builder.get(4, new Interval());
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetIntUnwritableIntervalGreaterThanEndIndexLooseException() {
    builder = new IntervalSet.Builder(30);
    builder.addAll(create(0, 1, 2, 3, 4, 4, 5, 6));
    builder.get(4, new Interval());
  }

  @Test
  public void testGetIntUnwritableInterval() {
    builder.addAll(create(0, 1, 2, 3, 4, 4, 5, 6));
    Interval buffer = new Interval();
    Interval result = builder.get(0, buffer);
    assertSame(result, buffer);
    assertEquals(new Interval(0, 1), result);

    result = builder.get(1, buffer);
    assertSame(result, buffer);
    assertEquals(new Interval(2, 3), result);

    result = builder.get(2, buffer);
    assertSame(result, buffer);
    assertEquals(new Interval(4, 4), result);

    result = builder.get(3, buffer);
    assertSame(result, buffer);
    assertEquals(new Interval(5, 6), result);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetIntLessThanZeroIndexException() {
    builder.addAll(create(0, 1, 2, 3, 4, 4, 5, 6));
    builder.get(-1);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetIntGreaterThanEndIndexTightException() {
    builder = new IntervalSet.Builder(0);
    builder.addAll(create(0, 1, 2, 3, 4, 4, 5, 6));
    builder.get(4);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetIntGreaterThanEndIndexLooseException() {
    builder = new IntervalSet.Builder(30);
    builder.addAll(create(0, 1, 2, 3, 4, 4, 5, 6));
    builder.get(4);
  }

  @Test
  public void testGetInt() {
    builder.addAll(create(0, 1, 2, 3, 4, 4, 5, 6));
    UnwritableInterval result = builder.get(0);
    assertEquals(new UnwritableInterval(0, 1), result);

    result = builder.get(1);
    assertEquals(new UnwritableInterval(2, 3), result);

    result = builder.get(2);
    assertEquals(new UnwritableInterval(4, 4), result);

    result = builder.get(3);
    assertEquals(new UnwritableInterval(5, 6), result);
  }

}
