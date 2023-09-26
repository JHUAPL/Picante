package picante.math.cones;

import static com.google.common.base.Preconditions.checkArgument;

import java.awt.geom.Path2D;
import java.awt.geom.PathIterator;
import java.util.Arrays;
import java.util.List;

/**
 * Implementation of {@link PathIterator} that allows various conditions not permitted by
 * {@link Path2D} to be setup.
 */
class BasicPathIterator implements PathIterator {

  private final List<Integer> types;
  private final List<double[]> coords;

  private int index = 0;

  BasicPathIterator(List<Integer> types, List<double[]> coords) {
    super();
    checkArgument(types.size() == coords.size());
    this.types = types;
    this.coords = coords;
  }

  @Override
  public int getWindingRule() {
    return PathIterator.WIND_EVEN_ODD;
  }

  @Override
  public boolean isDone() {
    return index == types.size();
  }

  @Override
  public void next() {
    index++;
  }

  @Override
  public int currentSegment(float[] coords) {

    double[] coordinates = this.coords.get(index);
    for (int i = 0; i < coordinates.length; i++) {
      coords[i] = (float) coordinates[i];
    }

    return types.get(index);
  }

  @Override
  public int currentSegment(double[] coords) {
    System.arraycopy(this.coords.get(index), 0, coords, 0, this.coords.get(index).length);
    return types.get(index);
  }


  static void pathIteratorToStandardOutput(PathIterator iterator) {
    double[] data = new double[6];
    for (; !iterator.isDone(); iterator.next()) {
      int type = iterator.currentSegment(data);
      System.out.println(type + ": " + Arrays.toString(data));
    }
  }


}
