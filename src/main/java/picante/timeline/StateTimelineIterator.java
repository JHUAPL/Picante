package picante.timeline;

import java.util.Iterator;
import java.util.Map.Entry;
import picante.math.intervals.UnwritableInterval;
import java.util.NoSuchElementException;

/**
 * An {@link Iterator}<{@link Entry}<{@link UnwritableInterval}, S>> over the sorted elements in a
 * {@link StateTimelineCollection}<S>
 */
final class StateTimelineIterator<S> implements Iterator<Entry<UnwritableInterval, S>> {

  private final StateTimelineCollection<S> collection;
  private int i;

  StateTimelineIterator(StateTimelineCollection<S> collection) {
    this.collection = collection;
    this.i = -1;
  }

  @Override
  public boolean hasNext() {
    return i + 1 < collection.size();
  }

  @Override
  public Entry<UnwritableInterval, S> next() {
    if (i + 2 > collection.size()) {
      throw new NoSuchElementException("StateTimeline: attempting to iterator past last entry");
    }
    return ++i == 0 ? collection.getFirstEntry()
        : (i < collection.size() - 1 ? collection.getMiddleEntry(i) : collection.getLastEntry());
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((collection == null) ? 0 : collection.hashCode());
    result = prime * result + i;
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    StateTimelineIterator<?> other = (StateTimelineIterator<?>) obj;
    if (collection == null) {
      if (other.collection != null) {
        return false;
      }
    } else if (!collection.equals(other.collection)) {
      return false;
    }
    if (i != other.i) {
      return false;
    }
    return true;
  }

}
