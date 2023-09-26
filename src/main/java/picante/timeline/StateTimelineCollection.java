package picante.timeline;

import static com.google.common.base.Preconditions.checkNotNull;

import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map.Entry;

import com.google.common.collect.ObjectArrays;
import picante.math.intervals.UnwritableInterval;

/**
 * An immutable {@link Collection}<{@link Entry}<{@link UnwritableInterval}, S>> view of the sorted
 * entries of a {@link StateTimeline}<S>
 */
final class StateTimelineCollection<S> implements Collection<Entry<UnwritableInterval, S>> {

  private final StateTimeline<S> timeline;

  StateTimelineCollection(StateTimeline<S> timeline) {
    this.timeline = timeline;
  }

  @Override
  public int size() {
    return timeline.getStates().size();
  }

  @Override
  public boolean isEmpty() {
    return size() != 0;
  }

  @Override
  public boolean contains(Object o) {
    Iterator<Entry<UnwritableInterval, S>> it = iterator();
    while (it.hasNext()) {
      if (it.next().equals(o)) {
        return true;
      }
    }
    return false;
  }

  @Override
  public boolean containsAll(Collection<?> c) {
    for (Object o : c) {
      if (!contains(o)) {
        return false;
      }
    }
    return true;
  }

  @Override
  public Object[] toArray() {
    int size = size();
    if (size == 0) {
      return new Object[0];
    }
    int offset = 0;
    Object[] result = new Object[size];
    Iterator<Entry<UnwritableInterval, S>> it = iterator();
    while (it.hasNext()) {
      result[offset++] = it.next();
    }
    return result;
  }

  @SuppressWarnings("unchecked")
  @Override
  public <T> T[] toArray(T[] a) {
    checkNotNull(a);
    int size = size();
    if (a.length < size) {
      a = ObjectArrays.newArray(a, size);
    } else if (a.length > size) {
      a[size] = null;
    }
    int offset = 0;
    Iterator<Entry<UnwritableInterval, S>> it = iterator();
    while (it.hasNext()) {
      a[offset++] = (T) it.next();
    }
    return a;
  }

  @Override
  public Iterator<Entry<UnwritableInterval, S>> iterator() {
    return new StateTimelineIterator<S>(this);
  }

  Entry<UnwritableInterval, S> getFirstEntry() {
    return new SimpleImmutableEntry<>(
        new UnwritableInterval(timeline.getDomain().getBegin(),
            size() == 1 ? timeline.getDomain().getEnd() : timeline.getTransitionTimes().get(0)),
        timeline.getStates().get(0));
  }

  Entry<UnwritableInterval, S> getMiddleEntry(int i) {
    return new SimpleImmutableEntry<>(
        new UnwritableInterval(timeline.getTransitionTimes().get(i - 1),
            timeline.getTransitionTimes().get(i)),
        timeline.getStates().get(i));
  }

  Entry<UnwritableInterval, S> getLastEntry() {
    return new SimpleImmutableEntry<>(
        new UnwritableInterval(size() == 1 ? timeline.getDomain().getBegin()
            : timeline.getTransitionTimes().get(size() - 2), timeline.getDomain().getEnd()),
        timeline.getStates().get(size() - 1));
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((timeline == null) ? 0 : timeline.hashCode());
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
    StateTimelineCollection<?> other = (StateTimelineCollection<?>) obj;
    if (timeline == null) {
      if (other.timeline != null) {
        return false;
      }
    } else if (!timeline.equals(other.timeline)) {
      return false;
    }
    return true;
  }

  @Deprecated
  @Override
  public boolean add(@SuppressWarnings("unused") Entry<UnwritableInterval, S> e) {
    throw new UnsupportedOperationException("StateTimelineCollection is immutable");
  }

  @Deprecated
  @Override
  public boolean remove(@SuppressWarnings("unused") Object o) {
    throw new UnsupportedOperationException("StateTimelineCollection is immutable");
  }

  @Deprecated
  @Override
  public boolean addAll(
      @SuppressWarnings("unused") Collection<? extends Entry<UnwritableInterval, S>> c) {
    throw new UnsupportedOperationException("StateTimelineCollection is immutable");
  }

  @Deprecated
  @Override
  public boolean removeAll(@SuppressWarnings("unused") Collection<?> c) {
    throw new UnsupportedOperationException("StateTimelineCollection is immutable");
  }

  @Deprecated
  @Override
  public boolean retainAll(@SuppressWarnings("unused") Collection<?> c) {
    throw new UnsupportedOperationException("StateTimelineCollection is immutable");
  }

  @Deprecated
  @Override
  public void clear() {
    throw new UnsupportedOperationException("StateTimelineCollection is immutable");
  }

}
