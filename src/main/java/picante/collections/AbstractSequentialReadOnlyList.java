package picante.collections;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;

/**
 * abstract base class for making a list view of objects; you just need the size() and get(i)
 * methods;
 * 
 * Note that this list does not implement the marker interface RandomAccess, meaning that the
 * collections will not assume fast random access. (There is a subclass that does have random access
 * if that's what you want.)
 * 
 * All methods that add or modify elements throw runtime exceptions.
 * 
 * @author vandejd1
 */
@SuppressWarnings("unused")
public abstract class AbstractSequentialReadOnlyList<T> implements List<T> {

  @Override
  public boolean add(T o) {
    throw new UnsupportedOperationException("Unsupported method called on a read-only list.");
  }

  @Override
  public void add(int index, T element) {
    throw new UnsupportedOperationException("Unsupported method called on a read-only list.");
  }

  @Override
  public boolean addAll(Collection<? extends T> c) {
    throw new UnsupportedOperationException("Unsupported method called on a read-only list.");
  }

  @Override
  public boolean addAll(int index, Collection<? extends T> c) {
    throw new UnsupportedOperationException("Unsupported method called on a read-only list.");
  }

  @Override
  public void clear() {
    throw new UnsupportedOperationException("Unsupported method called on a read-only list.");
  }

  /**
   * a very simple implementation that does linear search
   */
  @Override
  public boolean contains(Object o) {
    for (int i = 0; i < size(); i++) {
      if (get(i).equals(o)) {
        return true;
      }
    }
    return false;
  }

  /**
   * does linear search on every element in the list
   */
  @Override
  public boolean containsAll(Collection<?> c) {
    for (Object o : c) {
      if (!contains(o)) {
        return false;
      }
    }
    return true;
  }

  /**
   * a very simple implementation that does linear search to find the index of the element that
   * ".equals() the given object
   */
  @Override
  public int indexOf(Object o) {
    for (int i = 0; i < size(); i++) {
      if (get(i).equals(o)) {
        return i;
      }
    }
    return -1;
  }

  @Override
  public boolean isEmpty() {
    return size() == 0;
  }

  @Override
  public Iterator<T> iterator() {
    return listIterator();
  }

  @Override
  public int lastIndexOf(Object o) {
    throw new UnsupportedOperationException("Unsupported method called on a read-only list.");
  }

  @Override
  public ListIterator<T> listIterator() {
    return listIterator(0);
  }

  @Override
  public ListIterator<T> listIterator(int index) {
    return new ListItr(index);
  }

  @Override
  public boolean remove(Object o) {
    throw new UnsupportedOperationException("Unsupported method called on a read-only list.");
  }

  @Override
  public T remove(int index) {
    throw new UnsupportedOperationException("Unsupported method called on a read-only list.");
  }

  @Override
  public boolean removeAll(Collection<?> c) {
    throw new UnsupportedOperationException("Unsupported method called on a read-only list.");
  }

  @Override
  public boolean retainAll(Collection<?> c) {
    throw new UnsupportedOperationException("Unsupported method called on a read-only list.");
  }

  @Override
  public T set(int index, T element) {
    throw new UnsupportedOperationException("Unsupported method called on a read-only list.");
  }

  @Override
  public List<T> subList(int fromIndex, int toIndex) {
    // TODO: implement this?
    throw new UnsupportedOperationException("Unsupported method called on a read-only list.");
  }

  @Override
  public Object[] toArray() {
    // TODO: implement this?
    throw new UnsupportedOperationException("Unsupported method called on a read-only list.");
  }

  @Override
  public <X> X[] toArray(X[] a) {
    // TODO: implement this?
    throw new UnsupportedOperationException("Unsupported method called on a read-only list.");
  }

  private class Itr implements Iterator<T> {
    /**
     * Index of element to be returned by subsequent call to next.
     */
    int cursor = 0;

    /**
     * Index of element returned by most recent call to next or previous. Reset to -1 if this
     * element is deleted by a call to remove.
     */
    int lastRet = -1;

    // /**
    // * The modCount value that the iterator believes that the backing
    // * List should have. If this expectation is violated, the iterator
    // * has detected concurrent modification.
    // */
    // int expectedModCount = modCount;

    @Override
    public boolean hasNext() {
      return cursor != size();
    }

    @Override
    public T next() {
      // checkForComodification();
      try {
        T next = get(cursor);
        lastRet = cursor++;
        return next;
      } catch (IndexOutOfBoundsException e) {
        // checkForComodification();
        throw new NoSuchElementException();
      }
    }

    @Override
    public void remove() {
      throw new UnsupportedOperationException("Unsupported method called on a read-only list.");
    }

    // final void checkForComodification()
    // {
    // if (modCount != expectedModCount)
    // throw new ConcurrentModificationException();
    // }
  }

  private class ListItr extends Itr implements ListIterator<T> {
    ListItr(int index) {
      cursor = index;
    }

    @Override
    public boolean hasPrevious() {
      return cursor != 0;
    }

    @Override
    public T previous() {
      // checkForComodification();
      try {
        int i = cursor - 1;
        T previous = get(i);
        lastRet = cursor = i;
        return previous;
      } catch (IndexOutOfBoundsException e) {
        // checkForComodification();
        throw new NoSuchElementException();
      }
    }

    @Override
    public int nextIndex() {
      return cursor;
    }

    @Override
    public int previousIndex() {
      return cursor - 1;
    }

    @Override
    public void set(T o) {
      throw new UnsupportedOperationException("Unsupported method called on a read-only list.");
      // if (lastRet == -1)
      // throw new IllegalStateException();
      // // checkForComodification();
      //
      // try {
      // AbstractList.this.set(lastRet, o);
      // expectedModCount = modCount;
      // } catch (IndexOutOfBoundsException e) {
      // throw new ConcurrentModificationException();
      // }
    }

    @Override
    public void add(T o) {
      throw new UnsupportedOperationException("Unsupported method called on a read-only list.");
      // checkForComodification();
      //
      // try {
      // AbstractList.this.add(cursor++, o);
      // lastRet = -1;
      // expectedModCount = modCount;
      // } catch (IndexOutOfBoundsException e) {
      // throw new ConcurrentModificationException();
      // }
    }
  }

}
