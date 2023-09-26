package picante.collections;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import com.google.common.collect.ImmutableList;

/**
 * This class consists exclusively of static methods that operate on or return collections. It
 * contains polymorphic algorithms that operate on collections. It is much like the Collections
 * class in the java.util package, only it provides other useful generic methods.
 * 
 * The methods of this class all throw a NullPointerException if the collections or class objects
 * provided to them are null.
 */
public class CollectionUtilities {

  /**
   * Determine the index of the last element less than or equal to the specified key. The list must
   * be sorted into ascending order according to the natural ordering specified by the
   * implementation of the Comparable interface. If it is not sorted, the results are undefined. If
   * the list contains multiple elements equal to the specified object, the last one is guaranteed
   * to be extracted.
   * 
   * The method utilizes {@link java.util.Collections#binarySearch(List, Object)} so its performance
   * characteristics are closely related.
   * 
   * @param <T> the parameterized type of the key
   * @param list the list containing objects capable of being compared to an instance of T, sorted
   *        in an order consistent with the comparison
   * @param key the key to locate the last element less than or equal to in the list
   * 
   * @return the range returned is [-1,list.size()-1], where -1 indicates that key precedes all
   *         elements in the list.
   */
  public static <T> int lastLessThanOrEqualTo(List<? extends Comparable<? super T>> list, T key) {

    int result = Collections.binarySearch(list, key);

    /*
     * If the result was located in the list directly, locate the last element equal to it and
     * return that index.
     */
    if (result >= 0) {
      int lastEqualIndex = locateLastElementEqualTo(result, key, list);
      return lastEqualIndex;
    }

    return convertIndexForLessThan(result, list.size());
  }

  /**
   * Determine the index of the last element less than or equal to the specified key. The list must
   * be sorted into ascending order according to the ordering specified by the implementation of the
   * supplied Comparator. If it is not sorted, the results are undefined. If the list contains
   * multiple elements equal to the specified object, the last one is guaranteed to be extracted.
   * 
   * The method utilizes {@link java.util.Collections#binarySearch(List, Object, Comparator)} so its
   * performance characteristics are closely related.
   * 
   * @param <T> the parameterized type of the key
   * @param list the list containing objects to be compared against key with the supplied
   *        comparator, sorted in an ordering consistent with the comparator
   * @param key the key to locate the last element less than or equal to in the list
   * @param c the comparator defining the ordering to be utilized in the search
   * 
   * @return the range of the return values is [-1,list.size()-1], where -1 indicates that key
   *         precedes all elements in the list.
   */
  public static <T> int lastLessThanOrEqualTo(List<? extends T> list, T key,
      Comparator<? super T> c) {

    int result = Collections.binarySearch(list, key, c);

    /*
     * If the result was located in the list directly, locate the last element equal to it and
     * return that index.
     */
    if (result >= 0) {
      int lastEqualIndex = locateLastElementEqualTo(result, list, c);
      return lastEqualIndex;
    }

    return convertIndexForLessThan(result, list.size());
  }

  /**
   * Determine the index of the last element less than the specified key. The list must be sorted
   * into ascending order according to the natural ordering specified by the implementation of the
   * Comparable interface. If it is not sorted, the results are undefined. If the list contains
   * multiple elements equal to the specified object, the last one is guaranteed to be extracted.
   * 
   * The method utilizes {@link java.util.Collections#binarySearch(List, Object)} so its performance
   * characteristics are closely related.
   * 
   * @param <T> the parameterized type of the key
   * @param list the list containing objects capable of being compared to an instance of T, sorted
   *        in an order consistent with the comparison
   * @param key the key to locate the last element less than in the list
   * 
   * @return the range returned is [-1,list.size()-1], where -1 indicates that key precedes all
   *         elements in the list.
   */
  public static <T> int lastLessThan(List<? extends Comparable<? super T>> list, T key) {

    int result = Collections.binarySearch(list, key);

    /*
     * If the result was located in the list directly, locate the first element equal to it and
     * return that index less 1.
     */
    if (result >= 0) {
      int lastEqualIndex = locateFirstElementEqualTo(result, key, list);
      return lastEqualIndex - 1;
    }

    return convertIndexForLessThan(result, list.size());
  }

  /**
   * Determine the index of the last element less than the specified key. The list must be sorted
   * into ascending order according to the ordering specified by the implementation of the supplied
   * Comparator. If it is not sorted, the results are undefined. If the list contains multiple
   * elements equal to the specified object, the last one is guaranteed to be extracted.
   * 
   * The method utilizes {@link java.util.Collections#binarySearch(List, Object, Comparator)} so its
   * performance characteristics are closely related.
   * 
   * @param <T> the parameterized type of the key
   * @param list the list containing objects to be compared against key with the supplied
   *        comparator, sorted in an ordering consistent with the comparator
   * @param key the key to locate the last element less than in the list
   * @param c the comparator defining the ordering to be utilized in the search
   * 
   * @return the range of the return values is [-1,list.size()-1], where -1 indicates that key
   *         precedes all elements in the list.
   */
  public static <T> int lastLessThan(List<? extends T> list, T key, Comparator<? super T> c) {

    int result = Collections.binarySearch(list, key, c);

    /*
     * If the result was located in the list directly, locate the first element equal to it and
     * return that index less 1.
     */
    if (result >= 0) {
      int lastEqualIndex = locateFirstElementEqualTo(result, list, c);
      return lastEqualIndex - 1;
    }

    return convertIndexForLessThan(result, list.size());
  }

  /**
   * Determine the index of the first element greater than or equal to the specified key. The list
   * must be sorted into ascending order according to the natural ordering specified by the
   * implementation of the Comparable interface. If it is not sorted, the results are undefined. If
   * the list contains multiple elements equal to the specified object, the first one is guaranteed
   * to be extracted.
   * 
   * The method utilizes {@link java.util.Collections#binarySearch(List, Object)} so its performance
   * characteristics are closely related.
   * 
   * @param <T> the parameterized type of the key
   * @param list the list containing objects capable of being compared to an instance of T, sorted
   *        in an order consistent with the comparison
   * @param key the key to locate the first element greater than or equal to in the list
   * 
   * @return the range returned is [0,list.size()], where list.size() indicates that key follows all
   *         elements in the list.
   */
  public static <T> int firstGreaterThanOrEqualTo(List<? extends Comparable<? super T>> list,
      T key) {

    /*
     * This is just as simple as invoking the corresponding lastLessThan method and adding one.
     */
    return lastLessThan(list, key) + 1;
  }

  /**
   * Determine the index of the first element greater than or equal to the specified key. The list
   * must be sorted into ascending order according to the ordering specified by the implementation
   * of the supplied Comparator. If it is not sorted, the results are undefined. If the list
   * contains multiple elements equal to the specified object, the first one is guaranteed to be
   * extracted.
   * 
   * The method utilizes {@link java.util.Collections#binarySearch(List, Object, Comparator)} so its
   * performance characteristics are closely related.
   * 
   * @param <T> the parameterized type of the key
   * @param list the list containing objects to be compared against key with the supplied
   *        comparator, sorted in an ordering consistent with the comparator
   * @param key the key to locate the first element greater than or equal to in the list
   * @param c the comparator defining the ordering to be utilized in the search
   * 
   * @return the range of the return values is [0,list.size()], where list.size() indicates that key
   *         follows all elements in the list.
   */
  public static <T> int firstGreaterThanOrEqualTo(List<? extends T> list, T key,
      Comparator<? super T> c) {

    /*
     * This is just as simple as invoking the corresponding lastLessThan method and adding one.
     */
    return lastLessThan(list, key, c) + 1;
  }

  /**
   * Determine the index of the first element greater than the specified key. The list must be
   * sorted into ascending order according to the natural ordering specified by the implementation
   * of the Comparable interface. If it is not sorted, the results are undefined. If the list
   * contains multiple elements equal to the specified object, the first one is guaranteed to be
   * extracted.
   * 
   * The method utilizes {@link java.util.Collections#binarySearch(List, Object)} so its performance
   * characteristics are closely related.
   *
   * @param <T> the parameterized type of the key
   * @param list the list containing objects capable of being compared to an instance of T, sorted
   *        in an order consistent with the comparison
   * @param key the key to locate the first element greater than in the list
   * 
   * @return the range returned is [0,list.size()], where list.size() indicates that key follows all
   *         elements in the list.
   */
  public static <T> int firstGreaterThan(List<? extends Comparable<? super T>> list, T key) {

    /*
     * This is just as simple as invoking the corresponding lastLessThanOrEqualTo method and adding
     * one.
     */
    return lastLessThanOrEqualTo(list, key) + 1;
  }

  /**
   * Determine the index of the first element greater than the specified key. The list must be
   * sorted into ascending order according to the ordering specified by the implementation of the
   * supplied Comparator. If it is not sorted, the results are undefined. If the list contains
   * multiple elements equal to the specified object, the first one is guaranteed to be extracted.
   * 
   * The method utilizes {@link java.util.Collections#binarySearch(List, Object, Comparator)} so its
   * performance characteristics are closely related.
   * 
   * @param <T> the parameterized type of the key
   * @param list the list containing objects to be compared against key with the supplied
   *        comparator, sorted in an ordering consistent with the comparator
   * @param key the key to locate the first element greater than in the list
   * @param c the comparator defining the ordering to be utilized in the search
   * 
   * @return the range of the return values is [0,list.size()], where list.size() indicates that key
   *         follows all elements in the list.
   */
  public static <T> int firstGreaterThan(List<? extends T> list, T key, Comparator<? super T> c) {

    /*
     * This is just as simple as invoking the corresponding lastLessThanOrEqualTo method and adding
     * one.
     */
    return lastLessThanOrEqualTo(list, key, c) + 1;
  }

  /**
   * Locate the first element equal to the supplied key, starting at index. This expectation is that
   * index is already pointing at an element of the list that is equal to the supplied key from the
   * implementation of the Comparable interface's perspective.
   * 
   * Supplying the key is necessary in the event that the list itself implements Comparable of an
   * unrelated type, of which key is an instance of a subclass.
   * 
   * @param <T> the parameterized type of key
   * @param index the index at which to start the search. It should be such that:
   *        list.get(index).compareTo(key) == 0.
   * @param key the key to compare against for equality
   * @param list the list containing objects to be compared against key, sorted in an order
   *        consistent with the natural ordering defined by Comparable
   * 
   * @return the index of the first element in the list that is equal to the supplied key.
   */
  private static <T> int locateFirstElementEqualTo(int index, T key,
      List<? extends Comparable<? super T>> list) {

    /*
     * If we are already at the head of the list, then just return.
     */
    if (index == 0) {
      return index;
    }

    int result = index;

    ListIterator<? extends Comparable<? super T>> iterator = list.listIterator(index);

    /*
     * Loop over the previous elements as long as they continue to compare with equality to key,
     * subtracting values from result.
     */
    while ((iterator.hasPrevious()) && (iterator.previous().compareTo(key) == 0)) {
      result--;
    }

    return result;
  }

  /**
   * Locate the first element in the list equal to the element by the supplied index.
   * 
   * @param <T> the parameterized type of key
   * @param index the index at which to start the search. It should be an index to an existing
   *        element of the list.
   * @param list the list containing objects to be compared against key, sorted in an order
   *        consistent with the natural ordering defined by Comparable
   * @param c the comparator used to search through the list
   * 
   * @return the index of the first element in the list that is equal to the supplied key.
   */
  private static <T> int locateFirstElementEqualTo(int index, List<? extends T> list,
      Comparator<? super T> c) {

    /*
     * If we are already at the head of the list, then just return.
     */
    if (index == 0) {
      return index;
    }

    int result = index;

    ListIterator<? extends T> iterator = list.listIterator(index);

    T key = iterator.next();

    /*
     * Back up the interator to its previous state. We only stepped forward to extract the
     * comparison element.
     */
    iterator.previous();

    /*
     * Loop over elements in the list, decrementing result, until we find an element that no longer
     * compares to key with equality.
     */
    while ((iterator.hasPrevious()) && (c.compare(key, iterator.previous()) == 0)) {
      result--;
    }

    return result;

  }

  /**
   * Locate the last element equal to the supplied key, starting at index. This expectation is that
   * index is already pointing at an element of the list that is equal to the supplied key from the
   * implementation of the Comparable interface's perspective.
   * 
   * Supplying the key is necessary in the event that the list itself implements Comparable of an
   * unrelated type, of which key is an instance of a subclass.
   * 
   * @param <T> the parameterized type of key
   * @param index the index at which to start the search. It should be such that:
   *        list.get(index).compareTo(key) == 0.
   * @param key the key to compare against for equality
   * @param list the list containing objects to be compared against key, sorted in an order
   *        consistent with the natural ordering defined by Comparable
   * 
   * @return the index of the last element in the list that is equal to the supplied key.
   */
  private static <T> int locateLastElementEqualTo(int index, T key,
      List<? extends Comparable<? super T>> list) {

    /*
     * If we are already at the end of the list, just return.
     */
    if (index == list.size() - 1) {
      return index;
    }

    int result = index;

    ListIterator<? extends Comparable<? super T>> iterator = list.listIterator(index + 1);

    /*
     * Loop over elements in the list incrementing result as long as the elements compare with key
     * as equality.
     */
    while ((iterator.hasNext()) && (iterator.next().compareTo(key) == 0)) {
      result++;
    }

    return result;
  }

  /**
   * Locate the last element in the list equal to the element by the supplied index.
   * 
   * @param <T> the parameterized type of key
   * @param index the index at which to start the search. It should be an index to an existing
   *        element of the list.
   * @param list the list containing objects to be compared against key, sorted in an order
   *        consistent with the natural ordering defined by Comparable
   * @param c the comparator used to search through the list
   * 
   * @return the index of the last element in the list that is equal to the supplied key.
   */
  private static <T> int locateLastElementEqualTo(int index, List<? extends T> list,
      Comparator<? super T> c) {

    /*
     * If we are already at the end of the list, just return.
     */
    if (index == list.size() - 1) {
      return index;
    }

    int result = index;

    ListIterator<? extends T> iterator = list.listIterator(index);

    T key = iterator.next();

    /*
     * Loop over elements of the list, incrementing result, as long as these elements continue
     * comparing with key as equality.
     */
    while ((iterator.hasNext() && (c.compare(key, iterator.next()) == 0))) {
      result++;
    }

    return result;

  }

  /**
   * In the event that the binarySearch algorithm from the Collections class is unable to turn up a
   * matched element, locate the index of the element that is strictly less than the one sought.
   * 
   * @param result a negative result from either
   *        {@link java.util.Collections#binarySearch(List, Object)} or
   *        {@link java.util.Collections#binarySearch(List, Object, Comparator)}
   * 
   * @param listSize the size of the list over which the binary search was performed at the time of
   *        the search
   * 
   * @return the index of the element strictly less than the one sought after in the binary search.
   *         Range of returned values is [-1,listSize], where -1 indicates the value sought after
   *         precedes all elements in the list.
   */
  private static int convertIndexForLessThan(int result, @SuppressWarnings("unused") int listSize) {

    int insertionPoint = -result - 1;

    /*
     * Since the insertion point is defined as the index at which everyone will be shifted to the
     * right:
     * 
     * value: 10 20 30 40 index: 0 1 2 3
     * 
     * If insertion point is 1, then that means the result of inserting this element into the list
     * will result:
     * 
     * value: 10 15 20 30 40 index: 0 1 2 3 4
     * 
     * would be the result, and 15 would have reduced to an insertion point of 1. So, long story
     * short, if we reach here then just subtract one and we'll get the answer we desire.
     */
    return insertionPoint - 1;

  }

  /**
   * Adds the contents of an iterable to the supplied collection.
   * <p>
   * Elements are added to the collection via the {@link Collection#add(Object)} method in the order
   * that the iterator generated from the supplied iterable produces them.
   * </p>
   * 
   * @param <T> the element type of the iterable
   * @param iterable the iterable, producing elements of type T
   * @param collection the collection to receive elements of type T
   * 
   * @return a reference to collection for the convenience of method chaining
   */
  public static <T, C extends Collection<? super T>> C addAll(Iterable<T> iterable, C collection) {
    return addAll(iterable.iterator(), collection);
  }

  /**
   * Adds the contents of an iterator to the supplied collection.
   * <p>
   * Elements are added to the collection via the {@link Collection#add(Object)} method in the order
   * that the iterator produces them.
   * </p>
   * 
   * @param <T> the element type of the iterator
   * @param <C> the collection type
   * @param iterable the iterator, producing elements of type T
   * @param collection the collection to receive elements of type T
   * 
   * @return a reference to collection for the convenience of method chaining
   */
  public static <T, C extends Collection<? super T>> C addAll(Iterator<T> iterator, C collection) {
    while (iterator.hasNext()) {
      collection.add(iterator.next());
    }
    return collection;
  }


  /**
   * allows an ImmutableList of children objects to be converted to an ImmutableList of parent
   * objects; for an ordinary Java List, this would be dangerous because of the ability to
   * potentially ADD diverse objects into the list. But since this method deals with an immutable
   * list, nothing can be added. Note that the requirement that S extends T is not needed - the
   * compiler will let this through even if S does not extend T, but that's probably something you
   * don't want to do, so the S extends T is there to remind you of this fact. As long as S extends
   * T, the casting that goes on inside this method is safe.
   * 
   * @param <T> the type of the child
   * @param <S> the type of the parent
   * @param listOfChildren the children to convert
   * 
   * @return the same list cast as a list of parent objects (type T)
   */
  @SuppressWarnings("unchecked")
  public static <T, S extends T> ImmutableList<T> convertToListOfSuperclass(
      ImmutableList<S> listOfChildren) {
    // recast as list of parent objects:
    return (ImmutableList<T>) listOfChildren;
  }

}
