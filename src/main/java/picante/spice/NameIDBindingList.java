package picante.spice;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.Map;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;

/**
 * Package private class used to capture the logic to record user requested ID bindings supplied to
 * the SPICE environment builder.
 * <p>
 * This class is horribly inefficient, particularly with its use of data structures. It was
 * intentional, in that simplicity of implementation for this specific class is more important than
 * efficiency. This code should only really be utilized by the SPICE environment building tools,
 * something that is typically utilized infrequently within an application.
 * </p>
 * <p>
 * Names inserted into this binding list are converted to their canonical forms according to the
 * standard SPICE object/frame name equivalence class definition.
 * </p>
 * 
 * <p>
 * TODO: Now that we have decided it is an error to bind the same object multiple times, we could
 * turn the guts of this into a map safely. It would allow fast failing if the user specified the
 * same string (in the equivalence class) multiple times.
 * </p>
 *
 * @param <I> the ID code type captured by this list of bindings; typically this will be either
 *        {@link EphemerisID} or {@link FrameID}
 */
class NameIDBindingList<I> implements Iterable<NameIDBindingList.NameIDPair<I>> {

  /**
   * Class capturing a name ID pairing.
   * 
   * @param <I> the type of the ID
   */
  static class NameIDPair<I> {
    private final String name;
    private final I id;

    NameIDPair(String name, I id) {
      this.name = Utilities.canonicalizeSpiceName(name);
      this.id = id;
    }

    String getName() {
      return name;
    }

    I getID() {
      return id;
    }
  }

  /**
   * An ordered list of bindings, last entry takes precedence over prior entries.
   */
  private final LinkedList<NameIDPair<I>> bindingList = new LinkedList<NameIDPair<I>>();

  /**
   * Adds a binding request to the end of the binding list.
   * 
   * @param name the string name to bind
   * @param id the ID in which to bind to name
   */
  public void add(String name, I id) {
    bindingList.add(new NameIDPair<I>(name, id));
  }

  /**
   * Adds a map of string ID bindings to the binding list. Note: this map is iterated over in the
   * ordering of its keySet(). If any string name conflicts occur, the order of iteration defines
   * arrival ordering.
   * 
   * @param bindings the map of bindings to bind
   */
  public void addAll(Map<String, ? extends I> bindings) {
    for (String name : bindings.keySet()) {
      add(name, bindings.get(name));
    }
  }

  /**
   * Removes the highest precedence occurrence of name. If name is not present in the binding list,
   * this operation is just an expensive no-op.
   * 
   * @param name
   */
  public void remove(String name) {
    String canonicalName = Utilities.canonicalizeSpiceName(name);
    ListIterator<NameIDPair<I>> iterator = bindingList.listIterator(bindingList.size());
    while (iterator.hasPrevious()) {
      if (iterator.previous().name.equals(canonicalName)) {
        iterator.remove();
        return;
      }
    }
  }

  /**
   * Removes all of the supplied names. This is equivalent to invoking remove() in a loop over the
   * supplied iterable. If any of the supplied names are not present in the list, it results in what
   * amounts to a computationally expensive no-op.
   * 
   * @param names an iterable listing, in order, the names to be removed.
   */
  public void removeAll(Iterable<String> names) {
    for (String name : names) {
      remove(name);
    }

  }

  /**
   * Clears all of the bindings currently stored entirely.
   */
  public void clear() {
    bindingList.clear();
  }

  /**
   * {@inheritDoc}
   * 
   * Produces an iterator of the bindings, ordered from lowest priority to highest. Note: the
   * supplied iterator does not allow the remove operation and will throw
   * {@link UnsupportedOperationException} if invoked.
   */
  @Override
  public Iterator<NameIDPair<I>> iterator() {
    return new Iterator<NameIDBindingList.NameIDPair<I>>() {
      private final Iterator<NameIDPair<I>> iterator = bindingList.iterator();

      @Override
      public boolean hasNext() {
        return iterator.hasNext();
      }

      @Override
      public NameIDPair<I> next() {
        return iterator.next();
      }

      @Override
      public void remove() {
        throw new UnsupportedOperationException("Remove not supported.");

      }

    };
  }

}
