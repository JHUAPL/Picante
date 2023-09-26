/**
 * Author : vandejd1 Created : Apr 19, 2010
 *
 * Copyright (C) 2010 The Johns Hopkins University Applied Physics Laboratory (JHU/APL) All rights
 * reserved
 */
package picante.collections;

/**
 * captures a range of indices specified by a low index and a high index; both end points are
 * considered to be included in the range
 *
 *
 * NOTES for future development: If you want to add methods, look at the picante.math.intervals
 * package and make sure that any method names you use are consistent with the (non-integer)
 * interval processing class there. After discussion, we decided that its probably easier to
 * duplicate code for the integer interval mechanisms (if we ever need it) rather than try to come
 * up with a convoluted framework for handling (unwritable and writable) intervals of multiple
 * types.
 *
 * If you want to make this class writable: create a new parent class that is unwritable:
 * UnwritableIndexRange, and then move the getter methods there. Then this class becomes the
 * writable versions and you can add the setters here.
 *
 * This class is one instance of a larger concept - that of index mapping or index translation (as
 * in sub-setting, or selecting a sub-range). There is already a class in the timeseries library
 * that will eventually be moved into crucible that this class will eventually implement:
 * IIndexTranslator.
 *
 * @author vandejd1
 */
public class IndexRange {
  private final int lowIndex;
  private final int highIndex;

  public IndexRange(int lowIndexInclusive, int highIndexInclusive) {
    this.lowIndex = lowIndexInclusive;
    this.highIndex = highIndexInclusive;
  }

  /**
   * @return the lower index in the range; this index value is included in the range
   */
  public int getLowIndex() {
    return lowIndex;
  }

  /**
   * @return the upper index in the range; this index is included in the range
   */
  public int getHighIndex() {
    return highIndex;
  }

  /**
   * @return the total number of indices in the range, including the lower and upper endpoints
   */
  public int getNumIndicesIncluded() {
    return highIndex - lowIndex + 1;
  }

  /**
   * returns true if the given index is inside this range, i.e., if
   * {@code lowIndex <= idx <= highIndex}
   *
   * @param idx the index to test
   * 
   * @return true if contained in the range
   */
  public boolean contains(int idx) {
    return (idx >= lowIndex) && (idx <= highIndex);
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + highIndex;
    result = prime * result + lowIndex;
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
    IndexRange other = (IndexRange) obj;
    if (highIndex != other.highIndex) {
      return false;
    }
    if (lowIndex != other.lowIndex) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return "IndexRange [lowIndex=" + lowIndex + ", highIndex=" + highIndex + "]";
  }

}
