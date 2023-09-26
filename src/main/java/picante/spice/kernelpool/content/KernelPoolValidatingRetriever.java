package picante.spice.kernelpool.content;

import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;

import com.google.common.base.Predicate;
import com.google.common.collect.ImmutableList;
import picante.spice.kernelpool.UnwritableKernelPool;

/**
 * A class that wraps a kernel pool object, providing error checks upon retrieval operations.
 * <p>
 * Any format or error checks performed by methods contained within the public API of this method
 * should generate {@link KernelPoolValidationException}. These are checked exceptions to force the
 * users of this class to deal with the error condition immediately.
 * </p>
 */
class KernelPoolValidatingRetriever {

  private final UnwritableKernelPool pool;

  public KernelPoolValidatingRetriever(UnwritableKernelPool pool) {
    this.pool = pool;
  }

  public UnwritableKernelPool getPool() {
    return pool;
  }

  public boolean containsKeyword(String keyword) {
    return pool.isStringValued(keyword) || pool.isDoubleValued(keyword);
  }

  public String getString(String keyword) throws KernelPoolValidationException {
    checkTypeIsString(keyword);
    List<String> values = pool.getStrings(keyword);
    if (values.size() != 1) {
      throw new KernelPoolValidationException(keyword, "Has length: " + values.size() + ", not 1.");
    }
    return values.get(0);
  }

  public double getDouble(String keyword) throws KernelPoolValidationException {
    checkTypeIsNumeric(keyword);
    List<Double> values = pool.getDoubles(keyword);
    if (values.size() != 1) {
      throw new KernelPoolValidationException(keyword, "Has length: " + values.size() + ", not 1.");
    }
    return values.get(0);
  }

  public int getInteger(String keyword) throws KernelPoolValidationException {
    checkTypeIsNumeric(keyword);
    List<Integer> values = pool.getIntegers(keyword);
    if (values.size() != 1) {
      throw new KernelPoolValidationException(keyword, "Has length: " + values.size() + ", not 1.");
    }
    return values.get(0);
  }

  public int getInteger(String keyword, Set<Integer> acceptableValues)
      throws KernelPoolValidationException {
    int value = getInteger(keyword);

    if (acceptableValues.contains(value)) {
      return value;
    }

    throw new KernelPoolValidationException(keyword,
        "Contains a value, " + value + ", that is not in the list of acceptable values.");
  }

  public List<Integer> getIntegersExpectedLength(String keyword, Set<Integer> acceptableValues,
      int expectedLength) throws KernelPoolValidationException {
    List<Integer> values = getIntegersExpectedLength(keyword, expectedLength);

    for (Integer i : values) {
      if (!acceptableValues.contains(i)) {
        throw new KernelPoolValidationException(keyword,
            "At least one integer in the requested list, " + i
                + " does not conform to the acceptable values.");
      }
    }
    return values;
  }

  public List<Integer> getIntegersExpectedLength(String keyword, int expectedLength)
      throws KernelPoolValidationException {
    checkTypeIsNumeric(keyword);
    List<Integer> values = pool.getIntegers(keyword);
    if (values.size() != expectedLength) {
      throw new KernelPoolValidationException(keyword,
          "Value array length is: " + values.size() + ", expected length: " + expectedLength);
    }
    return values;
  }

  public List<Double> getDoublesMaximumLength(String keyword, int maximumAllowedLength)
      throws KernelPoolValidationException {
    checkTypeIsNumeric(keyword);
    List<Double> values = pool.getDoubles(keyword);
    if (values.size() > maximumAllowedLength) {
      throw new KernelPoolValidationException(keyword, "Value array length is: " + values.size()
          + ", which exceeds maximum allowed length: " + maximumAllowedLength);
    }
    return values;
  }

  public List<Double> getDoubles(String keyword) throws KernelPoolValidationException {
    checkTypeIsNumeric(keyword);
    List<Double> values = pool.getDoubles(keyword);
    return values;
  }

  /**
   * Retrieves the list of doubles, applying the supplied predicate to each value in the list.
   * 
   * @param keyword the keyword to retrieve
   * 
   * @param filter the filter that each value must pass
   * 
   * @return the list of values
   * 
   * @throws KernelPoolValidationException if the supplied keyword is not numeric, or if any element
   *         of the list fails the application (apply(value) == false) of the predicate filter
   */
  public List<Double> getDoublesWithValueValidation(String keyword, Predicate<Double> filter)
      throws KernelPoolValidationException {

    checkTypeIsNumeric(keyword);
    List<Double> values = pool.getDoubles(keyword);

    for (int i = 0; i < values.size(); i++) {
      if (!filter.apply(values.get(i))) {
        throw new KernelPoolValidationException(
            "The value at position: " + i + " in the kernel pool list associated with keyword: ["
                + keyword + "] fails the required validation: " + filter);
      }
    }

    return values;

  }

  public List<Double> getDoublesLengthNoMoreThan(String keyword, int lengthLimit)
      throws KernelPoolValidationException {
    checkTypeIsNumeric(keyword);
    List<Double> values = pool.getDoubles(keyword);
    if (values.size() > lengthLimit) {
      throw new KernelPoolValidationException(keyword, "Value array length is: " + values.size()
          + ", expected length to be no more than: " + lengthLimit);
    }
    return values;
  }

  public List<Double> getDoublesExpectedLength(String keyword, int expectedLength)
      throws KernelPoolValidationException {
    checkTypeIsNumeric(keyword);
    List<Double> values = pool.getDoubles(keyword);
    if (values.size() != expectedLength) {
      throw new KernelPoolValidationException(keyword,
          "Value array length is: " + values.size() + ", expected length: " + expectedLength);
    }
    return values;
  }

  public List<Double> getDoublesLengthModulo(String keyword, int modulo)
      throws KernelPoolValidationException {
    checkTypeIsNumeric(keyword);
    List<Double> values = pool.getDoubles(keyword);

    if (values.size() % modulo != 0) {
      throw new KernelPoolValidationException(keyword,
          "Value array length is not evenly divisible by the" + " required value: " + modulo);
    }

    return values;
  }

  public ImmutableList<String> getMatchingKeywords(Matcher matcher) {

    ImmutableList.Builder<String> builder = ImmutableList.builder();

    for (String keyword : pool.getKeywords()) {
      matcher.reset(keyword);

      if (matcher.matches()) {
        builder.add(keyword);
      }
    }

    return builder.build();
  }

  private void checkTypeIsString(String keyword) throws KernelPoolValidationException {
    if (pool.isDoubleValued(keyword)) {
      throw new KernelPoolValidationException(keyword, "Type is numeric, not string.");
    }
    if (!pool.isStringValued(keyword)) {
      throw new KernelPoolValidationException(keyword, "Keyword is absent.");
    }
  }

  private void checkTypeIsNumeric(String keyword) throws KernelPoolValidationException {
    if (pool.isStringValued(keyword)) {
      throw new KernelPoolValidationException(keyword, "Type is string, not numeric.");
    }
    if (!pool.isDoubleValued(keyword)) {
      throw new KernelPoolValidationException(keyword, "Keyword is absent.");
    }
  }

}
