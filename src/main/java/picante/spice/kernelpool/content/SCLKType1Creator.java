package picante.spice.kernelpool.content;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Predicate;
import com.google.common.base.Predicates;
import picante.exceptions.BugException;
import picante.spice.Utilities;
import picante.spice.kernel.tk.lsk.UniformTimeProvider;
import picante.spice.kernel.tk.sclk.SCLKInstantiationException;
import picante.spice.kernel.tk.sclk.SCLKType1;

class SCLKType1Creator {

  /**
   * Filter that accepts only integral valued doubles. This is not perfectly workable, as for large
   * enough doubles, they are necessarily integral and this will pass regardless.
   */
  @VisibleForTesting
  static final Predicate<Double> INTEGRAL_FILTER = new Predicate<Double>() {

    @Override
    public boolean apply(Double input) {
      double value = Utilities.round(input);

      return value == input;
    }
  };

  /**
   * Filter that the MODULI components of a type 1 SCLK must pass.
   */
  @VisibleForTesting
  static final Predicate<Double> GREATER_THAN_OR_EQUAL_TO_ONE = new Predicate<Double>() {

    @Override
    public boolean apply(Double input) {
      return input >= 1.0;
    }
  };

  /**
   * Additional filter that the MODULI components should pass.
   */
  @VisibleForTesting
  static final Predicate<Double> MODULI_VALIDATION =
      Predicates.and(INTEGRAL_FILTER, GREATER_THAN_OR_EQUAL_TO_ONE);

  /**
   * Set of acceptable values utilized by the SCLK system to identify the parallel time system.
   */
  @VisibleForTesting
  static final Set<Integer> ACCEPTABLE_TIME_SYSTEM_VALUES = new HashSet<Integer>();
  static {
    ACCEPTABLE_TIME_SYSTEM_VALUES.add(1);
    ACCEPTABLE_TIME_SYSTEM_VALUES.add(2);
  }

  /**
   * Creates a type 1 SCLK without the extra validation that will result in conversions that behave
   * in the same manner that NAIFs software behaves in the event of pathological input.
   * 
   * @param id
   * @param retriever
   * @param tdbConverter
   * @param tdtConverter
   * @return
   * @throws KernelPoolValidationException
   * @throws SCLKInstantiationException
   */
  SCLKType1 createForgiving(int id, KernelPoolValidatingRetriever retriever,
      UniformTimeProvider tdbConverter, UniformTimeProvider tdtConverter)
      throws KernelPoolValidationException, SCLKInstantiationException {

    /*
     * Use only what NAIF applies with regards to kernel pool content validation.
     */
    List<Double> moduli = retriever.getDoublesWithValueValidation("SCLK01_MODULI_" + id,
        GREATER_THAN_OR_EQUAL_TO_ONE);

    return new SCLKType1(-id, chooseConverter(id, retriever, tdbConverter, tdtConverter),
        new SCLKPartitionTable(-id, retriever.getDoubles("SCLK_PARTITION_START_" + id),
            retriever.getDoubles("SCLK_PARTITION_END_" + id)),
        moduli, retriever.getDoubles("SCLK01_OFFSETS_" + id),
        new SCLKType1ListBasedRecordTable(
            retriever.getDoublesLengthModulo("SCLK01_COEFFICIENTS_" + id, 3),
            computeTicksPerMostSignificantCount(moduli)));
  }

  /**
   * Creates a type 1 SCLK with extra validation that NAIF does not perform in their SCLK system,
   * but that all valid type 1 SCLK kernel content should pass.
   * 
   * @param id
   * @param retriever
   * @param tdbConverter
   * @param tdtConverter
   * @return
   * @throws KernelPoolValidationException
   * @throws SCLKInstantiationException
   */
  SCLKType1 create(int id, KernelPoolValidatingRetriever retriever,
      UniformTimeProvider tdbConverter, UniformTimeProvider tdtConverter)
      throws KernelPoolValidationException, SCLKInstantiationException {

    List<Double> moduli =
        retriever.getDoublesWithValueValidation("SCLK01_MODULI_" + id, MODULI_VALIDATION);

    return new SCLKType1(-id, chooseConverter(id, retriever, tdbConverter, tdtConverter),
        new SCLKPartitionTable(-id, retriever.getDoubles("SCLK_PARTITION_START_" + id),
            retriever.getDoubles("SCLK_PARTITION_END_" + id)),
        moduli, retriever.getDoublesWithValueValidation("SCLK01_OFFSETS_" + id, INTEGRAL_FILTER),
        new SCLKType1ListBasedRecordTable(
            retriever.getDoublesLengthModulo("SCLK01_COEFFICIENTS_" + id, 3),
            computeTicksPerMostSignificantCount(moduli)));
  }

  @VisibleForTesting
  static double computeTicksPerMostSignificantCount(List<Double> tickModulos) {

    /*
     * Since we are interested in the number of minor ticks in one major tick, we can ignore the
     * first entry in the moduli array.
     */
    double result = 1.0;
    for (int i = tickModulos.size() - 1; i > 0; i--) {
      result *= tickModulos.get(i);
    }

    return result;
  }

  @VisibleForTesting
  static UniformTimeProvider chooseConverter(int id, KernelPoolValidatingRetriever retriever,
      UniformTimeProvider tdbConverter, UniformTimeProvider tdtConverter)
      throws KernelPoolValidationException {

    /*
     * First, determine if the time system keyword is even present. If it isn't, then assume the TDB
     * converter by default.
     */
    String keyword = "SCLK01_TIME_SYSTEM_" + id;
    if (retriever.containsKeyword(keyword)) {
      switch (retriever.getInteger(keyword, ACCEPTABLE_TIME_SYSTEM_VALUES)) {
        case 1:
          return tdbConverter;
        case 2:
          return tdtConverter;
        default:
          throw new BugException("This should never happen in normal" + " execution of the code.");
      }
    }
    return tdbConverter;
  }

}
