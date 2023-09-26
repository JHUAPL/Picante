package picante.math.functions;

public class DifferentiableUnivariateFunctionAdaptorToComputable
    implements DifferentiableUnivariateFunction, Computable {

  public static class Result implements Computable.ComputableResult {
    private final double evaluated;
    private final double differentiated;

    protected Result(double evaluated, double differentiated) {
      this.evaluated = evaluated;
      this.differentiated = differentiated;
    }

    public double evaluated() {
      return evaluated;
    }

    public double differentiated() {
      return differentiated;
    }

  }

  private final DifferentiableUnivariateFunction function;

  public DifferentiableUnivariateFunctionAdaptorToComputable(
      DifferentiableUnivariateFunction function) {
    this.function = function;
  }

  @Override
  public double evaluate(double t) {
    return function.evaluate(t);
  }

  @Override
  public double differentiate(double t) {
    return function.differentiate(t);
  }

  @Override
  public Result compute(double t) {
    return new Result(evaluate(t), differentiate(t));
  }

}
