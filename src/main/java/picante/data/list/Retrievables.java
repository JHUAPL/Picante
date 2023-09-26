package picante.data.list;

import static com.google.common.base.Preconditions.checkElementIndex;
import picante.designpatterns.Writable;

public class Retrievables {

  public <R extends Writable<? super R, R>> Retrievable<R> of(final R value) {
    return new Retrievable<R>() {

      @Override
      public int size() {
        return 1;
      }

      @Override
      public R get(int index, R buffer) {
        checkElementIndex(index, 1);
        return buffer.setTo(value);
      }
    };

  }

  public <S, R extends Writable<S, R>> Retrievable<R> of(@SuppressWarnings("unused") Class<R> clazz,
      final S value) {
    return new Retrievable<R>() {

      @Override
      public int size() {
        return 1;
      }

      @Override
      public R get(int index, R buffer) {
        checkElementIndex(index, 1);
        return buffer.setTo(value);
      }
    };
  }

}
