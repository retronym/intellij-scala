import java.util.*;
public class JavaBox<T> {
  private final T t;
  public JavaBox(T t) { this.t = t; }
  public T get() { return t; }
  public static List<? extends CharSequence> strings() { return Arrays.asList("a"); }
  @SuppressWarnings("rawtypes") public static List raw() { return new ArrayList(); }
  public static <E extends Comparable<? super E>> E max(Collection<? extends E> c) { return Collections.max(c); }
  public static <A> A id(A a) { return a; }
}
