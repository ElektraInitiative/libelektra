package org.libelektra;

import com.sun.jna.Pointer;
import java.util.function.Function;
import java.util.function.Supplier;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/** Utility class for parameter and native API return value validation */
class ValidationUtil {

  /**
   * Ensures the {@code subject} not to be {@code null}
   *
   * @param <T> Actual type of the {@code subject}
   * @param subject Subject to be validated
   * @param argHint Hint to be included in the exception message
   * @return Subject to be validated, enabling a fluent interface
   * @throws IllegalArgumentException if {@code subject} is {@code null}
   */
  @Nonnull
  static <T> T argNotNull(@Nullable T subject, String argHint) {
    if (subject == null) {
      throw new IllegalArgumentException(String.format("Argument cannot be null: %s", argHint));
    }
    return subject;
  }

  /**
   * Ensures the {@code subject} not to be {@code null} of {@link String#isBlank}
   *
   * @param subject Subject to be validated
   * @param argHint Hint to be included in the exception message
   * @return Subject to be validated, enabling a fluent interface
   * @throws IllegalArgumentException if {@code subject} is {@code null}
   */
  @Nonnull
  static String argNotNullOrBlank(@Nullable String subject, String argHint) {
    String nonNullSubject = argNotNull(subject, argHint);
    if (nonNullSubject.isBlank()) {
      throw new IllegalArgumentException(String.format("Argument cannot be blank: %s", argHint));
    }
    return subject;
  }

  /**
   * Creates a key associated with the specified JNA {@link Pointer pointer} or throws an exception
   * provided by the specified {@code exceptionSupplier}
   *
   * @param <T> Actual type of the return value
   * @param pointer JNA pointer
   * @param factory Factory for return value consuming {@code pointer}
   * @param exceptionSupplier Exception factory
   * @return New key object associated with the specified JNA {@link Pointer pointer}
   */
  @Nonnull
  static <T> T checkPointer(
      @Nullable Pointer pointer,
      Function<Pointer, T> factory,
      Supplier<RuntimeException> exceptionSupplier) {
    if (pointer == null) {
      throw exceptionSupplier.get();
    }
    return factory.apply(pointer);
  }

  private ValidationUtil() {
    // intentionally left blank to prevent instantiation
  }
}
