package org.libelektra;

import com.sun.jna.Pointer;
import java.lang.ref.Cleaner;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import javax.annotation.Nonnull;

/**
 * Reference clean-up helper for Java representations with references to native Elektra resources
 */
class ReferenceCleaner {

  private static final Cleaner CLEANER_INSTANCE = Cleaner.create();

  /**
   * The garbage collector may call multiple cleanables in parallel. This {@link ReadWriteLock} is
   * being used to prevent multiple key sets being released in parallel and also to prevent keys
   * being released parallel to key sets. This is done in order to prevent a possible race condition
   * when releasing keys also being part of a key set being released.
   */
  private static final ReentrantReadWriteLock lock = new ReentrantReadWriteLock();

  /**
   * Registers a {@link Key} for informing the underlying native library about the release of the
   * key set reference as soon as the specified {@code key} becomes phantom reachable.
   *
   * <p>{@link Elektra#keyIncRef(Pointer)} is called for {@code key}.
   *
   * @param key {@link Key} to be cleaned up
   * @return {@link Cleaner.Cleanable} for releasing the resource manually before garbage collection
   * @throws IllegalStateException if {@code key} has already been released
   */
  @Nonnull
  static Cleaner.Cleanable registerKeyCleanUp(ReadableKey key) {
    Pointer keyPointer = key.getPointer();
    Elektra.INSTANCE.keyIncRef(keyPointer);
    KeyCleanupTask task = new KeyCleanupTask(keyPointer);
    return CLEANER_INSTANCE.register(key, task);
  }

  /**
   * Registers a {@link KeySet} for informing the underlying native library about the release of the
   * key set reference as soon as the specified {@code keySet} becomes phantom reachable.
   *
   * <p>{@link Elektra#ksIncRef(Pointer)} is called for {@code keySet}.
   *
   * @param keySet {@link KeySet} to be cleaned up
   * @return {@link Cleaner.Cleanable} for releasing the resource manually before garbage collection
   * @throws IllegalStateException if {@code keySet} has already been released
   */
  @Nonnull
  static Cleaner.Cleanable registerKeySetCleanUp(KeySet keySet) {
    Pointer keySetPointer = keySet.getPointer();
    Elektra.INSTANCE.ksIncRef(keySetPointer);
    KeySetCleanupTask task = new KeySetCleanupTask(keySetPointer);
    return CLEANER_INSTANCE.register(keySet, task);
  }

  private static class KeyCleanupTask implements Runnable {

    private final Pointer keyPointer;

    private KeyCleanupTask(Pointer keyPointer) {
      this.keyPointer = keyPointer;
    }

    @Override
    public void run() {
      try {
        lock.readLock().lockInterruptibly();
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
        throw new RuntimeException("Aquisition of key release lock has been interrupted.", e);
      }
      releaseKey();
      lock.readLock().unlock();
    }

    /**
     * Clean-up method to release key reference by first decrementing its reference counter and then
     * trying to free the native reference
     *
     * @return True, if the native reference actually has been freed, false if there are still other
     *     references to the native resource and therefore it was not freed
     */
    private boolean releaseKey() {
      Elektra.INSTANCE.keyDecRef(keyPointer);
      return (Elektra.INSTANCE.keyDel(keyPointer) == 0);
    }
  }

  private static class KeySetCleanupTask implements Runnable {

    private final Pointer keySetPointer;

    private KeySetCleanupTask(Pointer keySetPointer) {
      this.keySetPointer = keySetPointer;
    }

    @Override
    public void run() {
      try {
        lock.writeLock().lockInterruptibly();
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
        throw new RuntimeException("Aquisition of key set release lock has been interrupted.", e);
      }
      releaseKeySet();
      lock.writeLock().unlock();
    }

    /**
     * Clean-up method to release key set reference by trying to free the native reference
     *
     * @return True, if the native reference actually has been freed
     */
    private boolean releaseKeySet() {
      Elektra.INSTANCE.ksDecRef(keySetPointer);
      return (Elektra.INSTANCE.ksDel(keySetPointer) == 0);
    }
  }

  private ReferenceCleaner() {
    // intentionally left blank to prevent instantiation
  }
}
