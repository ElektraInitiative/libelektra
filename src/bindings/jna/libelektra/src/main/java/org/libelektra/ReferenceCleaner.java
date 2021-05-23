package org.libelektra;

import java.lang.ref.Cleaner;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import javax.annotation.Nullable;

import org.libelektra.exception.KeyReleasedException;
import org.libelektra.exception.KeySetReleasedException;

import com.sun.jna.Pointer;

/**
 * Reference clean-up helper for Java representations of Elektra entities with
 * references to native resources
 */
class ReferenceCleaner {

	/**
	 * #3825 TOD This constant can be used to disable automated native reference
	 * clean-up and is intended to be removed after no more occasional segfailts
	 * have appeared in ci for some time.
	 */
	@Deprecated(forRemoval = true)
	private static final boolean ENABLE_AUTO_NATIVE_REF_CLEANUP = true;

	@Nullable
	private static final Cleaner CLEANER_INSTANCE = ENABLE_AUTO_NATIVE_REF_CLEANUP ? Cleaner.create() : null;

	/**
	 * The garbage collector may call multiple cleanables in parallel. This
	 * {@link ReadWriteLock} is being used to prevent multiple key sets being
	 * released in parallel and also to prevent keys being released parallel to key
	 * sets. This is done in order to prevent a possible race condition when
	 * releasing keys also being part of a key set being released.
	 */
	private static final ReentrantReadWriteLock lock = new ReentrantReadWriteLock();

	/**
	 * Depending on whether {@link #ENABLE_AUTO_NATIVE_REF_CLEANUP} is {@code true},
	 * {@link Key#incRef()} is called for {@code newKey}.
	 *
	 * @param newKey Newly created {@link Key} object wrapping native key resource.
	 */
	static void keyWrapperCreated(Key newKey) {
		if (ENABLE_AUTO_NATIVE_REF_CLEANUP) {
			newKey.incRef();
		}
	}

	/**
	 * Registers a {@link Key} for informing the underlying native library about the
	 * release of the key set reference as soon as the specified {@code key} becomes
	 * phantom reachable.
	 *
	 * @param key {@link Key} to be cleaned up
	 * @return {@link Cleaner.Cleanable} for releasing the resource manually before
	 *         garbage collection
	 * @throws KeyReleasedException if {@code key} has already been released
	 */
	static Cleaner.Cleanable registerKeyCleanUp(Key key) {
		KeyCleanupTask task = new KeyCleanupTask(key.getPointer());
		return ENABLE_AUTO_NATIVE_REF_CLEANUP ? CLEANER_INSTANCE.register(key, task) : task::run;
	}

	/**
	 * Registers a {@link KeySet} for informing the underlying native library about
	 * the release of the key set reference as soon as the specified {@code keySet}
	 * becomes phantom reachable.
	 *
	 * @param keySet {@link KeySet} to be cleaned up
	 * @return {@link Cleaner.Cleanable} for releasing the resource manually before
	 *         garbage collection
	 * @throws KeySetReleasedException if {@code keySet} has already been released
	 */
	static Cleaner.Cleanable registerKeySetCleanUp(KeySet keySet) {
		KeySetCleanupTask task = new KeySetCleanupTask(keySet.getPointer());
		return ENABLE_AUTO_NATIVE_REF_CLEANUP ? CLEANER_INSTANCE.register(keySet, task) : task::run;
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
		 * Clean-up method to release key reference by first decrementing its reference
		 * counter and then trying to free the native reference
		 *
		 * @return True, if the native reference actually has been freed, false if there
		 *         are still other references to the native resource and therefore it
		 *         was not freed
		 */
		private boolean releaseKey() {
			if (ENABLE_AUTO_NATIVE_REF_CLEANUP) {
				Elektra.INSTANCE.keyDecRef(keyPointer);
			}
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
		 * Clean-up method to release key set reference by trying to free the native
		 * reference
		 *
		 * @return True, if the native reference actually has been freed
		 */
		private boolean releaseKeySet() {
			return (Elektra.INSTANCE.ksDel(keySetPointer) == 0);
		}
	}

	private ReferenceCleaner() {
		// intentionally left blank to prevent instantiation
	}
}
