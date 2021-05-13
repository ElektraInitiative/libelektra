package org.libelektra;

import java.lang.ref.Cleaner;

import org.libelektra.exception.KeyReleasedException;
import org.libelektra.exception.KeySetReleasedException;

import com.sun.jna.Pointer;

/**
 * Reference clean-up helper for Java representations of Elektra entities with references to native resources
 */
class ReferenceCleaner {

	private static final Cleaner CLEANER_INSTANCE = Cleaner.create();

	/**
	 * Registers a {@link Key} for informing the underlying native library about the
	 * release of the key set reference as soon as the specified {@code key} becomes
	 * phantom reachable.
	 * 
	 * @param key {@link Key} to be cleaned up
	 * @return {@link Cleaner.Cleanable} for releasing the resource manually before garbage collection
	 * @throws KeyReleasedException    if {@code key} has already been released
	 */
	static Cleaner.Cleanable registerKeyCleanUp(Key key) {
		return CLEANER_INSTANCE.register(key, new KeyCleanupTask(key.getPointer()));
	}

	/**
	 * Registers a {@link KeySet} for informing the underlying native library about
	 * the release of the key set reference as soon as the specified {@code keySet}
	 * becomes phantom reachable.
	 * 
	 * @param keySet {@link KeySet} to be cleaned up
	 * @return {@link Cleaner.Cleanable} for releasing the resource manually before garbage collection
	 * @throws KeySetReleasedException if {@code keySet} has already been released
	 */
	static Cleaner.Cleanable registerKeySetCleanUp(KeySet keySet) {
		return CLEANER_INSTANCE.register(keySet, new KeySetCleanupTask(keySet.getPointer()));
	}

	private static class KeyCleanupTask implements Runnable {

		private final Pointer keyPointer;

		private KeyCleanupTask(Pointer keyPointer) {
			this.keyPointer = keyPointer;
		}

		@Override
		public void run() {
			releaseKey();
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
			System.out.println(String.format("%nRelease called for native key pointer %s - RefCount: %d will be decreased ...",
					keyPointer.toString(),
					Elektra.INSTANCE.keyGetRef(keyPointer)));

			Elektra.INSTANCE.keyDecRef(keyPointer);

			System.out.println(String.format("%nTrying to delete native key pointer %s - RefCount: %d",
					keyPointer.toString(),
					Elektra.INSTANCE.keyGetRef(keyPointer)));

			int result = Elektra.INSTANCE.keyDel(keyPointer);

			System.out.println(String.format("%nTried to deleted native key pointer %s with result: %d",
					keyPointer.toString(), result));
			
			return (result == 0);
		}

	}

	private static class KeySetCleanupTask implements Runnable {

		private final Pointer keySetPointer;

		private KeySetCleanupTask(Pointer keySetPointer) {
			this.keySetPointer = keySetPointer;
		}

		@Override
		public void run() {
			releaseKeySet();
		}
		
		/**
		 * Clean-up method to release key set reference by trying to free the native reference
		 * 
		 * @return True, if the native reference actually has been freed
		 */
		private boolean releaseKeySet() {
			System.out.println(String.format("%nRelease called for native key set pointer %s ...", 
					keySetPointer.toString()));

			int result = Elektra.INSTANCE.ksDel(keySetPointer);

			System.out.println(String.format("%nTried to deleted native key set pointer %s with result: %d",
					keySetPointer.toString(), result));
			
			return (result == 0);
		}

	}

	private ReferenceCleaner() {
		// intentionally left blank
	}

}
