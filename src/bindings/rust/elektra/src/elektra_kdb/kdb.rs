use super::error;
use crate::{KDBError, KeySet, StringKey, WriteableKey};
use std::ptr::NonNull;

#[derive(Debug)]
pub struct KDB {
    ptr: NonNull<elektra_sys::KDB>,
    phantom: std::marker::PhantomData<elektra_sys::KDB>,
}

impl Drop for KDB {
    fn drop(&mut self) {
        println!("Drop {:?}", self);
        let mut err_key = StringKey::new_empty();
        unsafe {
            elektra_sys::kdbClose(self.as_ptr(), err_key.as_ptr());
        }
    }
}

impl KDB {
    /// Opens the session with the Key database.
    pub fn open<'a>() -> Result<Self, KDBError<'a>> {
        let mut error_key = StringKey::new_empty();
        let kdb_ptr = unsafe { elektra_sys::kdbOpen(error_key.as_ptr()) };

        if kdb_ptr.is_null() {
            Err(error::map_kdb_error(error_key))
        } else {
            Ok(KDB {
                ptr: unsafe { NonNull::new_unchecked(kdb_ptr) },
                phantom: std::marker::PhantomData,
            })
        }
    }

    /// Retrieve keys in an atomic and universal way.
    /// Note that the provided keyset is modified and contains the result.
    /// The return value is true, if the keys were successfully retrieved
    /// and false if there were no changes to the keyset.
    pub fn get<'a>(
        &mut self,
        keyset: &mut KeySet,
        key: &mut StringKey<'a>,
    ) -> Result<bool, KDBError<'a>> {
        let ret_val = unsafe { elektra_sys::kdbGet(self.as_ptr(), keyset.as_ptr(), key.as_ptr()) };

        if ret_val == 1 {
            Ok(true)
        } else if ret_val == 0 {
            Ok(false)
        } else {
            Err(error::map_kdb_error(key.duplicate()))
        }
    }

    /// Set keys in an atomic and universal way.
    /// The return value is true on success,
    /// and false if there were no changes to the KDB.
    /// # Notes
    /// You have to call [`get`] with `keyset` first.
    ///
    /// [`get`]: #method.get
    pub fn set<'a>(
        &mut self,
        keyset: &mut KeySet,
        key: &mut StringKey<'a>,
    ) -> Result<bool, KDBError<'a>> {
        let ret_val = unsafe { elektra_sys::kdbSet(self.as_ptr(), keyset.as_ptr(), key.as_ptr()) };

        if ret_val == 1 {
            Ok(true)
        } else if ret_val == 0 {
            Ok(false)
        } else {
            Err(error::map_kdb_error(key.duplicate()))
        }
    }

    /// This method can be used the given KDB handle meets certain clauses, specified in contract.
    /// The return value is true on success,
    /// and false if clauses of the contract are unmet.
    pub fn ensure<'a>(
        &mut self,
        keyset: &mut KeySet,
        key: &mut StringKey<'a>,
    ) -> Result<bool, KDBError<'a>> {
        let ret_val =
            unsafe { elektra_sys::kdbEnsure(self.as_ptr(), keyset.as_ptr(), key.as_ptr()) };

        if ret_val == 0 {
            Ok(true)
        } else if ret_val == 1 {
            Ok(false)
        } else {
            Err(error::map_kdb_error(key.duplicate()))
        }
    }

    pub fn as_ptr(&mut self) -> *mut elektra_sys::KDB {
        self.ptr.as_ptr()
    }
}

impl AsRef<elektra_sys::KDB> for KDB {
    fn as_ref(&self) -> &elektra_sys::KDB {
        unsafe { self.ptr.as_ref() }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn can_open_kdb() {
        let result = KDB::open();
        assert!(result.is_ok());
    }
}
