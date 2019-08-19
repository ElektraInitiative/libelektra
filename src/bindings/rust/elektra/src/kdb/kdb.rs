use super::error;
use crate::{KDBError, StringKey, WriteableKey};
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
    /// In case of a failure, a KDBError is returned.
    pub fn open() -> Result<Self, KDBError> {
        let mut error_key = StringKey::new_empty();
        let kdb_ptr = unsafe { elektra_sys::kdbOpen(error_key.as_ptr()) };

        if kdb_ptr as *const elektra_sys::KDB == std::ptr::null() {
            Err(error::map_kdb_error(error_key))
        } else {
            Ok(KDB {
                ptr: unsafe { NonNull::new_unchecked(kdb_ptr) },
                phantom: std::marker::PhantomData,
            })
        }
    }

    pub fn as_ptr(&mut self) -> *mut elektra_sys::KDB {
        self.ptr.as_ptr()
    }

    pub fn as_ref(&self) -> &elektra_sys::KDB {
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
