use crate::{Cursor, KeySet, ReadOnly, ReadableKey, StringKey};

fn next<T: ReadableKey>(
    cursor: &Option<Cursor>,
    keyset: &mut KeySet,
) -> (Option<Cursor>, Option<T>) {
    match cursor {
        None => {
            let key_ptr = unsafe { elektra_sys::ksNext(keyset.as_ptr()) };
            if key_ptr.is_null() {
                (None, None)
            } else {
                let new_cursor = Some(keyset.get_cursor());
                (new_cursor, Some(T::from_ptr(key_ptr)))
            }
        }
        Some(cursor) => {
            let mut new_cursor = Some(cursor + 1);
            keyset.set_cursor(new_cursor.unwrap());
            let key_ptr = unsafe { elektra_sys::ksCurrent(keyset.as_ptr()) };

            if key_ptr.is_null() {
                new_cursor = None;
                (new_cursor, None)
            } else {
                (new_cursor, Some(T::from_ptr(key_ptr)))
            }
        }
    }
}

pub struct ReadOnlyStringKeyIter<'a> {
    pub cursor: Option<Cursor>,
    pub keyset: &'a mut KeySet,
}

impl<'a> Iterator for ReadOnlyStringKeyIter<'a> {
    type Item = ReadOnly<StringKey<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let (new_cursor, item) = next(&self.cursor, &mut self.keyset);
        self.cursor = new_cursor;
        item
    }
}

pub struct StringKeyIter<'a> {
    pub cursor: Option<Cursor>,
    pub keyset: &'a mut KeySet,
}

impl<'a> Iterator for StringKeyIter<'a> {
    type Item = StringKey<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (new_cursor, item) = next(&self.cursor, &mut self.keyset);
        self.cursor = new_cursor;
        item
    }
}
