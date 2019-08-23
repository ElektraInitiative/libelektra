use crate::{KeyError, WriteableKey};

pub struct KeyBuilder<T: WriteableKey> {
    key: T,
}

impl<T: WriteableKey> KeyBuilder<T> {
    /// Construct a new key with a name.
    ///
    /// # Panics
    /// Panics when an allocation error (out of memory) in the C-constructor occurs.
    pub fn new(name: &str) -> Result<Self, KeyError> {
        let key = T::new(name)?;
        Ok(KeyBuilder { key })
    }
    /// Construct a new nameless key.
    ///
    /// # Panics
    /// Panics when an allocation error (out of memory) in the C-constructor occurs.
    pub fn new_empty() -> Self {
        let key = T::new_empty();
        KeyBuilder { key }
    }

    /// Set the name of a key. Must adhere to the rules for keynames otherwise an error is returned.
    /// Returns the size in bytes of this new key name including the ending NUL.
    ///
    /// # Panics
    /// Panics if the provided string contains interior nul bytes.
    pub fn name(mut self, name: &str) -> Result<Self, KeyError> {
        self.key.set_name(name)?;
        Ok(self)
    }

    /// Set the value of the key.
    pub fn value(mut self, key_value: T::SetValue) -> Self {
        self.key.set_value(key_value);
        self
    }

    /// Set a new meta-information.
    /// Returns a `KeyError::InvalidName` if the name is invalid.
    ///
    /// # Panics
    /// Panics if any of the provided strings contains interior nul bytes.
    pub fn meta(mut self, metaname: &str, metavalue: &str) -> Result<Self, KeyError> {
        self.key.set_meta(metaname, metavalue)?;
        Ok(self)
    }

    /// Consumes the KeyBuilder and returns the key.
    ///
    /// # Examples
    /// ```
    /// # use elektra::{KeyBuilder,StringKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let key: StringKey = KeyBuilder::new("user/test/newkey")?
    ///     .value("key_value")
    ///     .build();
    /// assert_eq!(key.name(), "user/test/newkey");
    /// assert_eq!(key.value(), "key_value");
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    pub fn build(self) -> T {
        self.key
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{BinaryKey, ReadableKey, StringKey};

    #[test]
    fn can_build_string_key() -> Result<(), KeyError> {
        let name = "user/test/newkey";
        let val = "key_value";
        let key: StringKey = KeyBuilder::new(name)?.value(val).build();
        assert_eq!(key.name(), name);
        assert_eq!(key.value(), val);
        Ok(())
    }

    #[test]
    fn can_build_binary_key() -> Result<(), KeyError> {
        let name = "user/test/binarykey";
        let overwrite = "overwrite me";
        let val = "😎";
        let key: BinaryKey = KeyBuilder::new(name)?
            .value(overwrite.as_bytes())
            .value(val.as_bytes())
            .build();
        assert_eq!(key.name(), name);
        assert_eq!(key.value(), val.to_owned().into_bytes());
        Ok(())
    }

    #[test]
    fn can_build_key_with_meta() -> Result<(), KeyError> {
        let name = "user/test/metatest";
        let key: StringKey = KeyBuilder::new(name)?
            .meta("metaname", "metavalue")?
            .meta("OWNER", "me")?
            .build();
        assert_eq!(key.meta("OWNER").unwrap().value(), "me");
        assert_eq!(key.meta("metaname").unwrap().value(), "metavalue");
        Ok(())
    }

}
