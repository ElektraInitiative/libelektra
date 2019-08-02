use crate::WriteableKey;

pub struct KeyBuilder<T: WriteableKey> {
    key: T,
}

impl<T: WriteableKey> KeyBuilder<T> {
    pub fn new(name: &str) -> KeyBuilder<T> {
        let key = T::new(name).unwrap();
        KeyBuilder { key }
    }

    pub fn value<V: Into<Vec<u8>>>(mut self, key_value: V) -> Self {
        self.key.set_value(key_value);
        self
    }

    pub fn meta(mut self, metaname: &str, metavalue: &str) -> Self {
        self.key.set_meta(metaname, metavalue);
        self
    }

    pub fn build(self) -> T {
        self.key
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{BinaryKey, ReadableKey, StringKey};

    #[test]
    fn can_build_string_key() {
        let name = "user/test/newkey";
        let val = "key_value";
        let key: StringKey = KeyBuilder::new(name).value(val).build();
        assert_eq!(key.get_name(), name);
        assert_eq!(key.get_string(), val);
    }

    #[test]
    fn can_build_binary_key() {
        let name = "user/test/binarykey";
        let val = "😎";
        let key: BinaryKey = KeyBuilder::new(name)
            .value("overwrite me!")
            .value(val)
            .build();
        assert_eq!(key.get_name(), name);
        assert_eq!(key.get_binary(), val.to_owned().into_bytes());
    }

    #[test]
    fn can_build_key_with_meta() {
        let name = "user/test/metatest";
        let key: StringKey = KeyBuilder::new(name)
            .meta("metaname", "metavalue")
            .meta("OWNER", "me")
            .build();
        assert_eq!(key.get_meta("OWNER").unwrap().get_value(), "me");
        assert_eq!(key.get_meta("metaname").unwrap().get_value(), "metavalue");
    }

}
