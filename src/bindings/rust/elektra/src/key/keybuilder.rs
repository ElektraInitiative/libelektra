use crate::Key;

pub struct KeyBuilder {
    key: Key,
}

impl KeyBuilder {
    pub fn new(name: &str) -> Self {
        let key = Key::new(name).unwrap();
        KeyBuilder {
            key
        }
    }

    pub fn string(mut self, key_value: &str) -> Self {
        self.key.set_string(key_value);
        self
    }

    pub fn build(self) -> Key {
        self.key
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn can_build_key() {
        let name = "user/test/newkey";
        let val = "key_value";
        let key = KeyBuilder::new(name).string(val).build();
        assert_eq!(key.get_name(), name);
        assert_eq!(key.get_string().unwrap(), val);
    }

}