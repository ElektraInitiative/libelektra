use std::cmp::Ordering;
use std::convert::TryInto;
use indexmap::{IndexSet};
use std::iter::FromIterator;
use std::str::FromStr;
use std::hash::{Hash, Hasher};

use enumflags2::{bitflags, BitFlags};

use relative_path::{RelativePath, RelativePathBuf};

#[derive(Copy, Clone, PartialEq, Debug, Hash)]
pub enum KeyNamespace {
    None,
    Cascading,
    Meta,
    Spec,
    Proc,
    Dir,
    User,
    System,
    Default,
}

pub enum KeyNamespaceError {
    InvalidNamespaceError
}

impl FromStr for KeyNamespace {
    type Err = KeyNamespaceError;

    fn from_str(namespace: &str) -> Result<Self, Self::Err> {
        match namespace {
            "meta" => Ok(KeyNamespace::Meta),
            "user" => Ok(KeyNamespace::User),
            _ => Ok(KeyNamespace::Default),
        }
    }
}

impl ToString for KeyNamespace {
    fn to_string(&self) -> String {
        let namespace = match self {
            KeyNamespace::Meta => "meta",
            KeyNamespace::User => "user",
            _ => "test",
        };

        namespace.to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct KeyName {
    namespace: KeyNamespace,
    path: RelativePathBuf,
}

impl KeyName {
    pub fn new (namespace: KeyNamespace, path: RelativePathBuf) -> KeyName {
        KeyName {
            namespace,
            path
        }
    }

    pub fn base_name(&self) -> Option<&str> {
        self.path.file_name()
    }

    pub fn set_base_name(&mut self, base_name: &str) {
        self.path.set_file_name(base_name);
    }

    pub fn append_name(&mut self, name: &str) {
        self.path = self.path.join(RelativePath::new(name));
    }

    pub fn namespace(&self) -> KeyNamespace {
        self.namespace
    }

    pub fn set_namespace(&mut self, namespace: KeyNamespace) {
        self.namespace = namespace
    }

    pub fn path(&self) -> &RelativePathBuf {
        &self.path
    }

    pub fn is_below_or_same(&self, other: &KeyName) -> bool {
        if self.namespace != other.namespace {
            return false;
        }

        let path = self.path().normalize();
        let other_path = other.path().normalize();

        self.is_below(&other) || path.eq(&other_path)
    }

    pub fn is_below(&self, other: &KeyName) -> bool {
        if self.namespace != other.namespace {
            return false;
        }

        let path = self.path().normalize();
        let other_path = other.path().normalize();

        other_path.starts_with(&path) == false
            && path.starts_with(&other_path) == true
    }

    pub fn is_directly_below(&self, other: &KeyName) -> bool {
        if self.namespace != other.namespace {
            return false;
        }

        let path = self.path().normalize();
        let other_path = other.path().normalize();

        if let Some(parent) = path.parent() {
            return other_path.eq(&parent)
        }

        return false;
    }
}

impl FromStr for KeyName {
    type Err = KeyError;

    fn from_str(name: &str) -> Result<Self, Self::Err> {
        let mut splitter = name.splitn(2, ":");

        let namespace = splitter.next()
            .ok_or(KeyError::InvalidNameError)?;

        let path = splitter.next()
            .ok_or(KeyError::InvalidNameError)?;

        let key_namespace = KeyNamespace::from_str(namespace)
            .or(Err(KeyError::InvalidNameError))?;

        Ok(KeyName::new(
            key_namespace,
            RelativePathBuf::from(path).normalize(),
        ))
    }
}

impl ToString for KeyName {
    fn to_string(&self) -> String {
        let mut name = self.namespace.to_string();
        name.push_str(":/");
        name.push_str(self.path.as_str());

        name
    }
}

#[bitflags(default = NAME | VALUE | META)]
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum KeyCopyFlags {
    NAME = 1,
    VALUE = 1<<1,
    META = 1<<2,
}

#[derive(Debug)]
pub enum KeyError {
    InvalidNameError,
    NullPointerError,
}

#[derive(Debug, Clone)]
pub struct Key {
    name: KeyName,
    value: Option<Vec<u8>>,
    meta: KeySet,
}

impl Eq for Key {}

impl PartialEq<Self> for Key {
    fn eq(&self, other: &Self) -> bool {
        self.name.path == other.name.path
    }
}

impl PartialOrd<Self> for Key {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.name.path.cmp(&other.name.path))
    }
}

impl Ord for Key {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.path.cmp(&other.name.path)
    }
}

impl Hash for Key {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.name.to_string().hash(state)
    }
}

impl Key {
    pub fn new(key_name: KeyName) -> Key {
        Key {
            name: key_name,
            value: None,
            meta: KeySet::default(),
        }
    }

    pub fn name(&self) -> &KeyName {
        &self.name
    }

    pub fn name_mut(&mut self) -> &mut KeyName {
        &mut self.name
    }

    pub fn set_name(&mut self, name: &str) -> Result<(), KeyError> {
        if let Ok(name) = KeyName::from_str(name) {
            self.name = name;
            Ok(())
        } else {
            Err(KeyError::InvalidNameError)
        }
    }

    pub fn set_keyname(&mut self, name: KeyName) {
        self.name = name;
    }

    pub fn set_value(&mut self, value: &[u8]) {
        self.value = Some(value.to_vec());
    }

    pub fn set_value_str(&mut self, value: &str) {
        self.value = Some(value.as_bytes().to_vec())
    }

    pub fn value(&self) -> Option<&[u8]> {
        match &self.value {
            Some(value) => Some(value.as_slice()),
            None => None,
        }
    }

    pub fn value_to_string(&self) -> Option<String> {
        if let Some(value) = &self.value {
            Some(String::from_utf8_lossy(value).to_string())
        } else {
            None
        }
    }

    pub fn meta(&self) -> &KeySet {
        &self.meta
    }

    pub fn meta_mut(&mut self) -> &mut KeySet {
        &mut self.meta
    }

    pub fn set_meta(&mut self, meta: KeySet) {
        self.meta = meta;
    }

    pub fn copy_from(&mut self, other: &Key, copy_flags: BitFlags<KeyCopyFlags>) {
        if copy_flags.contains(KeyCopyFlags::NAME) {
            self.name = other.name.clone()
        }

        if copy_flags.contains(KeyCopyFlags::VALUE) {
            if let Some(value) = other.value() {
                self.value = Some(value.clone().to_vec())
            } else {
                self.value = None
            }
        }

        if copy_flags.contains(KeyCopyFlags::META) {
            self.meta = (*other.meta()).clone()
        }
    }
}

impl FromStr for Key {
    type Err = KeyError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let key_name = KeyName::from_str(s)?;
        Ok(Key::new(key_name))
    }
}

pub struct KeyBuilder {
    name: KeyName,
    value: Option<Vec<u8>>,
    meta: Option<KeySet>,
}

impl KeyBuilder {
    pub fn new(key_name: KeyName) -> KeyBuilder {
        KeyBuilder {
            name: key_name,
            value: None,
            meta: None,
        }
    }

    pub fn value(mut self, value: &[u8]) -> KeyBuilder {
        self.value = Some(value.to_vec());
        self
    }

    pub fn meta(mut self, meta: KeySet) -> KeyBuilder {
        self.meta = Some(meta);
        self
    }

    pub fn build(self) -> Result<Key, KeyError> {
        let mut key = Key::new(self.name);

        if let Some(value) = self.value {
            key.set_value(&value);
        }

        if let Some(meta) = self.meta {
            key.set_meta(meta);
        }

        Ok(key)
    }
}

impl FromStr for KeyBuilder {
    type Err = KeyError;

    fn from_str(name: &str) -> Result<Self, Self::Err> {
        let key_name = KeyName::from_str(name)?;

        Ok(KeyBuilder {
            name: key_name,
            value: None,
            meta: None,
        })
    }
}

#[derive(Debug, Clone)]
pub struct KeySet {
    keys: IndexSet<Key>,
    refs: u16,
}

impl KeySet {
    pub fn len(&self) -> usize {
        self.keys.len()
    }

    pub fn append(&mut self, key: Key) {
        self.keys.insert(key);
    }

    pub fn append_all(&mut self, keyset: &KeySet) {
        for key in keyset.values() {
            // todo: improve (pop / remove?)
            self.append((*key).clone());
        }
    }

    pub fn clear(&mut self) {
        self.keys.clear()
    }

    pub fn take(&mut self, name: &str) -> Option<Key> {
        return match Key::from_str(name) {
            Ok(x) => self.keys.take(&x),
            Err(_) => None
        };
    }

    pub fn lookup_key(&self, key: &Key) -> Option<&Key> {
        self.keys.get(key)
    }

    pub fn lookup(&self, name: &str) -> Option<&Key> {
        if let Ok(key) = Key::from_str(name) {
            return self.lookup_key(&key);
        }

        None
    }

    pub fn get(&self, index: isize) -> Option<&Key> {
        self.keys.get_index(index.try_into().unwrap())
    }

    pub fn remove(&mut self, index: isize) -> Option<Key> {
        self.keys.shift_remove_index(index.try_into().unwrap())
    }

    pub fn values(&self) -> indexmap::set::Iter<Key> {
        self.keys.iter()
    }

    pub fn reference_counter(&self) -> u16 {
        self.refs
    }

    pub fn increase_reference_counter(&mut self) -> u16 {
        if self.refs == u16::MAX {
            return self.refs;
        }

        self.refs = self.refs + 1;
        self.refs
    }

    pub fn decrease_reference_counter(&mut self) -> u16 {
        if self.refs == 0 {
            return self.refs;
        }

        self.refs = self.refs - 1;
        self.refs
    }

    pub fn set_reference_counter(&mut self, n: u16) -> u16 {
        self.refs = n;
        self.refs
    }
}

impl Default for KeySet {
    fn default() -> KeySet {
        KeySet {
            keys: IndexSet::new(),
            refs: 0,
        }
    }
}

impl FromIterator<Key> for KeySet {
    fn from_iter<T: IntoIterator<Item=Key>>(iter: T) -> Self {
        let mut ks = KeySet::default();

        for key in iter {
            ks.append(key);
        }

        ks
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_key() {
        let mut key = Key::new(
            KeyName::from_str("user:/test/qwe/asd").unwrap()
        );

        assert_eq!(key.name().to_string(), "user:/test/qwe/asd");

        key.name_mut().set_base_name("zxc");
        assert_eq!(key.name().to_string(), "user:/test/qwe/zxc");

        key.set_name("user:/asd").expect("should succeed");
        assert_eq!(key.name().to_string(), "user:/asd");

        let key_name = KeyName::from_str("user:/test/qwe/asd").unwrap();
        key.set_keyname(key_name);
        assert_eq!(key.name().to_string(), "user:/test/qwe/asd");
    }


    #[test]
    fn test_key_ord() {
        let key1 = Key::new(
            KeyName::from_str("user:/test/qwe/asd").unwrap()
        );

        let key2 = Key::new(
            KeyName::from_str("user:/test/qwe/asd/zxc").unwrap()
        );

        let key3 = Key::new(
            KeyName::from_str("user:/test/qwe/asd/zxc").unwrap()
        );

        assert!(key1 == key1);
        assert!(key2 == key2);
        assert!(key2 == key3);

        assert!(key1 < key2);
        assert!(key2 > key1);

        assert!(key1 <= key2);
        assert!(key2 >= key1);
    }

    #[test]
    fn test_key_name() {
        let mut key_name = KeyName::from_str("user:/test/qwe/asd").unwrap();
        assert_eq!(key_name.to_string(), "user:/test/qwe/asd");
        assert_eq!(key_name.base_name().unwrap(), "asd");
        assert_eq!(key_name.namespace(), KeyNamespace::User);

        key_name.append_name("qweqweqwe");
        assert_eq!(key_name.to_string(), "user:/test/qwe/asd/qweqweqwe");
        assert_eq!(key_name.base_name().unwrap(), "qweqweqwe");
        assert_eq!(key_name.namespace(), KeyNamespace::User);

        key_name.set_base_name("zxc");
        assert_eq!(key_name.to_string(), "user:/test/qwe/asd/zxc");
        assert_eq!(key_name.base_name().unwrap(), "zxc");

        key_name.set_namespace(KeyNamespace::Meta);
        assert_eq!(key_name.to_string(), "meta:/test/qwe/asd/zxc");
        assert_eq!(key_name.namespace(), KeyNamespace::Meta);

        key_name.set_namespace(KeyNamespace::User);
        assert_eq!(key_name.to_string(), "user:/test/qwe/asd/zxc");
        assert_eq!(key_name.namespace(), KeyNamespace::User);
    }

    #[test]
    fn test_key_name_below() {
        let key_name1 = KeyName::from_str("user:/test/qwe/asd").unwrap();
        let key_name2 = KeyName::from_str("user:/test/qwe/asd/rty").unwrap();
        let key_name3 = KeyName::from_str("user:/test/qwe/asd").unwrap();
        let key_name4 = KeyName::from_str("meta:/test/qwe/asd").unwrap();

        assert!(key_name2.is_below(&key_name1));
        assert!(key_name2.is_below_or_same(&key_name1));
        assert!(key_name2.is_directly_below(&key_name1));
        assert!(!key_name1.is_below(&key_name2));
        assert!(!key_name1.is_below_or_same(&key_name2));
        assert!(!key_name1.is_directly_below(&key_name2));

        assert!(!key_name1.is_below(&key_name3));
        assert!(key_name1.is_below_or_same(&key_name3));
        assert!(!key_name1.is_directly_below(&key_name3));
        assert!(!key_name3.is_below(&key_name1));
        assert!(key_name3.is_below_or_same(&key_name1));
        assert!(!key_name3.is_directly_below(&key_name1));

        assert!(!key_name3.is_below(&key_name4));
        assert!(!key_name3.is_below_or_same(&key_name4));
        assert!(!key_name3.is_directly_below(&key_name4));
        assert!(!key_name3.is_directly_below(&key_name4));

        assert!(!key_name4.is_below(&key_name3));
        assert!(!key_name4.is_below_or_same(&key_name3));
        assert!(!key_name4.is_directly_below(&key_name3));
        assert!(!key_name4.is_directly_below(&key_name3));
    }

    #[test]
    fn test_key_value() {
        let mut key = Key::new(
            KeyName::from_str("user:/test").unwrap()
        );

        assert_eq!(None, key.value());
        assert_eq!(None, key.value_to_string());

        key.set_value_str("asdf");
        assert_eq!(vec![97, 115, 100, 102], key.value().unwrap());
        assert_eq!("asdf".as_bytes(), key.value().unwrap());
        assert_eq!("asdf", key.value_to_string().unwrap());
    }

    #[test]
    fn test_key_builder() {
        let key = KeyBuilder::from_str("user:/test/qwe/asd")
            .unwrap()
            .value("asd".as_bytes())
            .build()
            .unwrap();

        assert_eq!(key.name().to_string(), "user:/test/qwe/asd");
        assert_eq!(key.value_to_string().unwrap(), "asd");

        let key_name = KeyName::from_str("user:/test/qwe/zxc").unwrap();
        let key = KeyBuilder::new(key_name)
            .value("qwe".as_bytes())
            .build()
            .unwrap();

        assert_eq!(key.name().to_string(), "user:/test/qwe/zxc");
        assert_eq!(key.value_to_string().unwrap(), "qwe");
    }

    #[test]
    fn test_keyset() {
        let key = Key::new(
            KeyName::from_str("user:/test/qwe/asd").unwrap()
        );

        let keyset_content = vec![key];

        let mut keyset = KeySet::from_iter(keyset_content);
        assert_eq!(1, keyset.len());

        let key_lookup = keyset.get(0).unwrap();
        assert_eq!("user:/test/qwe/asd", key_lookup.name.to_string());
        assert_eq!(1, keyset.len());

        let key_lookup = keyset.lookup("user:/test/qwe/asd").unwrap();
        assert_eq!("user:/test/qwe/asd", key_lookup.name.to_string());
        assert_eq!(1, keyset.len());

        let key_removed = keyset.take("user:/test/qwe/asd").unwrap();
        assert_eq!("user:/test/qwe/asd", key_removed.name.to_string());
        assert_eq!(0, keyset.len());

        let key = Key::new(
            KeyName::from_str("user:/test/qwe/asd").unwrap()
        );

        let keyset_content = vec![key];
        let mut keyset = KeySet::from_iter(keyset_content);
        assert_eq!(1, keyset.len());

        keyset.clear();
        assert_eq!(0, keyset.len());
    }

    #[test]
    fn test_keyset_reference_counter() {
        let mut keyset = KeySet::default();

        assert_eq!(0, keyset.reference_counter());

        assert_eq!(0, keyset.decrease_reference_counter());
        assert_eq!(0, keyset.reference_counter());

        assert_eq!(1, keyset.increase_reference_counter());
        assert_eq!(1, keyset.reference_counter());

        assert_eq!(2, keyset.increase_reference_counter());
        assert_eq!(2, keyset.reference_counter());

        assert_eq!(1, keyset.decrease_reference_counter());
        assert_eq!(1, keyset.reference_counter());

        assert_eq!(0, keyset.decrease_reference_counter());
        assert_eq!(0, keyset.reference_counter());

        assert_eq!(u16::MAX, keyset.set_reference_counter(u16::MAX));
        assert_eq!(u16::MAX, keyset.reference_counter());

        assert_eq!(u16::MAX, keyset.increase_reference_counter());
        assert_eq!(u16::MAX - 1, keyset.decrease_reference_counter());
        assert_eq!(u16::MAX - 1, keyset.reference_counter());
    }

    #[test]
    fn test_key_copy() {
        let mut key_name = KeyName::from_str("user:/qwe").unwrap();
        let mut key1 = KeyBuilder::new(key_name)
            .value("qwe".as_bytes())
            .build()
            .unwrap();

        key_name = KeyName::from_str("user:/asd").unwrap();
        let key2 = KeyBuilder::new(key_name)
            .value("asd".as_bytes())
            .build()
            .unwrap();

        key1.copy_from(&key2, KeyCopyFlags::NAME | KeyCopyFlags::NAME);
        assert_eq!(key1.name().to_string(), "user:/asd");
        assert_eq!(key1.name(), key2.name());
        assert_ne!(key1.value().unwrap(), key2.value().unwrap());

        key1.copy_from(&key2, KeyCopyFlags::VALUE | KeyCopyFlags::VALUE);
        assert_eq!(key1.value().unwrap(), "asd".as_bytes());
        assert_eq!(key1.value().unwrap(), key2.value().unwrap());

        // key1.copy_from(key2, KeyCopyFlags::META);
        // assert_eq!(*key1.meta(), *key2.meta());
    }

    #[test]
    fn test_key_meta() {
        let key_name = KeyName::from_str("user:/qwe").unwrap();
        let mut key1 = KeyBuilder::new(key_name)
            .value("qwe".as_bytes())
            .build()
            .unwrap();

        let meta_key_name = KeyName::from_str("meta:/qwe").unwrap();
        let meta_key = KeyBuilder::new(meta_key_name)
            .value("asd".as_bytes())
            .build()
            .unwrap();

        assert_eq!(key1.meta().len(), 0);

        key1
            .meta_mut()
            .append(meta_key);

        assert_eq!(key1.meta().len(), 1);
        assert!(key1.meta().lookup("meta:/qwe").is_some())
    }
}