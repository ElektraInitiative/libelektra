#[cfg(test)]
mod elektra_examples {
    use crate::{KeyBuilder, ReadableKey, StringKey, WriteableKey};

    #[test]
    fn key_example() -> Result<(), Box<dyn std::error::Error>> {
        // This test should stay in sync with the example in the Readme
        // To create a simple key with a name and value
        let mut key = StringKey::new("user/test/language")?;
        key.set_value("rust");
        assert_eq!(key.name(), "user/test/language");
        assert_eq!(key.value(), "rust");

        // To iterate over the name
        for name in key.name_iter() {
            println!("Name: {}", name);
        }

        // Duplicate a key
        let key_duplicate = key.duplicate();

        // And compare them
        assert_eq!(key, key_duplicate);

        // To create a key with multiple meta values, use the KeyBuilder
        let mut key: StringKey = KeyBuilder::new("user/test/fruits")?
            .meta("banana", "🍌")?
            .meta("pineapple", "🍍")?
            .meta("strawberry", "🍓")?
            .build();
        assert_eq!(key.meta("pineapple")?.value(), "🍍");

        // We can iterate over the metakeys
        key.rewind_meta();
        for metakey in key.meta_iter() {
            println!("Key: {}, Value: {}", metakey.name(), metakey.value());
        }

        // Delete a metakey
        key.delete_meta("banana")?;

        // Check if key is in the user namespace
        assert!(key.is_user());

        Ok(())
    }
}
