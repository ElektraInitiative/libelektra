#[cfg(test)]
mod elektra_examples {
    use crate::{KeyBuilder, KeySet, LookupOption, ReadableKey, StringKey, WriteableKey};

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

    #[test]
    fn keyset_example() -> Result<(), Box<dyn std::error::Error>> {
        // Create a new KeySet with enough preallocated space for 5 Keys
        let mut keyset = KeySet::with_capacity(5);

        // Append some keys
        keyset.append_key(
            KeyBuilder::<StringKey>::new("user/sw/app/#1/host")?
                .value("localhost")
                .build(),
        )?;
        keyset.append_key(
            KeyBuilder::<StringKey>::new("user/sw/app/#1/port")?
                .value("8080")
                .build(),
        )?;

        // Iterate the keyset
        keyset.rewind();
        for mut key in keyset.iter_mut() {
            // Add a metakey to each key
            key.set_meta("setby", "owner")?;
        }

        // Lookup a key by name and set a new value
        if let Some(mut found_key) =
            keyset.lookup_by_name("user/sw/app/#1/port", LookupOption::KDB_O_NONE)
        {
            found_key.set_value("5001");
        } else {
            panic!("Did not find the key!");
        }

        // Delete a key by passing the POP option
        keyset.lookup_by_name("user/sw/app/#1/host", LookupOption::KDB_O_POP);

        // Remove the last (and now only) key in the set
        let removed_key = keyset.pop().unwrap();

        assert_eq!(removed_key.value(), "5001");
        // Check that the meta information is set
        assert_eq!(removed_key.meta("setby")?.value(), "owner");

        // The keyset is now empty
        assert_eq!(keyset.size(), 0);

        Ok(())
    }
}
