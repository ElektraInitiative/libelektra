extern crate elektra;

use elektra::{KeyBuilder, ReadableKey, StringKey, WriteableKey};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // To create a simple key with a name and value
    let mut key = StringKey::new("user:/test/language")?;
    key.set_value("rust");
    assert_eq!(key.name(), "user:/test/language");
    assert_eq!(key.value(), "rust");

    // To iterate over the name
    for name in key.name_iter() {
        println!("Name: {}", name);
    }

    // Duplicate a key
    let key_duplicate = key.duplicate(CopyOption::KEY_CP_ALL);

    // And compare them
    assert_eq!(key, key_duplicate);

    // To create a key with multiple meta values, use the KeyBuilder
    let mut key: StringKey = KeyBuilder::new("user:/test/fruits")?
        .meta("banana", "🍌")?
        .meta("pineapple", "🍍")?
        .meta("strawberry", "🍓")?
        .build();
    assert_eq!(key.meta("pineapple")?.value(), "🍍");

    // We can iterate over the metakeys
    for metakey in key.meta_iter() {
        println!("Key: {}, Value: {}", metakey.name(), metakey.value());
    }

    // Delete a metakey
    key.delete_meta("banana")?;

    // Check if key is in the user namespace
    assert!(key.is_user());

    Ok(())
}
