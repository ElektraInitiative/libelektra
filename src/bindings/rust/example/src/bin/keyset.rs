extern crate elektra;

use elektra::{KeyBuilder, KeySet, LookupOption, ReadableKey, StringKey, WriteableKey};

fn main() -> Result<(), Box<dyn std::error::Error>> {
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
        println!("Value of {} is {}", key.name(), key.value());
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

    println!("Value of {} changed to {}", removed_key.name(), removed_key.value());
    // Check that the meta information is set
    println!(r#"Value of "setby" metakey is "{}""#, removed_key.meta("setby")?.value());

    // The keyset is now empty
    println!("KeySet size is now {}", keyset.size());

    Ok(())
}
