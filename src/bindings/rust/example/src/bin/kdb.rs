extern crate elektra;

use elektra::{KeyBuilder, KeySet, LookupOption, StringKey, WriteableKey, KDB};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Open a KDB session
    let mut kdb = KDB::open()?;

    // Create a keyset that will hold the keys we get from the get call
    let mut ks = KeySet::with_capacity(10);

    // Get the current state of the key database
    let mut parent_key = StringKey::new("user:/test")?;
    kdb.get(&mut ks, &mut parent_key)?;

    // We can append a new key we want to store in the key database.
    let new_key: StringKey = KeyBuilder::new("user:/test/mycolor")?.value("#fff").build();
    ks.append_key(new_key);

    // Lookup a key in the keyset and modify it
    // As an example, we're modifying the same key we just added
    if let Some(mut key) = ks.lookup_by_name("user:/test/mycolor", LookupOption::KDB_O_NONE) {
        key.set_value("#ff00ff00");
    } else {
        panic!("Did not find the key!");
    }

    // Store the modified keyset
    let set_res = kdb.set(&mut ks, &mut parent_key);

    // If we don't care about specific errors and just want to print
    // a nice error message, we could do
    // set_res.unwrap_or_else(|err| panic!("{}", err));

    // Instead, you may want to match on specific error types
    match set_res {
        Ok(success) => {
            if success {
                println!("Successfully set new key in KDB.");
            } else {
                println!("No changes were made to the KDB.");
            }
            Ok(())
        }
        Err(kdb_error) => {
            if kdb_error.is_semantic() {
                // Handle the semantic validation error
                Ok(())
            } else {
                // Otherwise propagate the error up
                Err(Box::new(kdb_error))
            }
        }
    }
}
