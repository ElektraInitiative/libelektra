- infos = Information about the iterate plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides =
- infos/needs =
- infos/placements = presetstorage postgetstorage
- infos/status = unittest nodep libc experimental unfinished nodoc concept
- infos/description = conditionally calls exported functions

## Usage

Suppose you have a plugin bar that exports the function `foo(Key *k)`.
Then you can mount:

    kdb mount file.dump /example/iterate dump iterate when=bar foo Key

Which will execute `foo(k)` for every key that has the metadata `when`.
