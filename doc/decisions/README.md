# Introduction

We use decision template is based on
[''using patterns to capture architectural decisions''](https://dl.acm.org/doi/10.1109/MS.2007.124)
and [arc42 decisions](http://docs.arc42.org/section-9/)

Decisions need to:

- be implementable within the next major release
- be according to [Elektra's goals](/doc/GOALS.md)

To add a new decision copy `template.md` and add a link in the appropriate
section here.

## Implemented

- [Unit Testing](unit_testing.md)
- [Script Testing](script_testing.md)
- [Library Split](library_split.md)
- [Bootstrap](bootstrap.md)
- [Empty Files](empty_files.md)
- [CMake Plugins](cmake_plugins.md)
- [Logging](logging.md)
- [Elektra Web Structure](elektra_web.md)
- [Elektra Web Recursive Structure](elektra_web_recursive.md)
- [Cryptographic Key Handling](cryptograhic_key_handling.md)
- [Relative Storages](relative.md)
- [Deferred Plugin Calls](deferred_plugin_calls.md)
- [High-level API](high_level_api.md)
- [High-level API Help Message](highlevel_help_message.md)
- [Global KeySet](global_keyset.md)
- [Rest API Documentation](rest_api_documentation.md)
- [Ingroup Removal](ingroup_removal.md)
- [Commit Function](commit_function.md)
- [Error Message Format](error_message_format.md)
- [Default Values](default_values.md)
- [Error codes](error_codes.md)
- [Error code implementation](error_code_implementation.md)
- [Semantics for Name](semantics_name.md)
- [Base Name](base_name.md)
- [Lookup every key](lookup_every_key.md)
- [Holes and Non-leaf values](holes.md)
- [Multiple File Backends](multiple_file_backends.md)
- [Ensure](ensure.md) (@kodebach)

## Mostly Implemented

- [Array for Warnings](warning_array.md)
- [Array](array.md)
- [Boolean](boolean.md)

## Implementation Started

- [Iterators](iterators.md)
- [Reference Counting](reference_counting.md)

## Decided

- [Capabilities](capabilities.md) (@markus2330)
- [Remove elektraMalloc et al.](remove_elektra_malloc.md)

## In Discussion

- [keyString() return value](key_string_return_value.md)
- [Global Plugins](global_plugins.md) (@mpranj)
- [Error Semantics](error_semantics.md) (API)
- [Error Handling](error_handling.md)
- [Functions copying into buffer](functions_with_buffers.md)
- [Escaped Name](escaped_name.md) merge with:
- [Store the escaped and/or unescaped key name](store_name.md)
- [Spec Expressiveness](spec_expressiveness.md)
- [Metadata in Spec Namespace](spec_metadata.md)

## Delayed

- [Plugin Variants](plugin_variants.md)
- [Global Validation](global_validation.md)

## Rejected

- [CMake spec](cmake_spec.md)
- [Null Pointer Checks](null_pointer_checks.md)
- [Elektra Web Publish Subscribe](elektra_web_pubsub.md)
- [Internal Cache](internal_cache.md)
- [Vendor Spec](vendor_spec.md)
