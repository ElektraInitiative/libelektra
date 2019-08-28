# Bindings for the High-level API

This document describes how and when to write a language binding for the high-level API.

Writing bindings for high-level API is different from writing bindings for other parts of Elektra. This is mainly
because the high-level API has different goals.

## Goals

The goals of any high-level API (or binding to the C high-level API) for Elektra should be:

- **Type Safety:** The API should use Elektra's type system as dictated by the `type` plugin. The various types shall
  be mapped to native types of the target language. All API calls interacting with keys should reflect the type of the
  key. Type mismatches should produce errors.
- **Easy to Use:** The API should be easy to use and abstract as much of the low-level API as possible. The main part of
  the API should not conists of more than an initialization method, typed `get` and `set` calls and if required by the
  language a method freeing the acquired resources.
  - **No Errors in Getters:** It is a stated goal of our high-level API to make `get` calls not able to fail. This means
    `get` calls _cannot_ return an error under normal conditions. This can be ensure via checks during initialization or
    via code-generation. Both are based on the specification. Without a specification it is not possible to prevent
    errors because of missing keys, since we have no way of knowing which keys should exist. <br/><br/>
    There is an exception to this rule. Some target languages have a standard error concept, which not only forces the
    user to handle arising errors, but also does so in a simple and concise way. An example of this is Rust. Its `Result`
    type forces the user to handle errors and the `?` operator allows to do this in a concise way. <br/>
    However, we still recommend to avoid errors as far as possible.
- **Idomatic Code:** If the targt language has a concept of "idomatic code", the API should fall into that category.

## When to write Bindings

Based on the goals above, you should decide, whether it is possible to write a binding for the C high-level API while
preserving the goals.

If you decide to write a binding, proceed with this tutorial.

If not, designing an API to meet our goals is up to you. While designing the API you should keep in mind that you can
always use code-generator (`kdb gen`) templates like the C API does.

## How to write Bindings

Since the C high-level API consits of a shared library API and a code-generated part, your binding will also have these
two parts.

### Bindings for the `highlevel` Library

Writing the binding for the `highlevel` library works the same way as writing a language binding for any part of
Elektra. The only additional challenge is that the binding should still meet the same goals and (as far as possible)
fulfill the same guarantees as the C API.

Some languages like to split bindings into two parts. One version that maps the C API one-to-one and another part
that builds on the first one. The second part then uses the languages additional features. Since our API isn't inteded
to be used directly, but through generated code, it might not be necessary to write the second part. It may be
sufficient to write (or generate) a one-to-one mirror of the C API and use that in the code-generator template.

The generated code should still be understandable. So if writing a more idiomatic API on top of the
direct mapping, significantly simplifies the generated code (and maybe also the template), you should write such an API.

### Creating the Code-Generator Template

How to create a template for `kdb gen` is detailed in [this tutorial](code-generator.md).

The created template should support the same input keysets (and parent keys) as the `highlevel` template. If and how
exactly you implement the advanced features (structs, unions, ...) is up to you.
Note: enums should always be supported (if your language has them) as they are one of the `type`
plugins types.

For example, in C++ the generated code could consist of nested structs with overloaded operators. In that case
the structs features doesn't really make sense, since everything is already structs. Of course you could reuse some of
the `gen/*` metadata to allow some cross-compatibility.
