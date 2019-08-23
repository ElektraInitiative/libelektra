# Code-generator

This guide focuses on writing new templates for the code-generator. For using the code generator take a look at the man-page
[`kdb-gen(1)`](/doc/help/kdb-gen.md). For specifically using the code-generator for the high-level API you may want to follow
[this guide](/doc/tutorials/highlevel.md).

## Basics

The underlying framework of `kdb gen` is quite flexible. It is based on the [mustache templating system](https://mustache.github.io/).
Concretely we use [this C++ library](https://github.com/kainjow/Mustache) as a basis for our framework.

The file [`src/tools/kdb/gen.cpp`](/src/tools/kdb/gen.cpp) implements the command-line interface (CLI) and is of little interest to this
guide. Instead we will focus on the framework that is invoked via the CLI. The bulk of the framework is implemented in the classes
[`GenTemplate` and `GenTemplateList`](/src/tools/kdb/gen/template.hpp) (and of course kainjow's mustache library).

First we need to define a few terms:

- _template name_: This is the main identifier of a template. It is used in the command line to choose the template.
- _template base name_: Unlike the template name, this identifier is purely internal. It is the base name of all mustache files belonging
  to the template. In most cases this will be the same as the template name.
- _input keyset_: This keyset contains the data with which the template will be instantiated by the code-generator.
- _parent key_: The parent key is given in the command line and defines the input keyset (the keys below the parent key).
- _output name_: The base name for the output files as given in the command line. Suffixes may be appended, if there are multiple output
  files.
- _parts_: Each template may consist of multiple parts. Each part corresponds to exactly one mustache file and therefore exactly one output
  file.
- _part suffix_: Parts are identified by there part suffix. This suffix is appended to the template base name to obtain the name of the
  mustache file corresponding to the part. Similarly it is appended to the output name to obtain the output file for this part. The empty
  string `""` is a valid part suffix, although it is only recommend for templates with a single output file.

## Creating a new template

In this guide we will create a basic template, that generates a single file containing. The file will simple list all keys in our input
keyset. An example output would be (the same as `kdb ls`):

```
user/sw/myapp/#0/current
user/sw/myapp/#0/current/dir0
user/sw/myapp/#0/current/dir0/subdir0/key0
user/sw/myapp/#0/current/dir0/subdir0/key1
user/sw/myapp/#0/current/dir1
```

### Creating the mustache template

The first thing we need to create is a mustache template. You will need (at least) one template file for each output file. If you make use
of partials (see [below](#using-partials)), you of course have to write multiple files.

All template files must be located in the [`src/tools/kdb/gen/templates`](/src/tools/kdb/gen/templates) folder and must be named according
to the scheme `<template base name>.<part suffix>.mustache`.

For our simple example we create the file `src/tools/kdb/gen/templates/example.txt.mustache` with the following content:

```
{{# keys }}
{{{ name }}}
{{/ keys }}
```

Note: we will not go into detail on how mustache templates work, for more information see e.g.
[here](https://mustache.github.io/mustache.5.html). All of features supported by the kainjow library should be supported by our framework
as well.

### Creating the supporting class

Since we need to some way of supplying data to our template, we have to create a subclass of `GenTemplate`.

First we create a new directory for our template class in `src/tools/kdb/gen`, for our template it should be `src/tools/kdb/gen/example`. In
this directory we then create `example.cpp` and `example.hpp`. If your template becomes sufficiently complex, it may make sense to split it
into multiple files, for this reason we recommend creating a new directory for each template.

In `example.cpp` and `example.hpp` we create our subclass of `GenTemplate`. Therefore `example.hpp` should look like this:

```cpp
#ifndef ELEKTRA_EXAMPLE_HPP // choose a unique header guard
#define ELEKTRA_EXAMPLE_HPP

#include "../template.hpp"

class ExampleGenTemplate : public GenTemplate
{
public:
    ExampleGenTemplate () : GenTemplate ("example", { ".txt" }, {}, {})
    {
    }

protected:
    kainjow::mustache::data getTemplateData (const std::string & outputName, const std::string & part, const kdb::KeySet & ks,
                                             const std::string & parentKey) const override;
};

#endif // ELEKTRA_EXAMPLE_HPP
```

Apart from the line `ExampleGenTemplate () : GenTemplate ("example", { ".txt" }, {}, {})` everything should be more or less the same for
all templates. Let's dissect this line:

- `ExampleGenTemplate ()` we declare a zero argument constructor. All templates _must_ have only a single zero argument constructor,
  otherwise the framework cannot instantiate them.
- `: GenTemplate (` the constructor must invoke the base-class constructor.
- `"example",` we decided to use `example` as the base name for our template files. This base name will be replaced by the `outputName`
  chosen by the user, when invoking the code-generator.
- `{ ".txt" },` this is the list of part suffixes for our template. We only have a single part (output file) with the suffix `.txt`.
- `{},` the first empty list would contain all the partials our template uses. We don't use any.
- `{},` the second empty list, is actually a map. It would contain all parameters and whether they are required or not.
  We don't have parameters.

As you can see, the header is quite simple. The more important part is actually the source file. It contains the implementation of
`getTemplateData`, the function that delivers all the template data to the framework.

For our example we will use this implementation:

```cpp
kainjow::mustache::data ExampleGenTemplate::getTemplateData (const std::string & outputName, const std::string & part,
                                                             const kdb::KeySet & ks, const std::string & parentKey) const
{
    using namespace kdb;
    using namespace kainjow::mustache;

    list keyList;

    for (auto it = ks.begin (); it != ks.end (); ++it)
    {
        Key key = *it;

        auto keyObject = object({ { "name", key.getName() } });

        keyList.emplace_back(keyObject);
    }

    auto data = object({ { "keys", keyList } });

    return data;
}
```

The framework will invoke `getTemplateData` for each part of our template with the `outputName` and `parentKey` as given on the command line,
as well as the current part suffix (`part`) and the input keyset `ks`. All keys of the input keyset are guaranteed to be below `parentKey`.

How exactly the above code works is left as an exercise to the reader.

## Adding the class to `GenTemplateList`

To make the framework aware of our class (and by extension our template), we have to then add it to `GenTemplateList`. This is simply done
by adding a line to the implementation of `GenTemplateList::GenTemplateList ()` in [`template.cpp`](/src/tools/kdb/gen/template.cpp).

For our example that is:

```cpp
addTemplate<ExampleGenTemplate> ("example");
```

We need to specify `"example"` again, because this string defines how our template shall be called, i.e. what we need to specify in the
terminal to invoke it.

## Using the new template

Now you should be able to use the new template by running (after compiling/installing Elektra again):

```
kdb gen example user userkeys
```

This should produce the file `userkeys.txt`, whose content should match the output of `kdb ls user`.

## Advanced concepts

Lastly, we will discuss a few more advanced concepts.

### Switching delimiters

Switching the delimiters is fully supported and already in use in the high-level API template to allow for easier formatting.

### Parameters

Sometimes you want to incorporate user input beyond the contents of the input keyset into the code-generation. For example, if you generate
a web page, you may need to now where it is going to be hosted, so that you can generate correct links. Another example would be letting
the user choose the name of a generated function, when generating C code.

The first case would be required parameter. Without the domain name, we cannot generate the links. The second example meanwhile may be an
optional parameter. We could just use a sensible default name for the function, since it doesn't matter much which function the user will
call in their own code.

Our framework supports both required and optional parameters. To define the parameters of your template use the `parameters` argument of the
`GenTemplate` constructor. This parameter is a map from strings to bools. The keys are the parameter names and the values define, whether
the parameter is required (`true` means required).

Our two examples could use `{ { "domain_name", true } }` and `{ { "function_name", false } }` respectively.

To access the parameter value call one of the `getParameter` overloads. One takes a default value, the other takes a map of values and
verifies that one of the given values has been chosen. For more information see the relevant code documentation. There is also
`getBoolParameter` which a specialised version for boolean parameters. It accepts only `0` and `1` as values.

### Using partials

The use of partials is bit more involved than in other mustache frameworks. All the partial files for template `X` must be placed in the
folder `src/tools/kdb/gen/templates/X` and must use the file extension `.mustache`. Apart from the the filename can be chosen arbitrarily.

To use the partial named `Y` (i.e. the file `src/tools/kdb/gen/templates/X/Y.mustache`) you must use this mustache command:

```
{{> partial.enum.c }}
```

The prefix `partial.` is required by the framework, if you omit it, there will be an error.

### Custom escape functions

By default mustache escapes values for use in HTML (unless `{{{ name }}}` or `{{& name }}` is used). Since most of our templates are not in
fact HTML, the escape function can be customised. You simply have to override `GenTemplate::escapeFunction`. For an example see
`ElektraGenTemplate::escapeFunction`, it is designed for C code instead of HTML.
