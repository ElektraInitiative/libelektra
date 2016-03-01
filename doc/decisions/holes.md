# Holes in KeySets

holes in config makes sense in the KeySet as today, but is maybe not a
good idea for strongly hierarchically structured data.

+ allow specific overrides without override of structure below

See [Hierarchy Example](/src/bindings/cpp/examples/cpp_example_hierarchy.cpp)
for how to implement a hierarchy that gets rid of the problem that holes
might create in this context.
