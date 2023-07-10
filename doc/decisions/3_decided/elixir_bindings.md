# Elixir Bindings

## Problem

When creating Elixir bindings one needs a way to use some kind of foreign function interface.
In Erlang (which is the basis for Elixir) such a foreign function interface exists in the form of NIFs (native implemented functions).
The question is how this NIF interface should be implemented.

## Constraints

1. Elektra's C API is given.
2. The implementation should be completed in a given time frame of around 20 to 40 person-hours.

## Assumptions

1. The compatibility of the Elixir bindings is not very important.

## Solutions

### By hand

Implement the NIFs by hand.
That is, every function of the Elektra C API which one wishes to expose to the Elixir binding has to be explicitly implemented as a NIF.
The big disadvantage is, of course, that this process is error-prone.
Furthermore, any change in the C API has to be manually carried over to the NIF.

### Use Nifty code generation tool

Use the existing code generation tool [Nifty](http://parapluu.github.io/nifty/).

Reasons for not choosing this approach are:

- Installation process required several patches of Nifty.
- Resource management is not that supplied by the Erlang NIF library but a custom one.

### Write own code generation tool

Write a custom generation tool.

In principle one should be able to take a C header file and convert this to a NIF in an automatic fashion.

Solving this problem in general clearly would be a daunting task.
However, when restricting oneself to the use case for creating an interface for, say, Elektra's KDB this might actually have been feasible.

The main difference to Nifty would be that one could adopt the memory management tools provided by the Erlang NIF library directly.
Furthermore, it would be possible to write an improved interface for the types, e.g. binary and string.

The main reson for not choosing this approach were time constraints.

## Decision

The chosen approach was to use "Alternative A", i.e., to implement the NIFs by hand.

## Rationale

The main reason for this choice were time constraints.

Since I have assumed that the compatiblity of the Elixir bindings is not very important, it is acceptable if future changes break these bindings.

## Implications

- Every incompatible change in the C API needs manual changes in the Elixir binding.

## Related Decisions

- [GitHub PR: Elixir Bindings](https://github.com/ElektraInitiative/libelektra/pull/4623)

## Notes

- If there were a guarantee that the C API does not change, then the chosen approach would be unproblematic.
