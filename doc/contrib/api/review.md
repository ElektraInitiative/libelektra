On potential changes (including extensions) of the API/ABI please check following list:

- [ ] [use case](/doc/usecases) for API exists
- [ ] API design [review](reviews/) template is created by author of API change
- [ ] **Before** starting to implement the change: [decisions](/doc/decisions) for API is in step `decided`
- [ ] full Doxygen documentation (all parameters, full design-by-contract, brief, long, examples, etc.)
- [ ] still ABI/API forward-compatible
      (It is okay to break backward-compatible to add new symbols)
- [ ] symbol versioning done correctly (./doc/dev/symbol-versioning.md)
- [ ] all Bindings written & tested
      (only if bindings exist for the module)
- [ ] full Testcoverage
- [ ] change is mentioned in the release notes, section `Compatibility`
- [ ] API design is [reviewed](reviews/) by someone else, too
- [ ] [decisions](/doc/decisions) for API is in step `implemented`
