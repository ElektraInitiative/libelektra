On potential changes (including extensions) of the API/ABI please check following list:

- [ ] [use case](/doc/usecases) for API exists
- [ ] API design [review](/doc/api_review/) template is created by author of API change
- [ ] **Before** starting to implement the change: [decision](/doc/decision) for API is in step `decided`
- [ ] **After** implementing the change: [decision](/doc/decision) for API is in step `implemented`
- [ ] API design is [reviewed](/doc/api_review/) by someone else, too
- [ ] full Doxygen docu (all parameters, full design-by-contract, brief, long, examples, etc.)
- [ ] full Testcoverage
- [ ] still ABI/API forward-compatible
      (It is okay to break backward-compatible to add new symbols)
- [ ] all Bindings written & tested
      (only if bindings exist for the module)
- [ ] change is mentioned in the release notes, section `Compatibility`
- [ ] symbol versioning done correctly (./doc/dev/symbol-versioning.md)
