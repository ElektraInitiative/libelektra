# Release Procedure Documentation

This document describes what to do for a release.

### Manual Testing

- fuzz checker
- memleaks
- ASAN

If no release critical problems are found during testing, continue.

## Tasks before Release

### Updates

- Update `doc/COMPILE.md` to reflect actually tested setups
- Run `scripts/dev/update-infos-status` with arguments for heuristics (--auto) and check
  transitions in plugin status: especially from/to experimental
- Write release notes with guid (but without giving a final name, hashsums, statistics, proper download links or replacing the `<<VERSION>>` placeholder)
  > NOTE: hashsums and statistics get automatically inserted into release notes through the release pipeline

### Increment Version Number

- `CMakeLists.txt`
- Increment version in output of `doc/tutorials/hello-elektra.md`
- Increment <Version> of libelektra in `examples/external/java/read-keys-example/pom.xml` and `src/bindings/jna/README.md`
- Change `VERSION` variable in build-server:
  - Go to https://build.libelektra.org
  - Select "Manage Jenkins" -> "Configure System"
  - Scroll down until "Global Properties" and change the variable VERSION

### Check

- https://build.libelektra.org/job/libelektra/job/master/ must pass
- Run [audit-dependencies](/scripts/dev/audit-dependencies) to make sure no known vulnerabilities exist in project dependencies
- If something was updated in `go-elektra` make sure to follow [go-publishing](https://go.dev/doc/modules/publishing) for publishing the new version

## When Source Code is considered ready

- Trigger release pipeline with parameters:
  - The revision parameters should be set to 1 (default).  
    On a package revision release (version does not change, only package revision) the revision parameters need to be incremented.
  - The populate_release_notes parameter should be set to true (default).  
    If set to false, the automatic insertion of hashsums, statistics and moving the release notes to its final name, is disabled.
- Download artifacts after release build is finished and pipeline waits for input:
  To download the artifacts either open the "Artifacts" tab on Blue Ocean or open this URL
  `https://build.libelektra.org/job/libelektra-release/<BUILD_NUMBER>/artifact/artifacts/`
  where `<BUILD_NUMBER>` is the number of the current pipeline job which can be found at https://build.libelektra.org/job/libelektra-release
  in the "Build-History" (bottom-left) beginning with "#"
- Inspect the logs from the artifacts
  For each distribution two artifact folder exist: "`<DISTRO_CODENAME>`" and "`<DISTRO_CODENAME>-installed`"
  "`<DISTRO_CODENAME>-installed`" contains the test and strace logs of Elektra installed through the built packages
  For `<DISTRO_CODENAME>` artifact:
  - [ ] Error logs (`*.error`) must be empty
  - [ ] Check in the (`test-*`) folders if the passed tests really were successful, i.e.:
    - [ ] do not contain error output
    - [ ] were not skipped
  - [ ] Check if version is correct in the version file
  - [ ] Check if the package build log has warnings (`<DISTRO_CODENAME>/elektra_*.build` and `<DISTRO_CODENAME>/elektra_*.error`)
  - [ ] Check if all plugins/bindings/tools that are part of a package were included (`<DISTRO_CODENAME>/elektra_*.build`)
        For `<DISTRO_CODENAME>-installed` artifact:
  - [ ] Check in the (`test-*`) folders if the passed tests really were successful, i.e.:
    - [ ] do not contain error output
    - [ ] were not skipped
- Inspect the changes to the libelektra Git repository in git/master.log
  - [ ] Check diff of commit "release: regenerate plugins overview picture" and if
        `doc/images/plugins.pdf` was generated correctly
  - [ ] Check diff of commit "release: update plugin info status": Plugin tags should be sorted (optional commit)
  - [ ] Check diff of commit "release: update debian/changelog": Changelog should have a new entry for this release
  - [ ] Check diff of commit "release: update fedora/changelog": Changelog should have a new entry for this release

## Publish

- Publish packages and artifacts from the pipeline:
  - Go to the classic Jenkins UI at https://build.libelektra.org/job/libelektra-release
    (BlueOcean sometimes doesn't show the Input step on long pipelines)
  - Select "Paused for Input" on the side menu
  - On the prompt "Publish Release?" select "Yes" to publish the artifacts
- Verify that debs.libelektra.org and rpms.libelektra.org contain the released packages.
- Complete release notes:
  - Verify that download links are valid
  - Check if statistics and hashsums are present
- Run linkchecker to verify that all download links of the current release are working:
  `mkdir -p build && cd build && cmake .. && make html && ../scripts/link-checker external-links.txt`
- Verify if website is correct

### Preperation for next release

- Increment `CMPVERSION` in `scripts/build/run_icheck`
- Cleanup `tests/icheck.suppression` (and add info to release notes)

### Announce

send out release mails&post on social media (see GitHub issue #676)

### Special Requirements

- Every first release of the year
  - Update copyright in `LICENSE.md`
