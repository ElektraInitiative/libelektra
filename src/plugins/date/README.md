- infos = Information about the date plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = presetstorage postgetstorage
- infos/status = recommended productive maintained reviewed conformant compatible coverage unittest tested libc final
- infos/features/storage = limited
- infos/metadata = check/date check/date/format
- infos/description = validates date and time strings

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Validation options

The following representation standards of dates and times are currently supported and can be use by setting `check/date` to:

- `POSIX`

  see `STRPTIME(3)` for more information. A valid format has to be specified in `check/date/format`

- `ISO8601`

  see [ISO8601](https://en.wikipedia.org/wiki/ISO_8601). Possible format strings specified in `check/date/format`, default: `datetime complete+truncated`:

  - Dates/Time:

    - Date:

      - `calendardate`
        calendar dates: day of month - month - year.

      - `weekdate`
        calendar week and day numbers, e.g. YYYY-Www-D

      - `ordinaldate`
        year + day of the year

      - `date`
        `calendardate`, `weekdate`, and `ordinaldate` combined

    - Time:

      - `timeofday`
        24-hour timekeeyping system

      - `utc`
        coordinates universal time. either by appending a time-zone designator or the time difference to `UTC` to `timeofday`

    - Combined:
      `datetime`
      combination of Dates and Time according to th ISO8601 specification.

  - Representation:
    if no representation is specified, `complete+reduced+truncated` is used as default.

    - `complete`
      complete representation, dates are separated by hyphens, times by colon, e.g. YYYY-MM-DD or hh:mm:ss

    - `reduced`
      reduced precision, e.g. YYYY-MM, or hh

    - `truncated`
      truncated representation, hyphens used to indicate omitted components, e.g. --MM-DD or --ss

    - `complete+reduced+truncated`
      allow all 3 representations

    - `complete+reduced`
      allow only `complete` + `reduced` representation

    - `complete+truncated`
      allow only `complete` + `truncated` representation

    - `reduced+truncated`
      allow only `reduced` + `truncated` representation.

  - Format:
    if no format is specified both `basic` and `extended` are treated as valid.

    - `basic`
      no separating character between individual components of a `date`, `time` or `datetime` expression.

    - `extended`
      separating characters between components. `date` components separated by hyphen, `time` components by colon.

* `RFC2822`

  a set of possible format strings derived from rfc2822 3.3, no format string needed.

## Dependencies

POSIX.1-2001

## Examples

TBD

## Limitations

Testing timezone designators currently only works with glibc.
Unit tests using timezone designators are locale dependent.
