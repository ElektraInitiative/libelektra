- infos = Information about the date plugin is in keys below
- infos/author = Name <name@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = presetstorage postgetstorage
- infos/status = recommended productive maintained reviewed conformant compatible coverage specific unittest tested libc final
- infos/metadata =
- infos/description = one-line description of date

## Validation options ##

### Format string ###

`check/date/format`
	a format string compatible to `STRPTIME(3)`,

the date plugin tries to parse the date/time value base on the format string

### ISO8601 ###

`check/date/iso8601`
	a combination of the basic formats specified in iso8601 (see [wikipedia.org/ISO8601](https://en.wikipedia.org/wiki/ISO_8601) )
	
can be either `<data>`, `<time>[<tz>]` or `<date>T<time>[<tz>]`

where `<date>` can be

	`<calendardate>`: Calendar representation of the date

	`<weekdate>`: Week date representation

	`<ordinaldate>`: Ordinal date 

	`<date>`: any of the 3 date formats


`<time>` the 24-hour representation of the time

and `<tz>` the timezone designator

### RFC2822 ###

`check/date/rfc2822`
	a set of possible format strings derived from rfc2822 3.3
	

## Dependencies ##

None.

## Examples ##

TBD

## Limitations ##

None.
