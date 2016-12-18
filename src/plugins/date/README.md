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

The following representation standards of dates and times are currently supported and can be use by setting `check/date` to:

* `POSIX`
 
   see `STRPTIME(3)` for more information. a valid format has to be specified in `check/date/format` 

* `ISO8601`
  
   see [ISO8601](https://en.wikipedia.org/wiki/ISO_8601). possible format strings specified in `check/date/format`:
 
 * Date:
 
  * `<calendardate>`
   
   `YYYY-MM-DD`, `YYYYMMDD`, `YYYY-MM`, `--MM-DD`, `--MMDD`
   
  * `<weekdate>`
   
   `YYYY-Www`, `YYYYWww`, `YYYY-Www-D`, `YYYYWwwD`
   
   * `<ordinaldate>`
   
   `YYYY-DDD`, `YYYYDDD`
   
   * `<date>`
   
   `<calendardate>`, `<weekdate>`, `<ordinaldate>` 
 
 * Time:
  
  * `<time>`
  
   `hh:mm:ss`, `hhmmss`, `hh:mm`, `hhmm`, `hh`
  
  * `<tz>`
  
   `<time>Z`, `<time>±hh:mm`, `<time>±hhmm`, `<time>±hh`
 
 * Combined:
 
   `<date>T<time>`

* `RFC2822`
 
  a set of possible format strings derived from rfc2822 3.3, no format string needed.

## Dependencies ##

None.

## Examples ##

TBD

## Limitations ##

None.
