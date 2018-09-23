- infos = Information about the injection plugin is below
- infos/author = Michael Zronek <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = inject
- infos/recommends =
- infos/placements = presetstorage
- infos/status = maintained nodep
- infos/metadata = inject/rand/seed inject/structure/section/remove inject/structure/section/duplicate inject/structure/section/reallocate inject/semantic inject/resource inject/typo/transposition inject/typo/insertion inject/typo/case/toggle inject/typo/deletion inject/typo/change/char inject/typo/space inject/domain inject/limit/min inject/limit/max
- infos/description = Injections for configurations

# Injection Plugin

## Introduction

This plugin is used for purposefully alter single or multiple configuration settings in beneath a keyset.
The plugin searches for metadata 'markers' on every key and depending on the injection, requires additional metadata.
How the specific injection is carried out can either be random or, if desired, pseudorandom to guarantee reproducability.

## Usage

There up to 6 error types currently with injection types for each.

1. Structural errors (e.g. missing sections, parameters in wrong sections, omission necessary parameters in section)
2. Semantic errors (e.g. wrong version, documentation, confusing similar applications)
3. Resource errors (e.g. filename not available)
4. Typos (e.g. case, insertion, substitution, transposition)
5. Domain-specific misunderstandings (e.g. not an URL in /browser/start_page)
6. Limits of specification (e.g. border cases in ranges)

**All** injection types *can* have an additional metadata which is called `inject/rand/seed` which should be an integer value between 1-1000.
This value can be used to gaurantee reproducability of injected errors. As an example, in a character insertion injection (i.e. from `true` to `trGue`)
you can be assured that if you run the plugin on the same keyset twice, the injected character will be the same.
If you omit the `inject/rand/seed` metadata, the plugin will generate its own random number.

Please note that you can only have **one injection** per setting. If you have multiple injection types on a key, only a single one is taken (which one depends on the order of the implementation in which it checks for the metakeys)

### Structural Errors

Structural errors combine all injection types in which the structure and location of an configuration are not correct. So for example an expected setting under a certain section is actually appearing in another section in which it does not belong to.

* Section Remove
    * `inject/structure/section/remove`
    * Removes a section from a configuration (does not require randomness)
        ```
        a:
            b1:
                c1:
            b2:
                c2:
        ```
        &downarrow;&downarrow;&downarrow;
        ```
        [a/b1]
        inject/structure/section/remove =
        ```
        &downarrow;&downarrow;&downarrow;
        ```
        a:
            b2:
                c2:
        ```

* Section Reallocator
    * `inject/structure/section/reallocate`
    * Removes a section and puts it in another place
        ```
        a:
            b1:
                c1:
            b2:
                c2:
        ```
        &downarrow;&downarrow;&downarrow;
        ```
        [a/b1]
        inject/structure/section/reallocate =
        ```
        &downarrow;&downarrow;&downarrow; (random)
        ```
        a:
            b2:
                b1:
                    c1:
                c2:
        ```

* Section Duplicator
    * `inject/structure/section/duplicate`
    * Randomly duplicates a section somewhere else
        ```
        a:
            b1:
                c1:
            b2:
                c2:
        ```
        &downarrow;&downarrow;&downarrow;
        ```
        [a/b1]
        inject/structure/section/duplicate =
        ```
        &downarrow;&downarrow;&downarrow; (random)
        ```
         a:
            b1:
                c1:
            b2:
                b1:
                    c1:
                c2:
        ```

### Semantic Errors

Semantic errors come from misunderstandings of the actual setting. In this setting you provide an array notation of alternative values for a key in the metadata.

* Semantic injection
    * `inject/semantic/#[number]`
    * Takes one of the alternatives from the array and substitutes it in the key
    
        ```
        logger = log4j
        ```
        &downarrow;&downarrow;&downarrow;
        ```
        [logger]
        inject/semantic/#0 = pyLog
        inject/semantic/#1 = log4net
        inject/semantic/#2 = Log4js
        ```
        &downarrow;&downarrow;&downarrow; (random)
        ```
        logger = log4net
        ```

### Resource Errors

Resource errors are errors where actual required resources are not available (e.g. directory does not exist) or have the wrong permission (e.g. write permission to logfile is not given)

This injection type behaves equivalent to the semantic errors as it requires human understanding and possibly a presetup (generation of a file with wrong permissions).

* Resource injection
    * `inject/resource/#[number]`
    * Takes one of the alternatives from the array and substitutes it in the key
    
        ```
        port = 5555
        logfile = /var/log/application.log
        ```
        &downarrow;&downarrow;&downarrow;
        ```
        [port] #assuming they are in use already
        inject/resource/#0 = 22
        inject/resource/#1 = 0
        inject/resource/#2 = 80

        [logfile]
        inject/resource/#0 = /does/not/exist/application.log
        inject/resource/#1 = /root/application.log
        inject/resource/#2 = /full/partition/used/application.log

        ```
        &downarrow;&downarrow;&downarrow; (random)
        ```
        port = 22
        logfile = /does/not/exist/application.log
        ```

### Typo Errors

These errors are likely to be the most occuring ones in configurations. They result from quick typing and can have errors like omitted characters (true vs. tue)/ transposed characters (true vs. treu)/ different characters (true vs trur)/ etc.

* Transposition injection
    * `inject/typo/transposition`
    * Takes the value and shuffles two characters beside each other
    
        ```
        writeToSyslog = true
        ```
        &downarrow;&downarrow;&downarrow;
        ```
        [writeToSyslog]
        inject/typo/transposition =
        ```
        &downarrow;&downarrow;&downarrow; (random)
        ```
        logfile = rtue
        ```
* Insertion injection
    * `inject/typo/insertion`
    * Takes the value inserts a random character into it
    
        ```
        writeToSyslog = true
        ```
        &downarrow;&downarrow;&downarrow;
        ```
        [writeToSyslog]
        inject/typo/insertion =
        ```
        &downarrow;&downarrow;&downarrow; (random)
        ```
        logfile = trpue
        ```
* Casetoggle injection
    * `inject/typo/case/toggle`
    * Takes the value and toggles a character to upper or lowercase (depending on the character)
    
        ```
        writeToSyslog = true
        ```
        &downarrow;&downarrow;&downarrow;
        ```
        [writeToSyslog]
        inject/typo/case/toggle =
        ```
        &downarrow;&downarrow;&downarrow; (random)
        ```
        logfile = tRue
        ```
* Deletion injection
    * `inject/typo/deletion`
    * Takes the value removes a random character
    
        ```
        writeToSyslog = true
        ```
        &downarrow;&downarrow;&downarrow;
        ```
        [writeToSyslog]
        inject/typo/deletion =
        ```
        &downarrow;&downarrow;&downarrow; (random)
        ```
        logfile = tre
        ```
* Changecharacter injection
    * `inject/typo/change/char`
    * Takes the value subsitutes a random character
    
        ```
        writeToSyslog = true
        ```
        &downarrow;&downarrow;&downarrow;
        ```
        [writeToSyslog]
        inject/typo/change/char =
        ```
        &downarrow;&downarrow;&downarrow; (random)
        ```
        logfile = trye
        ```
* Spacecharacter injection
    * `inject/typo/space`
    * Takes the value adds a random number (between 1-5) of leading and trailing space characters
    
        ```
        writeToSyslog = "true"
        ```
        &downarrow;&downarrow;&downarrow;
        ```
        [writeToSyslog]
        inject/typo/space =
        ```
        &downarrow;&downarrow;&downarrow; (random)
        ```
        logfile = " true    "
        ```
### Domain Errors

Domain errors come from misunderstandings about the actual domain of the concrete setting. One good example would be that the domain are IP representations and a user types in a DNS entry like localhost.

These errors must be configured the same way as semantic errors as they are difficult to automatically generate.

* Domain injection
    * `inject/domain/#[number]`
    * Takes one of the alternatives from the array and substitutes it in the key
    
        ```
        server: 127.0.0.1
        port: 80
        startURL: mywebsite.com/index
        ```
        &downarrow;&downarrow;&downarrow;
        ```
        [server]
        inject/domain/#0 = localhost

        [port]
        inject/domain/#0 = eighty
        inject/domain/#1 = something from /etc/services

        [startURL]
        inject/domain/#0 = ssh://....
        inject/domain/#1 = "Welcome to my website"
        ```
        &downarrow;&downarrow;&downarrow; (random)
        ```
        server: localhost
        port: eighty
        startURL: ssh://mywebsite.com/index
        ```

### Limit Errors
These are not necessarily errors but many mistakes occur when having a setting to be configured to a limit of its specification (i.e. setting a configuration value to 1000 when 1-1000 is allowed). 

These errors are set with `inject/limit/min` and `inject/limit/max` and one of both is picked randomly (or pseudorandomly).

* Limit injection
    * `inject/limit/min` and `inject/limit/max`
    * Takes one of the alternatives from the array and substitutes it in the key
    
        ```
        contrast = 350
        ```
        &downarrow;&downarrow;&downarrow;
        ```
        [contrast]
        inject/limit/min = 1
        inject/limit/max = 500
        ```
        &downarrow;&downarrow;&downarrow; (random)
        ```
        contrast = 500
        ```
        
## Logging of changes

To see which changes were applied to the whole KeySet, additional metadata will be written to the rootkey.
Assume you mounted a configuration to `user/my/config` and attached some metakeys to settings.
All injections which were done are written in array notation to `inject/log/#[number]` and are continuous.
So for example:
```sh
kdb lsmeta user/my/config
#> inject/log/#0
#> inject/log/#1
...

kdb getmeta user/my/config inject/log/#0 
#> "Removed section user/my/config/some/section"

kdb getmeta user/my/config inject/log/#1 
#> "Changed value "true" to "truGe" on user/my/config/boolean/value"
```

