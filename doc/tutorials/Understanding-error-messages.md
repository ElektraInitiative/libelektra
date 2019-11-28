# Understanding Error Messages

If you want to use Elektra for any of your projects you will sooner or later run into errors. These can happen
quite easily such trying to mount with insufficient permissions, missing plugin configurations or when you want to write
specifications for your configurations and test it out.

Here is an example how you can trigger an error for an invalid IP entry in the `hosts` file:

```sh
sudo kdb mount --with-recommends /etc/hosts system/hosts hosts
sudo kdb set system/hosts/ipv4/my 88.198.134.1777
# RET: 5
# Sorry, module network issued the error C03200:
# Validation Semantic: name: system/hosts/ipv4/my value: 88.198.134.1777 message: Name or service not known

#cleanup
sudo kdb umount system/hosts
```

You can see that every message comes with an error code which is `C03200` in the upper example which is a semantic validation error.
Since v0.9 Elektra has hierarchically structured error codes with are leaned on to [SQL States](https://en.wikipedia.org/wiki/SQLSTATE).
Some categories of Elektra are `parents` of more specific error categories and summarize certain behavior patterns. E.g., is the 
semantic validation error a concrete subcategory of `Validation errors` (`C03XXX`) that indicate that a certain requirement is not fulfilled
by the provided value.
You can see the whole hierarchy as well as the mindset behind its categorization in our [categorization guideline](../dev/error-categorization.md).
Every category comes with its own solution approach which is intended to be as generic as possible. In the next sections we will give you
more understanding of each category.

- Permanent errors C01000
  - Resource C01100 (conrete)
    - Out of Memory C01110 (concrete)
  - Installation C01200 (concrete)
  - Logical C01300 
    - Internal C01310 (conrete)
    - Interface C01320 (conrete)
    - Plugin Misbehavior C01330 (conrete)
- Conflicting State C02000 (conrete)
- Validation C03000
  - Syntactic C03100 (conrete)
  - Semantic C03200 (conrete)
  
## Permanent Errors C01XXX (abstract)

Permanent errors are these kind of errors which cannot be fixed by retrying, setting a different value, etc. It either lies outside
of Elektra and has to be fixed there (e.g., giving correct permissions to a file or missing installation files) or within Elektra
but is a bug or invalid programming arguments.

 