# Decisions

## metadata hierarchy

- `define/type`
   
   If not present we assume the there aren't any type definitions. If present, it will be used as a cut point to extract all type definitions.

- `define/type/<TYPENAME>/check/<PLUGIN>`

   if the value is an array indice `#*` it's assumed that all keys below belong to the same check, otherwise assume that the value is the only check metadata for `PLUGIN`.

- `type`

   depending on its value:

   - `#*`

      assume SUBTYPE - each elements value has to be a defined type

   - otherwise

      assume there are no keys below. the value is a the defined type the key will be validated against


- `define/type/<TYPENAME>`

   if value == `sum`

      if `check/*` metakeys are detected below treat every entry as a separate type

      else: if a keyset `define/type/<TYPENAME>/#*` exists - treat each element is a separate type

## Sumtype 

### dummys

when array syntax is used: create dummy types for each element because it's easier to manage

## Type tree 

### initialize and update

a type definition will be only read once and currently wont be updated at runtime

new types can be added at runtime


