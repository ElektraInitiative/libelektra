kdb-merge(1) -- Three-way merge of KeySets.
================================
## DESCRIPTION
Does a three-way merge between keysets.  
On success the resulting keyset will be saved to mergepath.  
On unresolved conflicts nothing will be changed.  

## THREE-WAY MERGE
The `kdb merge` command uses a three-way merge by default.  
A three-way merge is when three versions of a file (or in this case, KeySet) are compared in order to automatically merge the changes made to the KeySet over time.  
These three versions of the KeySet are:  

* `base`:
  The `base` KeySet is the original version of the KeySet.  
  Sometimes the `base` is called the "parent" since it is the KeySet that the others derive from.

* `ours`:
  The `ours` KeySet represents the user's current version of the KeySet.  
  This would only differ from `base` if the user has made any changes to any of the Keys contained within.  

* `theirs`:
  The `theirs` KeySet usually represents the default version of a KeySet (usually the package maintainer's version).  
  This would differ from `base` for a variety of reasons but especially if the default has been changed for a new version of software.  

The three-way merge works by comparing the `ours` KeySet and the `theirs` KeySet to the `base` KeySet. By looking for differences  in these KeySets, a new KeySet called `result` is created that represents a merge of these KeySets.  

## USAGE
`kdb merge [options] ourpath theirpath basepath resultpath`  

* ourpath:
  Path to the keyset to serve as `ours`  

* theirpath:
  path to the keyset to serve as `theirs`  

* basepath:
  path to the `base` keyset  

* resultpath:
  path without keys where the merged keyset will be saved    

## CONFLICTS
Conflicts occur when a Key has a different value in all three KeySets.  
Conflicts in a merge can be resolved using a [strategy](#STRATEGIES) with the `-s` argument.  

## STRATEGIES
Currently the following strategies exist:  

 * preserve:
   Merge only those keys where just one side deviates from base (default).  

 * ours:
   Whenever a conflict exists, use our version.  

 * theirs:
   Whenever a conflict exists, use their version.  
    
 * cut:
   Removes existing keys below the resultpath and repalces them with the merged keyset.  

 * import:
   Preserves existing keys in the resultpath if they do not exist in the merged keyset.  
   If the key does exist in the merged keyset, it will be overwritten.   

## EXAMPLES
To complete a simple merge of three KeySets:  
    `kdb merge user/ours user/theirs user/base user/result`  

To complete a merge whilst using the `ours` version of the KeySet to resolve conflicts:  
    `kdb merge -s ours user/ours user/theirs user/base user/result`  

To complete a three-way merge and overwrite all current keys in the `resultpath`:  
    `kdb merge -s cut user/ours user/theirs user/base user/result`  

