kdb-merge(1) -- Three-way merge of KeySets.
================================
## DESCRIPTION
Does a three-way merge between keysets.  
On success the resulting keyset will be saved to mergepath.  
On unresolved conflicts nothing will be changed.  

## USAGE
`kdb merge [options] ourpath theirpath basepath resultpath`  

* ourpath:
  Path to the keyset to serve as ours  

* theirpath:
  path to the keyset to serve as theirs  

* basepath:
  path to the base keyset  

* resultpath:
  path without keys where the merged keyset will be saved    

## CONFLICTS
Conflicts in a merge can be resolved using a [strategy](#STRATEGIES) with the `-s` argument.  

## STRATEGIES
Currently the following strategies exist:  

 * preserve:
   Merge only those keys where just one side deviates from base (default).  

 * ours:
   Whenever a conflict exists, user our version.  

 * theirs:
   Whenever a conflict exists, user their version.  
    
 * cut:
   Removes existing keys below the resultpath and repalces them with the merged keyset.  

 * import:
   Preserves existing keys in the resultpath if they do not exist in the merged keyset.  
   If the key does exist in the merged keyset, it will be overwritten.   

