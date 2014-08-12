# How-To: kdb merge #

## Introduction ##

The kdb tool allows users to access and perform functions on the Elektra Key Database from the command line. We added
a new command to this very useful tool, the merge command. This command allows a user to perform a three-way merge 
of KeySets from the kdb tool. 

The command to use this tool is:

	kdb merge [options] ourpath theirpath basepath resultpath

The standard naming scheme for a three-way merge consists of ours, theirs, and base:
*	ours refers to the local copy of a file
*	theirs refers to a remote copy
*	base refers to their common anscestor. 

This works very similarly for KeySets, especially ones that consist of mounted conffiles. 

For mounted conffiles:
*	ours should be the user's copy
*	theirs would be the maintainers copy, 
*	base would be the conffile as it was during the last package upgrade or during the package install. 

If you are just trying to merge any two KeySets that derive from the same base, ours and theirs can be interchanged. 
In kdb merge, ourpath, theirpath, and basepath work just like ours, theirs, and base except each one represents the 
root of a KeySet. Resultpath is pretty self-explanatory, it is just where you want the result of the merge to be saved under. 

## Options ##

As for the options, there are a few basic options:
	
	-i  --interactive			which attempts the merge in an interactive way
	
	-t  --test					which tests the propsed merge and informs you about possible conflicts
    
	-b --overrideBase			which overwrites the base KeySet with the result.

### Strategies ###

Addtionally there is an option to specify a merge strategy, which is very important.

The option for strategy is:

	-s --strategy <name>		which is used to specify a strategy to use in case of a conflict

The current list of strategies are:

	preserve			the merge will fail if a conflict is detected
	
	ours				the merge will use our version during a conflict
	
	theirs				the merge will use their version during a conflict
	
	base				the merge will use the base version during a conflict

If no strategy is specified, the merge will default to the preserve strategy as to not risk making the wrong decision. 
If any of the other strategies are specified, when a conflcit is detected, merge will use the Key specified by the
strategy (ours, theirs, or base) for the resulting Key. 

## Basic Example ##

Basic Usage:

	kdb merge system/hosts/ours system/hosts/theirs system/hosts/base system/hosts/result
	
## Examples Using Strategies ##
  
Here are examples of the same KeySets being merged using different strateigies.
The KeySets are mounted using the simpleini file, the left side of '=' is the name of
the Key, the right side is its string value.
  
We start with the base KeySet, system/base:

	key1=1  
	key2=2  
	key3=3  
	key4=4  
	key5=5  
	
Here is our KeySet, system/ours:

	key1=apple  
	key2=2  
	key3=3  
	key5=fish  
	
Here is their KeySet, system/theirs:

	key1=1  
	key2=pie  
	key4=banana  
	key5=5  
	
Now we will examine the result KeySet with the different strategies.

### Preserve ###

	kdb merge -s preserve system/ours system/theirs system/base system/result
	
The merge will fail because of a conflict for key4 since key4 was deleted in our KeySet and
edited in their KeySet. Since we used preserve, the merge fails and the result KeySet is not saved.

### Ours ###

	kdb merge -s ours system/ours system/theirs system/base system/result

The result KeySet, system/result will be:

	key1=apple  
	key2=pie  
	key3=3  
	key5=fish  
	
### Theirs ###

	kdb merge -s theirs system/ours system/theirs system/base system/result
	
The result KeySet, system/result will be:

	key1=apple
	key2=pie
	key4=banana
	key5=fish
	
### Base ###

	kdb merge -s base system/ours system/theirs system/base system/result
	
The result KeySet, system/result will be:

	key1=apple
	key2=pie
	key3=3
	key4=4
	key5=5
