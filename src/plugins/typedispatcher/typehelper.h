#include <kdbplugin.h>

#define DEVBUILD
#define VERBOSEBUILD

// helper for readability 
typedef enum
{
    ERROR = -1,
    SUCCESS = 1,

}RC;

typedef enum
{
    SKEL,
    SUBTYPE,
    SUMTYPE,
}TypeType;


// configuration of a defined type
typedef struct
{
    TypeType type;
    Key *scope;		// name of the key where the type was defined
    KeySet *checks;	// holds keys with the check metadata for each type check
    KeySet *types;	// references to the keys with supertypes
    KeySet *params;	// parameter names + types
}TypeConfig;

// helpertype for pointers to validateKey functions
typedef int (*ValidateFunction)(Key *, Key *);

// helper struct for loaded plugins
typedef struct
{
    Plugin *plugin;		// address of loaded plugin
    ValidateFunction f;		// pointer to the validateKey function of the plugin
}PluginConfig;

// holds data needed by most functions
typedef struct
{
    KeySet *modules;		// modules keyset for PluginOpen/PluginClose
    KeySet *plugins;		// all loaded plugins. keyname == plugin name 
    				// value == PluginConfig
    KeySet *types;		// all defined types. keyname == name of type
    				// value == TypeConfig
}DispatchConfig; 


// splits type string into type and arguments
typedef struct
{
    char *type; 	//type name
    KeySet *args;	//a key for each argument, keynames are array indices,
    			// values the arguments
}ArgumentConfig;

//helpers.c functions
DispatchConfig * initDispatchConfig();
void closeDispatchConfig(Plugin *);
KeySet *getAllKeysBelow(const Key *, KeySet *);
KeySet *getKeysDirectBelow(const Key *, KeySet *);
TypeConfig *newTypeConfig();
void setTypeType(TypeConfig *, const Key *);
TypeType getTypeType(TypeConfig *);
Key *getTypeKey(DispatchConfig *, const char *);
TypeConfig *getType(DispatchConfig *, const char *);
ArgumentConfig *parseTypeString(DispatchConfig *, const char *);
void freeArgumentConfig(ArgumentConfig *);
KeySet *makeParamKS(KeySet *, ArgumentConfig *);

// typereader.c
int getTypeDefinitions(Key *, DispatchConfig *, Key *);

// typecheck.c
int validateKey(Key *, DispatchConfig *, Key *);

// pluginhelper.c
ValidateFunction getValidateFunction(DispatchConfig *, const char *);
