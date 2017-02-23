#include <kdbplugin.h>

#define DEVBUILD
#undef VERBOSEBUILD

typedef enum
{
    ERROR = -1,
    SUCCESS = 1,
}RC;

typedef enum
{
    SUBTYPE,
    SUMTYPE,
}Types;

typedef struct
{
    Types type;
    Key *scope;
    KeySet *checks;
    KeySet *types;
}TypeConfig;

typedef int (*ValidateFunction)(Key *, Key *);

typedef struct
{
    Plugin *plugin;
    ValidateFunction f;
}PluginConfig;

typedef struct
{
    KeySet *modules;
    KeySet *plugins;
    KeySet *types;
}DispatchConfig; 


void freeTypes(KeySet *);
void closePlugins(KeySet *);
DispatchConfig * initDispatchConfig();
void closeDispatchConfig(Plugin *);
KeySet *getAllKeysBelow(const Key *, KeySet *);
KeySet *getKeysDirectBelow(const Key *, KeySet *);
int readTypeNames(DispatchConfig *, const Key *, const Key *, Key *parentKey);
int readTypeConfig(DispatchConfig *, const Key *, const Key *, KeySet *, Key *);
