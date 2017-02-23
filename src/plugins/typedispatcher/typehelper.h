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


DispatchConfig * initDispatchConfig();
void closeDispatchConfig(Plugin *);
int getTypeDefinitions(Key *, DispatchConfig *, Key *);
int validateKey(Key *, DispatchConfig *, Key *);
