#include "message.h"

Message *wrapper_kdbOpen(KDBHandle *handle, Message *request, uid_t remoteeuid, gid_t remoteegid);
Message *wrapper_kdbClose(KDBHandle *handle, Message *request);
Message *wrapper_kdbStatKey(KDBHandle handle, Message *request);
Message *wrapper_kdbGetKey(KDBHandle handle, Message *request);
Message *wrapper_kdbSetKey(KDBHandle handle, Message *request);
Message *wrapper_kdbSetKeys(KDBHandle handle, Message *request);
Message *wrapper_kdbRename(KDBHandle handle, Message *request);
Message *wrapper_kdbRemoveKey(KDBHandle handle, Message *request); 
Message *wrapper_kdbGetChild(KDBHandle handle, Message *request);
Message *wrapper_kdbMonitorKey(KDBHandle handle, Message *request);
Message *wrapper_kdbMonitorKeys(KDBHandle handle, Message *request); 

