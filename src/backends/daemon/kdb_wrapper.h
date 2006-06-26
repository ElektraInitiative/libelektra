#include "message.h"

Message *wrapper_kdbOpen(Message *request);
Message *wrapper_kdbClose(Message *request);
Message *wrapper_kdbStatKey(Message *request);
Message *wrapper_kdbGetKey(Message *request);
Message *wrapper_kdbSetKey(Message *request);
Message *wrapper_kdbSetKeys(Message *request);
Message *wrapper_kdbRename(Message *request);
Message *wrapper_kdbRemoveKey(Message *request); 
Message *wrapper_kdbGetChild(Message *request);
Message *wrapper_kdbMonitorKey(Message *request);
Message *wrapper_kdbMonitorKeys(Message *request); 

