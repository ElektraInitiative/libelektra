
void *wrapper_kdbOpen(void *request);
void *wrapper_kdbClose(void *request);
void *wrapper_kdbStatKey(void *request);
void *wrapper_kdbGetKey(void *request);

/* int wrapper_kdbSetKey(int nbArgs, Argument *args[], Message *reply);
int wrapper_kdbSetKeys(int nbArgs, Argument *args[], Message *reply);
int wrapper_kdbRename(int nbArgs, Argument *args[], Message *reply);
int wrapper_kdbRemoveKey(int nbArgs, Argument *args[], Message *reply); */
void *wrapper_kdbGetChild(void *request);
/* int wrapper_kdbMonitorKey(int nbArgs, Argument *args[], Message *reply);
int wrapper_kdbMonitorKeys(int nbArgs, Argument *args[], Message *reply); */

