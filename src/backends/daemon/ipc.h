int ipc_connect(int s,const char *p);
int ipc_stream(void);
int ipc_bind(int s,const char *p);
int ipc_bind_reuse(int s,const char *p);
int ipc_accept(int s,char *p,int l,int *trunc);
int ipc_local(int s,char *p,int l,int *trunc);
int ipc_listen(int s,int backlog);
int ipc_eid(int s,int *u,int *g);

int ndelay_on(int fd);
int ndelay_off(int fd);

int getpeereid(int s,uid_t *u,gid_t *g);
	
