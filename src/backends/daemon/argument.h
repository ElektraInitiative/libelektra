
/* Struct which store an RPC Argument */
typedef struct {
	DataType type;	/* Type of this args (see ArgType)	*/
	
	union {
		void	*complexData;	/* Complex data (struct)	*/
		char	*string;	/* String data			*/
		int	integer;	/* integer data			*/
	} data;
} Argument;

Argument *argumentNew();
int argumentInit(Argument *arg);
int argumentSetValue(Argument *arg, DataType type, const void *value);
int argumentClose(Argument *arg);
int argumentDel(Argument *arg);
