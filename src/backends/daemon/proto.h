#include "datatype.h"

/* Struct which store an RPC Argument */
typedef struct {
	DataType type;	/* Type of this args (see ArgType)	*/
	
	union data {
		void	*complexData;	/* Complex data (struct)	*/
		char	*string:	/* String data			*/
		int	integer;	/* integer data			*/
	};
} MessageArg;

/* Message type */
typedef enum {
	MSG_REQUEST,
	MSG_REPLY
} MessageType;

/* Struct Message */
typedef struct {
	MessageType	type;		
	
	unsigned int	procId;		/* Procedure ID			*/
	
	int		nbArgs;		/* # args			*/
	MessageArg	*args[];	/* Argument / return parameter	*/
} Message;

