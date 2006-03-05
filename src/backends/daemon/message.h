/* Message type */
typedef enum {
	MESSAGE_REQUEST,
	MESSAGE_REPLY
} MessageType;

/* Struct Message */
typedef struct {
	MessageType       type;
	
	unsigned int      procId; /* Procedure ID */
	
	int               nbArgs; /* # args */
	Argument          **args; /* Argument / return parameter */
} Message;

const Argument *messageStealArgByIndex(const Message *msg, int index);
Message *messageNew();
Message *messageNewRequest(int procedure, ...);
int messageInit(Message *msg);
int messageGetProcedureId(const Message *msg);
int messageAddArgument(Message *msg, Argument *argument);
const Argument *messageStealArgByIndex(const Message *msg, int index);
int messageClose(Message *msg);
int messageDel(Message *msg);

