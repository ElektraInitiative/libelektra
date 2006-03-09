typedef struct {
	uint32_t  magic;
	int16_t   version;

	size_t    dataLen;
} ProtocolHeader;

ssize_t protocolSendMessage(int fd, const Message *msg);
ssize_t protocolReadMessage(int fd, Message *msg);

