
int messageSetData(Message *msg, const void *data, size_t dataLen)
{
	assert(msg != NULL);

	if ( msg->data ) {
		free(msg->data);
		msg->data = NULL;
		msg->dataLen = 0;
	}
	
	if ( (msg->data = malloc(dataLen)) == NULL )
		return -1;
	memcpy(msg->data, data, dataLen);
	msg->dataLen = dataLen;

	return 0;
}

Message *messageNew(MessageType type, int procedureId)
{
	Message	*new;

	new = (Message *) malloc(sizeof(Message));
	if ( new == NULL )
		return NULL;
	
	/* Init the message */
	memset(new, 0, sizeof(Message));
	new->type = type;
	new->procID = procedureId;

	return new;
}

MessageArg *argumentNew(DataType type, void *arg)
{
	MessageArg	*new;

	new = (MessageArg *) malloc(sizeof(MessageArg));
	new->type = type;
	
	
	if ( type == DATA_TYPE_INTEGER )
		new->data.integer = (int) arg;
	else if ( type == DATA_TYPE_STRING )
		new->data.string = (char *) arg;
	else if ( type >= DATA_TYPE_KEY && type < DATA_TYPE_LAST )
		new->data.complexData = arg;
	else if ( type != DATA_TYPE_UNKNOW ) {
		free(new);
		return NULL;
	}

	return new;
}

int messageAddArgs(Message *msg, MessageArg *argument)
{
	MessageArg	*new;
	MessageArg	**newArgs;
	assert(msg != NULL);
	assert(argument != NULL);
	
	/* Extend array for the new argument */
	newArgs = (MessageArg **) realloc(msg->args, (msg->nbArgs + 1) * sizeof(MessageArg *));
	if ( newArgs == NULL )
		return -1;
	msg->args = newArgs;

	msg->args[msg->nbArgs] = argument;
	msg->nbArgs++;

	return (ssize_t) msg->nbArgs;
}

MessageArg *messageStealArgByIndex(Message *msg, int index)
{
	assert(msg != NULL);

	if ( index < msg->nbArgs )
		return msg->args[index];

	return NULL;
}

int messageClose(Message *msg)
{
	int	i;

	assert(msg != NULL);
	
	for(i = 0 ; i<msg->nbArgs ; i++) {
		argumentDel(msg->args[i]);
	}
	free(msg->args);
	msg->nbArgs = 0;

	return 0;
}

int messageDel(Message *msg)
{
	assert(msg != NULL);

	messageClose(msg);
	free(msg);

	return 0;
}



void messageFree(Message *msg)
{
	free(msg->data);
	free(msg);
}

ssize_t messageWrite(int fd, const Message *msg)
{
	ssize_t	ret;
	ssize_t	written = 0;
	
	assert(msg != NULL);

	if ( (ret = write(fd, &msg->cmd, sizeof(msg->cmd))) == -1 )
		return -1;
	written += ret;
		
	if ( (ret = write(fd, &msg->dataLen, sizeof(msg->dataLen))) == -1 )
		return -1;
	written += ret;
	
	if ( (ret = write(fd, msg->data, msg->dataLen)) == -1 )
		return -1;
	written += ret;

	return written;
}

ssize_t messageRead(int fd, Message *returned)
{
	size_t	offset, block;
	ssize_t	ret;
	ssize_t read = 0;
	
	assert(returned != NULL);

	/* Read command */
	ret = read(fd, &msg->cmd, sizeof(msg->cmd));
	if ( ret == -1 )
		return -1;
	read += ret;

	/* Read data len */
	ret = read(fd, &msg->dataLen, sizeof(msg->dataLen));
	if ( ret == -1 )
		return -1;
	read += ret;

	/* Allocate memory for data */
	msg->data = malloc(msg->dataLen);
	if ( msg->data == NULL )
		return -1;

	/* Read data */
	block = MIN(msg->dataLen, SSIZE_MAX);
	for(offset = 0 ; (offset + block) < msg->dataLen ; offset += block) {
		ret = read(fd, &msg->data, msg->dataLen);
		if ( ret ==  -1 ) {
			free(msg->data);
			msg->dataLen = 0;
			return -1;
		}
	}

	return read;
}
