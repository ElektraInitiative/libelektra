size_t keySerializeGetSize(const Key *key);
size_t keySetSerializeGetSize(KeySet *ks);
size_t messageSerializeGetSize(const Message *msg);
size_t argumentSerializeGetSize(const Argument *arg);

ssize_t keySerialize(const Key *key, void *data, size_t dataSize);
ssize_t keyUnserialize(const void *data, Key *key);

ssize_t keySetUnserialize(const void *data, KeySet *returned);
ssize_t keySetSerialize(KeySet *ks, void *data, size_t dataLen);

ssize_t messageSerialize(const Message *msg, void *data, size_t dataSize);
ssize_t messageUnserialize(const void *data, Message *returned);

ssize_t argumentSerialize(const Argument *arg, void *data, size_t dataSize);
ssize_t argumentUnserialize(const void *data, Argument *returned);

