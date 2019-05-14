**Memory Value Plugin**

Memory Value Plugin checks the correct format of memory specifications.

E.g. setting the max RAM consumption of JVM

*maxMem=2048MB valid*

*maxMem=2048MXB invalid*

*maxMem=2048P invalid*

*maxMem=2048 invalid*

the plugin checks wether the string is valid memory setting or not  and normalizes its value to bytes, thus other methods can easily process the value.

