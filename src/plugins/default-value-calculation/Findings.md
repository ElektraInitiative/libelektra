# Default value calculation

This Markdown file shortly describes my found requirements for default value calculation:

1) Default to a part of the keyname

    Example in lcdexec:
    ```ini
    # definition of a menu
    [CPUinfo]
    DisplayName="Show CPU"
    ```
    In the above example the `DisplayName` which is set to "Show CPU" should have the default value `CPUinfo` which is part of its keyname.

2) Default depends on a value of another key

    Example in LCDd:
    ```ini
    [CwLnx]

    # Select the LCD model [default: 12232; legal: 12232, 12832, 1602]
    Model=12232

    # Select the LCD size. Default depends on model:
    # 12232: 20x4
    # 12832: 21x4
    # 1602: 16x2
    Size=20x4
    ```

    This basically boils down to a simple `switch` statement.

3) The default copies the value of another key

    Example in Cassandra:
    ```yaml
    roles_validity_in_ms: 2000

    #If roles_validity_in_ms is non-zero, then this must be also.
    # Defaults to the same value as roles_validity_in_ms.
    roles_update_interval_in_ms: 2000

    # Leaving this blank will set it to the same value as listen_address
    broadcast_address: 1.2.3.4
    ```
    Many more examples such as this are there too

    Basically a conditional block

4) Computation + if condition + setting

    ```yaml
    # If max_mutation_size_in_kb is set explicitly then commitlog_segment_size_in_mb must be set to at least twice the size of max_mutation_size_in_kb / 1024
    commitlog_segment_size_in_mb: 32

    # memtable_cleanup_threshold defaults to 1 / (memtable_flush_writers + 1)
    memtable_cleanup_threshold: 0.11
    ```

# Auto-Detection

Cassandra:

1) Some examples of calculations based on the target system

```yaml
# Default value ("auto") is 1/256th of the heap or 10MB, whichever is greater
prepared_statements_cache_size_mb:

# Default value ("auto") is 1/256th of the heap or 10MB, whichever is greater
thrift_prepared_statements_cache_size_mb:

# "auto" means (min(5% of Heap (in MB), 100MB)). Set to 0 to disable key cache.
key_cache_size_in_mb:

# "auto" means (min(2.5% of Heap (in MB), 50MB)). Set to 0 to disable counter cache.
# NOTE: if you perform counter deletes and rely on low gcgs, you should disable the counter cache.
counter_cache_size_in_mb:

# the ideal number of "concurrent_writes" is dependent on the number of cores in
# your system; (8 * number_of_cores) is a good rule of thumb.
concurrent_writes: 32

# "concurrent_reads" should be set to (16 * number_of_drives) in
# order to allow the operations to enqueue low enough in the stack
# that the OS and drives can reorder them. Same applies to
# "concurrent_counter_writes", since counter writes read the current
# values before incrementing and writing them back.
concurrent_reads: 32
concurrent_counter_writes: 32

# Defaults to the smaller of 1/4 of heap or 512MB.
file_cache_size_in_mb: 512

# Cassandra will set both to 1/4 the size of the heap.
memtable_heap_space_in_mb: 2048
memtable_offheap_space_in_mb: 2048

# The default value is the smaller of 8192, and 1/4 of the total space
# of the commitlog volume.
commitlog_total_space_in_mb: 8192

# memtable_flush_writers defaults to two for a single data directory.
# This means that two  memtables can be flushed concurrently to the single data directory. If you have multiple data directories the default is one memtable flushing at a time but the flush will use a thread per data directory so you will get two or more writers.
memtable_flush_writers: 2

# The default value is the min of 4096 mb and 1/8th of the total space
# of the drive where cdc_raw_directory resides.
cdc_total_space_in_mb: 4096

# If left empty, this will default to 5% of the heap size
index_summary_capacity_in_mb:

# The default is sync because on Windows hsha is about 30% slower.  On Linux,
# sync/hsha performance is about the same, with hsha of course using less memory.
rpc_server_type: sync

# concurrent_compactors defaults to the smaller of (number of disks,
# number of cores), with a minimum of 2 and a maximum of 8.
concurrent_compactors: 1
```

# Misc

Also auto detection but should throw error if not installed. So its hybrid between spec and auto detection in my opinion.

```yaml
# Warning: before enabling this property make sure to ntp is installed
# and the times are synchronized between the nodes.
cross_node_timeout: false
```