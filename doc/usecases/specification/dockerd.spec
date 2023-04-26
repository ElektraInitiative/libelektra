[runtimes/_]
meta:/type = string
meta:/description = Additional OCI compatible runtime
meta:/example = custom

[runtimes/_/path]
meta:/type = string
meta:/description = The path to the OCI compatible runtime
meta:/example = /usr/local/bin/my-runc-replacement

[runtimes/_/runtimeArgs]
meta:/array/min = 0
meta:/description = List of runtime arguments for the OCI compatible runtime
meta:/example = --debug

[allow-nondistributable-artifacts]
meta:/array/min = 0
meta:/description = Allow push of nondistributable artifacts to registry (list)

[api-cors-header]
meta:/type = string
meta:/description = Set CORS headers in the Engine API

[authorization-plugins]
meta:/array/min = 0
meta:/description = Authorization plugins to load

[bip]
meta:/type = string
meta:/description = Specify network bridge IP

[bridge]
meta:/type = string
meta:/description = Attach containers to a network bridge

[cgroup-parent]
meta:/type = string
meta:/description = Set parent cgroup for all containers

[config-file]
meta:/type = string
meta:/description = Daemon configuration file
meta:/default = /etc/docker/daemon.json

[containerd]
meta:/type = string
meta:/description = containerd grpc address

[containerd/namespace]
meta:/type = string
meta:/description = Containerd namespace to use
meta:/default = moby

[containerd/plugins-namespace]
meta:/type = string
meta:/description = Containerd namespace to use for plugins
meta:/default = plugins.moby

[cpu-rt/period]
meta:/type = int
meta:/description = Limit the CPU real-time period in microseconds for the parent cgroup for all containers (not supported with cgroups v2)

[cpu-rt/runtime]
meta:/type = int
meta:/description = Limit the CPU real-time runtime in microseconds for the parent cgroup for all containers (not supported with cgroups v2)

[cri-containerd]
meta:/type = boolean
meta:/description = start containerd with cri

[data-root]
meta:/type = string
meta:/description = Root directory of persistent Docker state
meta:/default = /var/lib/docker

[debug]
meta:/type = boolean
meta:/description = Enable debug mode

[default/address-pools]
meta:/array/min = 0
meta:/description = Default address pools for node specific local networks (list)

[default/address-pools/#/base]
meta:/type = string
meta:/description = Ip address (ipv4) + subnet
meta:/example = 172.30.0.0/16

[default/address-pools/#/size]
meta:/type = int
meta:/description = Number of ip addresses in this pool with base
meta:/example = 24

[default/cgroupns-mode]
meta:/description = Default mode for containers cgroup namespace
meta:/default = private
meta:/check/enum = #1
meta:/check/enum/#0 = host
meta:/check/enum/#1 = private

[default/gateway]
meta:/check/ipaddr = ipv4
meta:/description = Container default gateway IPv4 address

[default/gateway-v6]
meta:/check/ipaddr = ipv6
meta:/description = Container default gateway IPv6 address

[default/ipc-mode]
meta:/description = Default mode for containers ipc
meta:/check/enum = #1
meta:/default = private
meta:/check/enum/#0 = shareable
meta:/check/enum/#1 = private

[default/runtime]
meta:/type = string
meta:/description = Default OCI runtime for containers
meta:/default = runc

[default/shm-size]
meta:/type = string
meta:/description = Default shm size for containers
meta:/default = 64MiB

[default/ulimits/_]
meta:/type = string
meta:/description = Default ulimits for containers
meta:/example = 64000

[default/ulimits/_/Hard]
meta:/type = int
meta:/description = Hard limit for ulimit
meta:/example = 64000

[default/ulimits/_/Name]
meta:/type = string
meta:/description = Name for the ulimit. Matches the globbing _ name in default/ulimits/_
meta:/example = nofile

[default/ulimits/_/Soft]
meta:/type = int
meta:/description = Soft limit for ulimit

[dns/_]
meta:/type = string
meta:/description = DNS server to use

[dns/opt/_]
meta:/type = string
meta:/description = DNS options to use

[dns/search/_]
meta:/type = string
meta:/description = DNS search domains to use

[exec/opt/_]
meta:/type = string
meta:/description = Runtime execution options

[exec/root/_]
meta:/type = string
meta:/description = Root directory for execution state files
meta:/default = /var/run/docker

[experimental]
meta:/type = boolean
meta:/description = Enable experimental features
meta:/default = false

[fixed-cidr]
meta:/type = string
meta:/description = IPv4 subnet for fixed IPs

[fixed-cidr-v6]
meta:/type = string
meta:/description = IPv6 subnet for fixed IPs

[group]
meta:/type = string
meta:/description = Group for the unix socket
meta:/default = docker

[host/_]
meta:/type = string
meta:/description = Daemon socket(s) to connect to

[host-gateway-ip]
meta:/enum/ipaddr = ipv4
meta:/description = IP address that the special 'host-gateway' string in add-host resolves to. Defaults to the IP address of the default bridge.

[http-proxy]
meta:/type = string
meta:/description = HTTP proxy URL to use for outgoing traffic

[https-proxy]
meta:/type = string
meta:/description = HTTPS proxy URL to use for outgoing traffic

[icc]
meta:/type = boolean
meta:/description = Enable inter-container communication
meta:/default = true

[init]
meta:/type = boolean
meta:/description = Run an init in the container to forward signals and reap processes
meta:/default = false

[init-path]
meta:/type = string
meta:/description = Path to the docker-init binary

[insecure-registries]
meta:/array/min = 0
meta:/description = Enable insecure registry communication by specifying the insecure-registries. (list)

[ip]
meta:/description = Default IP when binding container ports
meta:/enum/ipaddr = ipv4
meta:/default = 0.0.0.0

[ip-forward]
meta:/type = boolean
meta:/description = Enable net.ipv4.ip_forward
meta:/default = true

[ip-masq]
meta:/type = boolean
meta:/description = Enable IP masquerading
meta:/default = true

[iptables]
meta:/type = boolean
meta:/description = Enable addition of ip6tables rules
meta:/default = false

[iptables]
meta:/type = boolean
meta:/description = Enable addition of iptables rules
meta:/default = true

[ipv6]
meta:/type = boolean
meta:/description = Enable IPv6 networking
meta:/default = false

[labels/_]
meta:/type = string
meta:/description = Set key=value labels to the daemon

[live-restore]
meta:/type = boolean
meta:/description = Enable live restore of docker when containers are still running
meta:/default = false

[log/driver]
meta:/type = boolean
meta:/description = Default driver for container logs
meta:/default = json-file

[log/level]
meta:/description = Set the logging level
meta:/enum/check = #4
meta:/enum/check/#0 = debug
meta:/enum/check/#1 = info
meta:/enum/check/#2 = warn
meta:/enum/check/#3 = error
meta:/enum/check/#4 = fatal
meta:/default = info

[log/opts/cache/disabled]
meta:/type = boolean
meta:/description = Log option to disable cache
meta:/example = false

[log/opts/cache/max-file]
meta:/type = int
meta:/description = Log option to specify max file of cache
meta:/example = 4

[log/opts/cache/max-size]
meta:/type = int
meta:/description = Log option to specify max size of cache
meta:/example = 20m

[log/opts/cache/compress]
meta:/type = boolean
meta:/description = Log option to specify if cache needs to be compressed
meta:/example = true

[log/opts/env]
meta:/type = string
meta:/description = Log option to specify environment of log file
meta:/example = os,customer

[log/opts/labels]
meta:/type = string
meta:/description = Log option to specify labels
meta:/example = labels

[log/opts/max-file]
meta:/type = string
meta:/description = Log option to specify max file
meta:/example = 4

[log/opts/max-size]
meta:/type = string
meta:/description = Log option to specify max size
meta:/example = 10m

[max-concurrent-downloads]
meta:/type = int
meta:/description = Set the max concurrent downloads
meta:/default = 3

[max-concurrent-uploads]
meta:/type = int
meta:/description = Set the max concurrent uploads
meta:/default = 5

[max-download-attempts]
meta:/type = int
meta:/description = Set the max download attempts for each pull
meta:/default = 5

[metrics-addr]
meta:/type = string
meta:/description = Set default address and port to serve the metrics api on

[mtu]
meta:/type = int
meta:/description = Set the containers network MTU
meta:/default = 1500

[network-control-plane-mtu]
meta:/type = int
meta:/description = Network Control plane MTU
meta:/default = 1500

[no-new-privileges]
meta:/type = boolean
meta:/description = Set no-new-privileges by default for new containers
meta:/default = false

[no-proxy]
meta:/type = string
meta:/description = List of hosts or IP addresses for which the proxy is skipped
meta:/array/min = 0

[node-generic-resource]
meta:/array/min = 0
meta:/description = List of advertise user-defined resources
meta:/example = NVIDIA-GPU=UUID1

[oom-score-adjust]
meta:/type = int
meta:/description = Set the oom_score_adj for the daemon

[pidfile]
meta:/type = string
meta:/description = Path to use for daemon PID file
meta:/default = /var/run/docker.pid

[raw-logs]
meta:/type = boolean
meta:/description = Full timestamps without ANSI coloring
meta:/default = false

[registry-mirrors]
meta:/array/min = 0
meta:/description = List of preferred registry mirror

[rootless]
meta:/type = boolean
meta:/description = Enable rootless mode; typically used with RootlessKit
meta:/default = false

[seccomp-profile]
meta:/type = string
meta:/description = Path to seccomp profile. Use "unconfined" to disable the default seccomp profile.
meta:/default = builtin

[selinux-enabled]
meta:/type = boolean
meta:/description = Enable selinux support
meta:/default = false

[shutdown-timeout]
meta:/type = int
meta:/description = Set the default shutdown timeout
meta:/default = 15

[storage/driver]
meta:/type = string
meta:/description = Storage driver to use

[storage/opts]
meta:/array/min = 0
meta:/description = List of storage driver options

[swarm-default-advertise-addr]
meta:/type = string
meta:/description = Set default address or interface for swarm advertised address

[tls]
meta:/type = boolean
meta:/description = Use TLS; implied by tls/verify
meta:/default = false

[tls/cacert]
meta:/type = string
meta:/description = Trust certs signed only by this CA
meta:/default = ~/.docker/ca.pem

[tls/cert]
meta:/type = string
meta:/description = Path to TLS certificate file
meta:/default = ~/.docker/cert.pem

[tls/key]
meta:/type = string
meta:/description = Path to TLS key file
meta:/default = ~/.docker/key.pem

[tls/verify]
meta:/type = boolean
meta:/description = Use TLS and verify the remote
meta:/default = false

[userland/proxy]
meta:/type = boolean
meta:/description = Use userland proxy for loopback traffic
meta:/default = true

[userland/proxy/path]
meta:/type = string
meta:/description = Path to the userland proxy binary

[userns-remap]
meta:/type = string
meta:/description = User/Group setting for user namespaces

[validate]
meta:/default = boolean
meta:/description = Validate daemon configuration and exit
