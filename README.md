## For Dev

To compile run: 
``` 
mirage configure -t xen --dhcp=<true|false>
```
To execute as a Xen-domU run: 
```
sudo xl create ./store.xl -c 'extra="repo=<repo-uri> token=<kernel-uuid> migration=<true|false>  ipv4=<kernel-ip> ipv4_gateway=<gateway-ip> resolver=<dns-ip> resolver-port=<dns-port>"'
```
* "repo-uri" is the URI from which the kernel will retrieve it's state data.
* "token" is the token assigned to the kernel. Used to authenticate and identify kerenl.
* Migration, if true will cause the kernel to stay in a steady state until MirageManager calls it to start functionality.
**If DHCP is used, the following values dont need to be set**
* "kernel-ip" definees the ip the kernel will assume on the br0 network of the xen host.
* "gateway-ip" definees the ip of the network gateway the kernel will use to route it's requests.
* "dhcp-ip" and "dns-port" define the location of the used DNS server

## For Deployment

To build a Unikernel to be ready for MirageManager run the build script with
´´´
./build_scriot.sh
```
