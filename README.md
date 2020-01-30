To compile run: 
``` 
mirage configure -t xen
```
To execute as a Xen-domU run: 
```
sudo xl create ./store.xl -c 'extra="repo=<repo-uri> token=<kernel-uuid> migration=<true|false>  ipv4=<kernel-ip> ipv4_gateway=<gateway-ip>"'
```
* "repo-uri" is the URI from which the kernel will retrieve it's state data.
* "token" is the token assigned to the kernel. Used to authenticate and identify kerenl
* "kernel-ip" definees the ip the kernel will assume on the br0 network of the xen host
* "gateway-ip" definees the ip of the network gateway the kernel will use to route it's requests
