To compile run: 
``` 
mirage configure -t xen
```
To execute as a Xen-domU run: 
```
sudo xl create ./store.xl -c 'extra="origin_uri=<repo-uri> uuid=<kernel-uuid> ipv4=<kernel-ip>"'
```
* "kernel-uuid" is used to identify the kernel at the repo.
* "kernel-ip" definees the ip, the kernel will assume on the br0 network of the xen host
* "repo-uri" is the URI from which the kernel will retrieve it's state data for the uuid given.
It's optional, if none is given the kernel will use it's default values
