#! /bin/bash
make clean
rm ./static/* ./dhcp/*
mkdir -p static dhcp &&
mirage configure -t xen --dhcp=true && 
make depend && 
make &&
find -maxdepth 1 -iname '*.xl' -exec cp {} ./dhcp \; &&
find -maxdepth 1 -iname '*.xen' -exec cp {} ./dhcp \; &&
make clean
mirage configure -t xen --dhcp=false && 
make &&
find -maxdepth 1 -iname '*.xl' -exec cp {} ./static \; &&
find -maxdepth 1 -iname '*.xen' -exec cp {} ./static \; &&
make clean