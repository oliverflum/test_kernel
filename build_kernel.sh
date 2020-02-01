#! /bin/bash
make clean
rm ./static/* ./dhcp/*
mkdir -p static dhcp &&
mirage configure -t xen --dhcp=true && 
make depend && 
make &&
find -iname '*.xl' -exec cp {} ./dhcp \; &&
find -iname '*.xen' -exec cp {} ./dhcp \; &&
make clean
mirage configure -t xen --dhcp=false && 
make &&
find -iname '*.xl' -exec cp {} ./static \; &&
find -iname '*.xen' -exec cp {} ./static \; &&
make clean