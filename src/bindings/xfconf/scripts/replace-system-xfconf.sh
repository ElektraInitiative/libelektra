#!/bin/sh

mv /usr/lib/libxfconf-0.so.3.0.0 /usr/lib/libxfconf-0.so.3.0.0.bak
cp ./lib/libxfconfbinding.so /usr/lib/libxfconf-0.so.3.0.0
