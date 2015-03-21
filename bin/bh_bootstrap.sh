#!/bin/bash

cd ~/

sudo apt-get -y install libgmp10 htop ocaml-findlib parallel g++; 
tar xf ~/build_host_tools.tar.bz2; 
mkdir -p stdlib_tests/src; 
sudo mkdir -p /opt/rust-alpha 2> /dev/null; 
sudo chown ubuntu:ubuntu /opt/rust-alpha; 
tar xf ~/rust_code.tar.bz2 -C /
cd src/
/opt/rust-alpha/bin/rustc -A warnings -C rpath main.rs
cp rbmc ../bin/
