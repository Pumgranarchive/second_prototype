#!/bin/sh

sudo iptables -t nat -A OUTPUT -p tcp -d pumgrana.com -j DNAT --to-destination 127.0.1.1:8081