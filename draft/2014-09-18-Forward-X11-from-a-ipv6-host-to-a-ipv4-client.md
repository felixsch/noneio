---
title: Forward X11 from a ipv6 host to a ipv4 client
description: ssh x11 forwarding from a client to a host via gateway
tags: ssh x11 forwarding ipv6
---
Since my new ISP uses ipv6 I was confronted with the problem: How should I use my remote window to test software (Xephyr in my case) when the clients ISP does not support ipv6.

Thanksfully my vservers supports ipv6. In this combination I was able to put up a ipv4 -> ipv6 gateway with X11 forwarding.

_Note: This method does also work with more than one gateway and ipv4 only_

## Lets go!

Here my setup:

* __A__ - my Client
* __B__ - my Server
* __C__ - the remote host I want to connect with X11 forwarding

The ipv6 synatix in ssh looks like: `\[x:x:x:x\]`



### Create a tunnel to C via B from A
 
    ssh -N -L <someport>:user@A:<portofC> user@B &
    
### Connect to remote Host via tunnel

    ssh -X userFromC@localhost -p <someport>

__Yeay ready to use X11 forwarding!!!__

To speed up the connection you can set some option on which encryption method is used!

Currently I'm using: `ssh -XC -c blowfish-cbc,arcfour`
