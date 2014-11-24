---
title: Testing Centos 7 as Desktop Linux
description: Testing Centos 7 as Desktop Linux for daily use
tags: Linux Centos
---

__After using Fedora for quite a time, I wanted to test Centos for my workstation. Centos is a really nice Linux Distribution which I already use as a server operating system. Now I want to know how it performs as Desktop Distribution.__



![](http://none.io/share/c7-screenshot.png)



Centos is a stable _rpm_ based Linux Distribution which uses sources derived from Red Hat Enterprise Linux (RHEL).

With Centos 7 _Initd_ was replaced with _systemd_ and the Gnome Desktop got a major version bump (from v2.x to v3.x). Centos now supports the _XFS_ file system.

##_Let's start testing Centos 7:_


Here are the things I usually doing on the desktop machine:

* Developing Haskell and C applications
* Using `VirtualBox` to test some distributions or using snapshots to test software under various conditions
* Playing games via _Steam_ (which requires proprietary NVidia video driver)

##Step 1: Download and Installation
I selected a net install ISO from [http://isoredirect.centos.org/centos/7/isos/x86_64/](http://isoredirect.centos.org/centos/7/isos/x86_64/).
To install you need to input a package mirror where the packages should be downloaded. (A source could be [http://mirror.centos.org/centos/7/os/x86_64/](http://mirror.centos.org/centos/7/os/x86_64/)).

Centos uses the same installer as Fedora. The installer creates a default partition scheme using XFS as default file system. Setting up users and password was no problem.

Two clicks later and 2 min waiting.. Voila ready to reboot!

##Step 2: Third-Party repositories and proprietary NVidia video driver

Because the Centos basic repository does not include a lot of software I need I activated a few third-party repositories:

 - __EPEL:__ A third party repository enables a lot of software which is available in Fedora is maintained by the fedora community ([http://fedoraproject.org/wiki/EPEL](http://fedoraproject.org/wiki/EPEL))
 - __ELRepo:__ A repository focusing on driver support for Centos 5/6/7. ([http://elrepo.org/](http://elrepo.org/))
 - __nux-desktop__: A media repository which offers packages for VLC and steam
 
To enable _Epel_:

    yum install epel-release

To install _ELRepo_:

    rpm --import https://www.elrepo.org/RPM-GPG-KEY-elrepo.org
    rpm -Uvh http://www.elrepo.org/elrepo-release-7.0-2.el7.elrepo.noarch.rpm
Finally I added _nux-desktop_ for packages of _vlc_ and _steam_:

    rpm -Uvh http://li.nux.ro/download/nux/dextop/el7/x86_64/nux-dextop-release-0-1.el7.nux.noarch.rpm


##Step 3: Installing Xfce and NVidia drivers

I neither like Gnome or KDE. That is why I always install Xfce as desktop environment.


    yum groups install "X Window System"
    yum groups install "Xfce"
    yum install faience-icon-theme
    yum install dejavu-sans-fonts dejavu-serif-fonts


This will install around __120Mb__. Because of the __ELRepo__ repository `kmod-nvidia` is now available for install:

    yum install kmod-nvidia

Before I rebooted I enabled _X_ at start-up.

    systemctl set-default graphical.target

Reboot!


##Step 4: Install all the things
After reboot I needed to setup icons and backgrounds. I also downloaded and installed the greybird theme.

Now I installed all things I need:

    yum install git vlc gvim ghc cabal-install firefox tmux steam ...

##Step 5: Haskell Environment
Centos 7 does not provide Haskell support from core. Fortunately Fedora does. That's why EPEL contains all needed packages to setup a Haskell environment.

EPEL currently ships: __ghc 7.6.3__ and __cabal-install 1.16.0.2__.

I updated _cabal-install_ and removed the package with yum because I want to use the _sandbox_ feature of _cabal_ (cabal >= 1.18 is needed).

    yum install zlib-devel
    cabal update
    cabal install cabal-install
    yum remove cabal-install

And added `$$HOME/.cabal/bin` to my _$$PATH_.

##Step 6: VirtualBox

To use VirtualBox I needed to enable the VirtualBox own `rpm` repository [www.virtualbox.org](www.virtualbox.org).

    cd /etc/yum.repos.d
    wget http://download.virtualbox.org/virtualbox/rpm/rhel/virtualbox.repo

I installed dkms before VirtualBox

    yum install dkms


and VirtualBox:


    yum install VirtualBox-4.3


Ready to rumble!

#Conclusion

Centos is an Enterprise Operating system which focuses on business and server applications. This means the development focus lies in stable server and basic office software. The result:

Unfortunately some packages were not available which I'm daily using (like pavucontrol which is not even available via third party repositories). The lack of a lot normal software makes it necessary to add a lot third party repositories. This clearly will negatively affect the stability in long a term. 

This is clearly a showstopper (because the main reason why I tested Centos was to gain more stability).

Despite the fact that a lot software I'm using was missing Centos was running really smooth and nice. Surprised by topicality of some software (e.g. GHC 7.6.3 which is also the latest version on Fedora 20), 

Conclusion in short: Nice server OS but as Desktop distribution I will stay with Fedora.

And a new Fedora 21 with xmonad and Xfce looks great too!

![](http://none.io/share/fedora21beta.png)


