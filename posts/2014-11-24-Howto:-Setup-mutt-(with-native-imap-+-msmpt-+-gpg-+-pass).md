---
title: Howto: Setup mutt (with native imap + msmpt + gpg + pass)
description: Howto setup mutt properly using nativ imap, msmpt, gpg and password encryption via a password manager
tags: Application Linux Mutt
---

__If you ever tried to setup _mutt_? You might encountered problems with all the configuration stuff which is needed to setup _mutt_ properly. For a long time I've been searching a solution for viewing emails remotely. That's why I give mutt another chance, to prove its greatness.__


![](http://none.io/share/mutt.png)


##Configure...

As already mentioned the most tedious part of using mutt is to grasp the configuration which is needed to bring mutt in a state where it is running fine.
What I wanted to configure:

- Multiple email accounts with same host using native imap/smpt support of mutt
- Gpg support for each account
- Using `pass` to store passwords
- A usable theme

__My folder structure looks like:__


~~~
 ~/.mutt
   |- muttrc
   |- acc.account1
   |- acc.account2
   |- cache
     | ...mutt generated header cache...
   |- colors
     |- mutt-colors-blue.muttrc
~~~

##Basic configuration

Before we setup gpg, pass and multi accounts we setup the  basic configurations:

_~/.mutt/muttrc_:

~~~{.bash .numberLines}

# Basic email releated settings
set header_cache = "~/.mutt/cache" 
set imap_check_subscribed
set imap_keepalive = 300
unset imap_passive
set mail_check = 60

# Set this path if you need not authorized certificates
#set certificate_file="~/.mutt/certs"

# basic bindings bindings

# switch between folders
macro index 'c' '<change-folder>?<change-dir><home>^K=<enter>'
# mark all as read
macro index <esc>m "T~N<enter>;WNT~O<enter>;WO\CT~T<enter>" "mark all messages read"
# jump to next mailbox with unread messages
bind index,pager n next-unread-mailbox

set sendmail="/usr/bin/msmtp" 
set envelope_from=yes
set edit_hdrs

set sort=threads
set sort_aux=date-sent


# nice date formats taken from ubuntuusers.de
set date_format="%a, %d. %b %H:%M"
set index_format="%4C %Z %D %-22.22F (%?l?%4l&%4c?) %s"
set folder_format="%2C %8s %d %t %N %f"
set pager_index_lines=10

~~~

##GPG
Fortunately now days most distributions already come with a handy configuration which makes configuring mutt really easy. In my case (Archlinux) it was just adding

    source /etc/Muttrc.gpg.dist
 
to my `~/.mutt/muttrc`.


##Password manager pass

I use [pass](http://www.passwordstore.org/) as password manager.

To get a password into mutt a custom variable per password is needed. __(Beware mutt only allows user variables starting with "my_")__


For each password I added

    set my_pass_acc1=`pass email/example/acc1`

to my `~/.mutt/muttrc`. This will make that pass is invoked on startup which will ask you the encryption password once.


####Multiple Accounts

To set up multiple accounts we need to add an account and a folder `Hook`. For each account we setup a key binding to select the email account, an `account-hook` to set user name and password (and extra configuration) and a `folder-hook` to load configurations from a separate file.


For each account you want to add put this into your `~/.mutt/muttrc`

~~~{.bash}
macro index,pager <f5> "<change-folder>imaps://acc1@example.com<enter>"
account-hook imaps://acc1@example.com/* "set imap_user="acc1" imap_pass=$$my_pass_acc1"
folder-hook imaps://acc1@example.com/* 'source ~/.mutt/acc.acc1'
~~~

and create a configuration for each account which will be sourced by `folder-hook`. `~/.mutt/acc.acc1`:

~~~{.bash .numberLines}
set realname='Clara Fall'
set from=acc1@example.com
set hostname="example.com"
set signature="some signature"
set ssl_force_tls=yes
set ssl_starttls = yes

#imap
set imap_user=acc1
set imap_pass=$$my_pass_acc1
set folder = imaps://acc1@example.com/
set spoolfile = +INBOX
set postponed = +Drafts
set record = +Sent

#smtp
set smtp_url="smtp://acc1@example.com:587"
set smtp_pass=$$my_pass_acc1
set ssl_starttls=yes
set ssl_force_tls=yes


~~~
Both `folder` and `smpt_url` follow this syntax:

    SSL:
    [imaps|smtps]://user@server:<port>/<imapfolder>
    
    NON SSL or activated via options:
    [imap|smtp]://user@server:<port>/<imapfolder>
    
    
Because I already set `msmpt` in my `~/.mutt/muttrc` it worked flawless only settings smpt settings in my `acc` file.

_NOTE: You need to set `envelope_form` to yes to make `msmtp` work!_


Nice now mutt works with multiple accounts + imap + smpt + pass password manager!

At the end: I use a nice blue mutt theme from [here](https://elric80.wordpress.com/mutt-2/colors/).

Have fun!

