---
title: Fullscreen flash player with multiple monitors
description: Howto hack the flash player to stay Fullscreen even if focus leaves the screen
tags: Flash Fullscreen
---

I am usually looking documentations or livestream while procrastinating in front of my computer. Sadly, some of the websites I prefere still using the __madness__ of flash player. Not only the list of bugs is gruesome. Also the performance..

One thing which makes me raging is the super "secure" feature to not allow switching the focus of a Fullscreen flash window to another window, which is a typical thing everybody wants in a multi monitor setup.

Luckily, there is a nasty hack to overcome this problem!

Just open the `libflashplayer.so` with a hexeditor of your choice and search for the X11 atom `_NET_ACTIVE_WINDOW` and overwrite some chars.

Voila, it works like a charm!

__But__ this solution works until flashplayer is updated again (and due to this massive amount of bugs this happens quite frequently)


