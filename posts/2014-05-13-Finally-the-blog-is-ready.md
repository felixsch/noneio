---
title: The blog is finally ready
description: The first blog entry
tags: misc
---

**I proudly present my new blog. I thought about two years to start a new blog. My first try
,written in Django, failed after few month, because of lack of posts and general interest from my side.
Now I decided to give me another chance.**

Goal of the project is, to write down my thoughts about programing, politics and all sort of things where I\'m interested in (cooking for example). I also trying to encourage myself to write posts in English to hopefully improve my English skills (yes I need to write/talk a lot more in English).

I decided to put the dynamic generated content approach aside and try something more simple. Now the blog is written in Haskell using the static web processor [Hakyll](http://jaspervdj.be/hakyll/) to generate all pages. Hakyll make use of the wonderful pandoc package which converts almost any text format to another. I decided to stick with markdown, because it seems the De facto standard in easy text markup languages.
Because of that all content is generated statically I decided to add disqus to the blog posts to give you the chance to tell me your thoughts. I also installed [Piwik](http://piwik.org). Piwik is a open source web traffic analyser like Google-Analytics but without giving all data to Google. Because I\'m really curious to see how much traffic the Blog generates.

The Blog and Piwik is hosted by uberspace.de which seems to be the right choice in hosting low traffic projects. Uberspace offers a shell with unlimited email and mysql accounts/databases. The documentation is great for people which want to try hosting their services on there own but want some background informations (assuming you understand German). Indeed a nice feature is, that you can decide you much you want to pay for the service. Overall uberspace is a really nice looking project, with really cool intention (make hosting available for all people).


By the way: The source of this blog is released under CC Licence and can be found at [github.com/felixsch/noneio](http://github.com/felixsch/noneio).





