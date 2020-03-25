---
layout: post
title: Creating a personal knowledge base
categories: [Emacs, Firefox, org-mode, org-wiki, Scrapyard, elisp]
---

Despite that there are gigabytes of writings on this topic, ranging
from the recipes of managing a bunch of markdown files on GitHub to
the manuals for setting up a local MediaWiki server, I'll describe 
a solution that works for me.

Basically, any software tool with the following four features is suitable 
for a usable knowledge base:

* Hierarchical data organization
* Hyperlinking and cross-referencing
* Tagging
* Search

Occasionally you may also need a [concept mapping](https://en.wikipedia.org/wiki/Concept_map) tool, and utilize an
advanced note taking system, such as the one used by [Niklas Luhmann](https://en.wikipedia.org/wiki/Niklas_Luhmann),
which is described in the book [How to Take Smart Notes](https://www.goodreads.com/en/book/show/34507927). Luhmann did his
main work by elaborately wringing notes on the read material and implemented
a sophisticated system of links and indexes to keep the notes together. 

Additionaly, I have one more requirement: there should be an ability to
easily attach and open arbitrary documents, such as PDF files or files 
produced by concept mapping software.

All my demands are fulfilled by the two following sets of programs:

* [Emacs org-mode](https://orgmode.org/) + [org-wiki](https://github.com/caiorss/org-wiki)
* [Firefox](http://firefox.com) + [Scrapyard bookmark manager](https://addons.mozilla.org/en-US/firefox/addon/scrapyard/)

