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
easily attach and open large quantities of arbitrary documents, 
such as PDF files or files produced by concept mapping software.

All my demands are fulfilled by the two following sets of tools:

* [Emacs org-mode](https://orgmode.org/) + [org-wiki](https://github.com/caiorss/org-wiki)
* [Firefox](http://firefox.com) + [Scrapyard bookmark manager](https://addons.mozilla.org/en-US/firefox/addon/scrapyard/)

[org-wiki](https://github.com/caiorss/org-wiki) is a [Emacs](http://emacs.org) module which utilizes
[org-mode markup](http://ergoemacs.org/emacs/emacs_org_markup.html)
and [provides](https://caiorss.github.io/org-wiki/) a bunch of commands
for wiki-related manipulations, for example the [commands](https://github.com/caiorss/org-wiki#commands-to-download-files) for downloading and inserting attachment files.
In Windows, an instance of org-wiki may be automatically 
installed and configured as a part of [RHO Emacs](https://rho-emacs.sourceforge.io) distribution.



1. Hierarchical grouping


![](/posts/images/org-wiki-hierarchy.png)
