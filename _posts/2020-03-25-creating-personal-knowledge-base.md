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
advanced note-taking system, such as the one used by [Niklas Luhmann](https://en.wikipedia.org/wiki/Niklas_Luhmann),
which is described in the book [How to Take Smart Notes](https://www.goodreads.com/en/book/show/34507927). Luhmann did his
main work by elaborately wringing notes on the reading material and implemented
a sophisticated system of links and indexes to keep the notes together. 

Additionally, I have one more requirement: there should be an ability to
easily attach and open large quantities of arbitrary documents, 
such as PDF files or files produced by concept mapping software.

All my demands are fulfilled by the two following sets of tools:

* [Emacs org-mode](https://orgmode.org/) + [org-wiki](https://github.com/caiorss/org-wiki)
* [Firefox](http://firefox.com) + [Scrapyard bookmark manager](https://addons.mozilla.org/en-US/firefox/addon/scrapyard/)

[org-wiki](https://github.com/caiorss/org-wiki) is an [Emacs](http://emacs.org) module that utilizes
[org-mode markup](http://ergoemacs.org/emacs/emacs_org_markup.html)
and [provides](https://caiorss.github.io/org-wiki/) a bunch of commands
for wiki-related manipulations, for example, the [commands](https://github.com/caiorss/org-wiki#commands-to-download-files) to download and insert attachment files.
In Windows, an instance of org-wiki may be automatically 
installed and configured as a part of [RHO Emacs](https://rho-emacs.sourceforge.io) distribution.
I use it to keep any long-term files and notes that I need less often than, 
for example, any web-resources available online.

[Scrapyard](https://addons.mozilla.org/en-US/firefox/addon/scrapyard/), that
reqires [Firefox](http://firefox.com), is an advanced bookmark manager with 
the abilities of web-clipping, cloud-bookmarking and note-taking which 
resembles the good old [ScrapBook](https://en.wikipedia.org/wiki/ScrapBook).
I use it to store and organize bookmarks to online resources and save
local archives of web-pages and PDF documents.

Let's consider the knowledge-base related features of this software in more
detail.


1. Hierarchical grouping

org-wiki uses files at the top level of its organizational structure. 
Links to the topic files are placed in a single index file. The topic files
are divided into headings and subheadings of various levels. If necessary, any
file could be quiclky accessed through the Emacs [SpeedBar](https://www.emacswiki.org/emacs/SpeedBar) module.

![org-wiki hierarchy](/posts/images/org-wiki-hierarchy.png)

Scrapyard uses shelves at its top level. Bookmarks and page archives could
be grouped in an hierarchical set of folders.

![Scrapyard hierarchy](/posts/images/scrapyard-hierarchy.png)


2. Hyperlinking and cross-referencing

org-mode provides a wide [variety](https://orgmode.org/guide/Hyperlinks.html)
of hyperlinking to files and headings in them (usually you need an unique ID in 
the property drawer of a heading to be able to refer to it from a link).
