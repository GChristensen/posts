---
layout: post
title: Creating a personal knowledge base
categories: [Emacs, Firefox, org-mode, org-wiki, Scrapyard, elisp]
---

Despite that there are gigabytes of writings on this topic, ranging
from the recipes of managing a bunch of markdown files on GitHub to
the manuals for setting up a local MediaWiki server, I will describe 
the solution that works for me.

Any note-taking tool with the following four features is suitable 
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
currently works only in [Firefox](http://firefox.com), is an advanced bookmark manager with 
the abilities of web-clipping, cloud-bookmarking, and note-taking which 
resembles the good old [ScrapBook](https://en.wikipedia.org/wiki/ScrapBook).
I use it to store and organize bookmarks to online resources and save
local archives of web-pages and PDF documents.

Let's consider the knowledge-keeping features of this software in more
detail.


**1. Hierarchical grouping**

org-wiki uses files at the top level of its organizational structure. 
Links to the topic files are placed in a wiki index file. The topic files
are divided into headings and subheadings of various levels. If necessary, any
file could be quickly accessed through the Emacs [SpeedBar](https://www.emacswiki.org/emacs/SpeedBar) module.

![org-wiki hierarchy](/posts/images/org-wiki-hierarchy.png)

Scrapyard uses shelves at its top level. Bookmarks and page archives could
be grouped in a hierarchical tree of folders.

![Scrapyard hierarchy](/posts/images/scrapyard-hierarchy.png)


**2. Hyperlinking and cross-referencing**

org-mode provides a wide [variety](https://orgmode.org/guide/Hyperlinks.html)
of hyperlinking possibilities to files and headings in them.
Usually, you need a unique ID in the property drawer of a heading 
to be able to refer to it from a link.

Scrapyard allows referring any bookmark or page archive from notes through
a link with `ext+scrapyard://` protocol which requires a UUID of a 
referred item (see the addon help for more details).

**3. Tagging**

org-mode allows adding [tags](https://orgmode.org/manual/Tags.html) at the end 
of a headline, separated by colons and [search](https://orgmode.org/manual/Tag-Searches.html#Tag-Searches) by them.

Scrapyard also allows to specify tags inside bookmark properties and 
provides a special tag search mode.

**4. Search** 

Both Scrapyard and Emacs provide search as a built-in feature in the former
case and, for example, through the `lgrep` command in the recent.

### Hyperlinking from org-wiki to Scrapyard and vice versa

If necessary, it is possible to create links from org-wiki to archives and
documents in Scrapyard and vice versa. I assume that you are using [RHO Emacs](https://rho-emacs.sourceforge.io) distribution in Windows with the preinstalled org-protocol (you need to configure org-protocol manually
otherwise).

#### Referencing from org-wiki to Scrapyard

At first you need to register `ext+scrapyard` protocol in Windows registry
using a .reg file with the following content: 

```
REGEDIT4

[HKEY_CLASSES_ROOT\ext+scrapyard]
@="URL:Scrapyard Protocol"
"URL Protocol"=""
[HKEY_CLASSES_ROOT\ext+scrapyard\shell]
[HKEY_CLASSES_ROOT\ext+scrapyard\shell\open]
[HKEY_CLASSES_ROOT\ext+scrapyard\shell\open\command]
@="\"C:\\Program Files\\Mozilla Firefox\\firefox.exe\" \"%1\""
```

Then add the following code to your `.emacs` configuration file:

```clojure
(defcustom scrapyard-url-protocol "scrapyard"
  "Scrapyard protocol"
  :group 'scrapyard
  :type 'string)

(defun scrapyard-follow (uuid)
  "Open Scrapyard URL"
  (browse-url (concat "ext+scrapyard://" uuid)))

(org-link-set-parameters scrapyard-url-protocol :follow #'scrapyard-follow)
```

Now you may use the following links in your org markup:

```
[[scrapyard:B79C8A274D0B4378835976C2B2554ACD][link text]]
```

The link includes a Scrapyard UUID of the referred item which is available from the extended item properties.

#### Referencing from Scrapyard to org-wiki

Since we assume that org-protocol is already configured in your system, 
just add the following code to your `.emacs` configuration file:

```clojure
(defun org-protocol-open-reference (args)
  (org-open-link-from-string  (plist-get args :link)))

(add-to-list 'org-protocol-protocol-alist
             '("open-reference"
               :protocol "open-reference"
               :function org-protocol-open-reference
               :kill-client t))
```

Add a unique `CUSTOM_ID` property to the headline being referred, for example, 
with the value `my_headline`.

Now you can use the following links from Scrapyard notes in the Org format:

```
[[org-protocol://open-reference?link=file:path/to/file.org::#my_headline][link text]]
```
