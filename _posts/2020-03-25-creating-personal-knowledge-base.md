---
layout: post
title: Creating a personal knowledge base
categories: [Emacs, Firefox, org-mode, org-wiki, Scrapyard, elisp]
---

Despite that there are gigabytes of writings on this topic, ranging
from the recipes for managing a bunch of markdown files on GitHub to
the manuals for setting up a local MediaWiki server, I will describe 
the solution that works for me.

Any note-taking tool with the following four features is suitable 
for establishing a pretty usable knowledge base:

* Hierarchical data organization
* Hyperlinking and cross-referencing
* Tagging
* Search

For a worthy knowledge base, it may be vital to implement a
note-taking system, such as Zettelkasten invented by [Niklas Luhmann](https://en.wikipedia.org/wiki/Niklas_Luhmann).
As described in the book [How to Take Smart Notes](https://www.goodreads.com/en/book/show/34507927), Luhmann did his
primary work by elaborately wringing notes on the reading material. He created
a sophisticated system of links and indexes to keep the notes together. Through links, his indexes branched into
multiple contexts. This helped him to understand the researched topic in many ways.

[concept- or mind-mapping](https://en.wikipedia.org/wiki/Concept_map) tool would also be a good addition.
This means that our software should have the ability to easily attach and open large quantities of documents, 
such as PDF files or files produced by concept mapping tools.

Most of my demands are fulfilled by the two following software packages:

* [Emacs org-mode](https://orgmode.org/) + [org-wiki](https://github.com/caiorss/org-wiki)
* [Firefox](http://firefox.com) + [Scrapyard bookmark manager](https://addons.mozilla.org/en-US/firefox/addon/scrapyard/)

[org-wiki](https://github.com/caiorss/org-wiki) is an [Emacs](http://emacs.org) module based on
[org-mode markup](http://ergoemacs.org/emacs/emacs_org_markup.html)
that [provides](https://caiorss.github.io/org-wiki/) a bunch of commands
for wiki-related manipulations, for example, the [commands](https://github.com/caiorss/org-wiki#commands-to-download-files) to download and insert attachment files,
or commands to quickly insert links.
In Windows, an instance of org-wiki may be automatically 
installed and configured as a part of [RHO Emacs](https://gchristensen.github.io/rho-emacs/) distribution.
I use it to store any files that I need less often and to keep free-form notes, for which Emacs is indispensable.

[Scrapyard](https://addons.mozilla.org/en-US/firefox/addon/scrapyard/), is an advanced bookmark manager that 
is able to clip fragments of pages, it has cloud bookmarking, and also could be used for note-taking. Much like
the good old [ScrapBook](https://en.wikipedia.org/wiki/ScrapBook).
I use Scrapyard to store and organize bookmarks to online resources and keep a
local archive of web pages and PDF documents.

Let's consider the features of this software in more detail.

**1. Hierarchical grouping**

Carefully maintained hierarchical organization allows to effortlessly find any necessary content.

org-wiki utilizes files at the top level of its organizational structure. 
There are topic files and index files. Index files contain links to the topic files. The topic files
are divided into headings and subheadings of various levels. Emacs [SpeedBar](https://www.emacswiki.org/emacs/SpeedBar)
allows to quickly access any file, if necessary.

![org-wiki hierarchy](/posts/images/org-wiki-hierarchy.png)

There are also non-hierarchical approaches to note-taking. The [org-roam](https://orgroam.com) Emacs package, which is 
also available in [RHO Emacs](https://gchristensen.github.io/rho-emacs/), is a popular example.

Scrapyard utilizes shelves at its top level. Bookmarks and page fragments could
be grouped into hierarchical trees of folders.

![Scrapyard hierarchy](/posts/images/scrapyard-hierarchy.png)

**2. Hyperlinking and cross-referencing**

org-mode provides a wide [variety](https://orgmode.org/guide/Hyperlinks.html)
of hyperlinking possibilities to files and headings in them.
Usually, you need a unique ID in the property drawer of a heading 
to be able to refer to it from a link.

Scrapyard allows referring any bookmark or page archive from the text of notes through
a link with `ext+scrapyard://` protocol. Such links require an UUID of the
referred item (the addon help provides more details on linking).

**3. Tagging**

org-mode allows adding [tags](https://orgmode.org/manual/Tags.html) at the end 
of a headline, separated by colons, and [searching](https://orgmode.org/manual/Tag-Searches.html#Tag-Searches) by them.

Scrapyard also allows to specify tags inside bookmark properties and 
provides a special tag search mode.

**4. Text Search** 

Both Scrapyard and Emacs provide text search. It is a built-in feature in Scrapyard. In the org-mode it may be necessary 
to use grep in all its possible Emacs flavors.

### Hyperlinking from org-wiki to Scrapyard and vice versa

It is possible to create links from org-wiki to archives and documents in Scrapyard and vice versa.

#### Referencing from org-wiki to Scrapyard

At first, it is necessary to register `ext+scrapyard` protocol in Windows registry
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

RHO Emacs adds this registry entry automatically.

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

The link includes a Scrapyard UUID of the referred item, which is available from the extended item properties.

#### Referencing from Scrapyard to org-wiki

Since we assume that org-protocol is already configured in your system by RHO Emacs, 
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

Now you can use the following links from Scrapyard notes:

```
[[org-protocol://open-reference?link=file:path/to/file.org::#my_headline][link text]]
```
