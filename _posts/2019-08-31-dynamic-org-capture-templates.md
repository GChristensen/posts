---
layout: post
title: Create dynamic capture templates to use with org-protocol. In Windows!
categories: [iShell Extension, Emacs, elisp, JavaScript]
---


[org-protocol](https://orgmode.org/manual/Protocols.html) offers a nice
possibility to capture URLs along with some selected text from many web-browsers
into Emacs [org-mode](https://orgmode.org/). But most of the tools you can find
out there allow a little control over the process - usually, it is
only possible to put just plain text into some hard-coded org file. Below we develop an
[iShell Extension](https://gchristensen.github.io/ishell/) command which allows to
capture org-formatted text under any
[headline](https://orgmode.org/manual/Headlines.html) in one of the several 
user-specified org files. If you are interested only in iShell or only in Org
(or only in Windows), you may still skim through the code to find out how things
work.

<video src="/posts/videos/ishell-demo.webm" width="100%" type="video/webm" controls></video>

### Creating an iShell command

In the following command we refer to two fictional org files: `~/org/foo.org` and
`~/org/bar.org` (relative to the user home directory) available through the `foo` and `bar`
autocompletion shortcuts from iShell. There are also three fictional org headlines: "Items", "Things" 
and "Widgets" available to autocompletion. The noun-type `noun_org_headline`
also allows to enter an arbitrary headline name. Although, in theory, it is 
[possible](http://kitchingroup.cheme.cmu.edu/blog/2017/01/03/Find-stuff-in-org-mode-anywhere/)
to automatically maintain an index of all org-files and headlines and 
[obtain](https://github.com/eschulte/emacs-web-server)
it in iShell, this is a work for real aficionados.

By default, the command extracts selection as plain text, but it could be
marked for processing to org-formatted text, if the value of `format` parameter is specified in
command arguments.

Paste the following code into iShell command editor: 

```javascript
// the list of target org files
let ORG_FILES = {
    "foo": "~/org/foo.org",
    "bar": "~/org/bar.org"
};

// the list of predefined org headlines to place captures under
let ORG_HEADLINES = ["Items", "Things", "Widgets"];

// capture formats
let ORG_FORMATS = ["text", "org"];

// TODO states
let TODO_STATES = ["TODO", "WAITING", "POSTPONED"];

/** 
    A noun type that allows to generate suggestions taken from the 
    predefined list combined with user input.
 
    @nountype 
 */
function noun_org_headline(text, html, callback, selectionIndices) {
    if (text === cmdAPI.getSelection()) // mute stray selection
        return {};

    let matcher = new RegExp(text, "i");
    // generate predefined suggestions (from ORG_HEADLINES)
    let suggs = ORG_HEADLINES.map(h => ({name: h}))
        .filter(i => (i.match = matcher.exec(i.name), !!i.match))
        .map(i => cmdAPI.makeSugg(i.name, i.name, null,
            cmdAPI.matchScore(i.match), selectionIndices));
    // add a suggestion as it was entered by the user (from the text argument)
    suggs.push(cmdAPI.makeSugg(text, html, null,
        suggs.length? .1: 1, selectionIndices));

    return suggs;
}

// a helper function to safely get argument text
let getArgumentText = arg =>
    arg?.text && arg.text !== cmdAPI.getSelection() && arg.text !== "this"
        ? arg.text
        : "";

/**
    <!-- command syntax help -->

    # Syntax
    **org-capture** [*title* | **this**] [**at** *file*] [**in** *headline*]
    [**with** *todo*] [**as** *format*]
    
    # Arguments
    - _title_ - captured URL title.
    - _org file_ - org-file to place the capture in.
    - _headline_ - headline to palce the capture under.
    - _todo_ - todo state: {**TODO** | **WAITING** | **POSTPONED**}.
    - _format_ - {**text** | **org**}.
    
    @command
    @markdown
    @descripiton Captures the current tab URL or selected text to an org-file.
    @uuid F36F51E1-60B3-4451-B08E-6A4372DA74DD
 */
class OrgCapture {
    constructor(args) {
        args[OBJECT] = {nountype: noun_arb_text, label: "title"};
        args[AT]     = {nountype: ORG_FILES, label: "file"};
        args[IN]     = {nountype: noun_org_headline, label: "headline"};
        args[AS]     = {nountype: ORG_FORMATS, label: "format"};
        args[WITH]   = {nountype: TODO_STATES, label: "todo"};
    }

    preview({OBJECT, AT, IN, AS, WITH}, display) {
        let html = "";
        let tab = cmdAPI.getActiveTab();

        if (!tab) {
            display.set("The current tab is unsuitable for capture.");
            return;
        }

        let text = getArgumentText(OBJECT)? OBJECT.text: tab.title;

        html += "Title: <span style='color: #45BCFF;'>"
            + cmdAPI.escapeHtml(text) + "</span><br>";

        if (AT?.data)
            html += "File: <span style='color: #FD7221;'>"
                + cmdAPI.escapeHtml(AT.data) + "</span><br>";

        if (text = getArgumentText(IN)) // beware of assignments in condition
            html += "Headline: <span style='color: #7DE22E;'>"
                + cmdAPI.escapeHtml(text) + "</span><br>";

        if (text = getArgumentText(WITH))
            html += "TODO state: <span style='color: #FC6DAC;'>"
                + text + "</span><br>";

        if (text = getArgumentText(AS))
            html += "Format: <span style='color: white;'>"
                + text + "</span><br>";

        display.set(html);
    }

    execute({OBJECT, AT, IN, AS, WITH}) {
        let tab = cmdAPI.getActiveTab();

        if (!tab)
            return;

        // URL-safe Base 64 encoding
        let b64enc = s => btoa(unescape(encodeURIComponent(s)))
            .replace(/=+$/g, "")
            .replace(/\+/g, "-")
            .replace(/\//g, "_");

        // get and pack capture options to an org-protocol URL
        let title = b64enc(getArgumentText(OBJECT)? OBJECT.text: tab.title);
        let url = b64enc(tab.url);
        let file = AT?.data
            ? b64enc(AT.data)
            : b64enc(Object.values(ORG_FILES)[0]);
        let headline = b64enc(getArgumentText(IN));
        let type = getArgumentText(AS)? AS.text: "text";
        let todo = getArgumentText(WITH);
        let body = b64enc(type === "text"
            ? cmdAPI.getSelection()
            : cmdAPI.getHtmlSelection());

        let subprotocol = type === "text"? "capture-ishell": "capture-html";
        let orgUrl = `org-protocol://${subprotocol}?template=P`
            + `&title=${title}&url=${url}&body=${body}&todo=${todo}`
            + `&file=${file}&headline=${headline}&format=${type}`;

        if (orgUrl.length > 32500) {
            cmdAPI.notify("Selection is too long.");
            return;
        }

        // launch emacsclient
        // it is assumed that Emacs is running when you are trying to capture
        location.href = orgUrl;
    }
}
```

Because we have several arbitrary-text arguments, we also need `getArgumentText` helper function to go around two iShell parser quirks:
- It passes the current selection, if it presents, as an argument value to the custom [noun-types](https://gchristensen.github.io/ishell/addon/ui/options/tutorial.html#Introduction_to_Noun_Types).
- It requires special `this` [anaphoric pronoun](https://gchristensen.github.io/ishell/addon/ui/options/tutorial.html#Anaphora)
  in place of the first arbitrary-text argument value
  if there is an active selection and values of other arbitrary-text arguments contain spaces. 
  If there is no selection, `this` is considered as the literal value of an argument.

Thanks to `getArgumentText` and `this` parser predefined keyword it is possible to capture the selected text, or a link (when there is
no selection) under any custom headline with spaces in its name.
The process is shown in the video above.


### org-protocol in Windows

org-protocol URL is altered in various ways on its path to Emacs when capturing
from a browser in Windows:

- A slash is appended to the subprotocol name. For an example:<br> `org-protocol://capture?url=...`
becomes `org-protocol://capture/?url=...`<br> Because of this Emacs may not recognize a
subprotocol.
- The URL is encoded into the local unibyte system character set, so Emacs will get single byte characters
instead of UTF-8.

In addition, the URL should be no longer than 32kb. All this makes setup of org-protocol
in Windows a non-trivial task.

To address the problem with URL you may advice `org-protocol-check-filename-for-protocol` function:

```clojure
(defun advice-org-protocol-check-filename (orig-fun &rest args)
  (let ((fname (car args)))
    (let ((correct-url
           (replace-regexp-in-string (regexp-quote "/?")
                                     "?" fname nil 'literal)))
      (apply orig-fun (cons correct-url (cdr args))))))
  
(advice-add 'org-protocol-check-filename-for-protocol
            :around #'advice-org-protocol-check-filename) 
```

The character set problem requires recoding of the obtained URL components:

```clojure
(defun decode-capture-component (c)
  (decode-coding-string (plist-get org-store-link-plist c) 
                         locale-coding-system))

(add-to-list 'org-capture-templates
             '("p" "Protocol" entry (file "")
               "* %?[[%(decode-capture-component :link)]\
[%(decode-capture-component :description)]] %U\n\
%(decode-capture-component :initial)\n")) 
``` 
 
But you may just install [&rho;Emacs](https://gchristensen.github.io/rho-emacs/) which does all this automatically.

### Configuring Emacs

To pass captured items to Emacs we define two custom org-protocol:// subprotocols:
- `capture-ishell` - custom supbrotocol name used to pass plain UTF-8 text.
- `capture-html` - custom subprotocol name used to process HTML (it is actually defined by
 [org-protocol-capture-html](https://github.com/alphapapa/org-protocol-capture-html)
library which is included in &rho;Emacs installed with `org-protocol` option).  

NOTE: you need to manually install `org-protocol-capture-html` if you are using 
a regular emacs distribution. It also requires [pandoc](https://pandoc.org/)
binary somewhere on the PATH (pandoc is included in &rho;Emacs). 

Org [capture template](https://orgmode.org/manual/Capture-templates.html) 
used to store links and text in the code below is completely dynamic and is composed
out of the org-protocol link parameters. 
The most of the URL parameters obtained from iShell are Base64-encoded to preserve UTF-8.

Paste the following code into your `.emacs` configuration file:

```clojure
;; get the destination org file path specified in iShell
(defun capture-get-destination-file ()
  ;; `capture-decoded-org-protocol-query' global variable contains
  ;; org-protocol url query parameters, stored earlier (see below)
  (plist-get capture-decoded-org-protocol-query :file))

;; find the destination org headline specified in iShell (or insert one
;; if absent) and position point near it in the buffer
;; see more at org-mode source code: https://bit.ly/2lJqz1a
(defun capture-get-destination-headline ()
  (let ((headline (plist-get capture-decoded-org-protocol-query :headline)))
    (if (string= headline "")
        (goto-char (point-max))
      (progn
        (goto-char (point-min))
        (if (re-search-forward (format org-complex-heading-regexp-format
                                       (regexp-quote headline))
                               nil t)
            (beginning-of-line)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert "* " headline "\n")
          (beginning-of-line 0))))))

;; create a dynamic template - TODO and body are inserted only if
;; they are present in org-protocol URL parameters 
(defun capture-get-org-capture-template-body ()
  (let ((content (plist-get capture-decoded-org-protocol-query :body))
        (todo (plist-get capture-decoded-org-protocol-query :todo))
        (finalizer "%(capture-finalize-capture)"))
    (let ((orglink (concat "* "
                           (unless (string= todo "")
                             (concat todo " "))
                           "%?[[%:link][%:description]] %U\n")))
      (let ((template (concat orglink
                              (unless (string= content "")
                                  "%:initial\n"))))
        (concat template finalizer)))))

(defun capture-finalize-capture ()
  (setf capture-decoded-org-protocol-query nil) ; free memory
  "")

;; the template is comprised entirely of functions
(add-to-list 'org-capture-templates
               '("P" "Advanced protocol"
                 entry (file+function
                        capture-get-destination-file
                        capture-get-destination-headline)
                 (function capture-get-org-capture-template-body)))

;; envoke standard org-protocol-capture with decoded arguments
(defun capture-org-protocol-ishell (args)
  (org-protocol-capture
   (capture-decode-base64-args args)))

;; define `capture-iShell' org-protocol subprotocol handler
(add-to-list 'org-protocol-protocol-alist
             '("capture-ishell"
               :protocol "capture-ishell"
               :function capture-org-protocol-ishell
               :kill-client t))

;; decode Base64-encoded arguments before passing them into
;; org-protocol-capture-html--with-pandoc
(defun advice-org-protocol-capture-html (orig-fun &rest args)
  (apply orig-fun (list (capture-decode-base64-args (car args)))))

(advice-add 'org-protocol-capture-html--with-pandoc
            :around #'advice-org-protocol-capture-html)

;; helper functions

;; transform URL-safe Base64 to the regular Base64
(defun base64url-to-base64 (str)
  (setq str (replace-regexp-in-string "-" "+" str))
  (setq str (replace-regexp-in-string "_" "/" str))
  (let ((mod (% (length str) 4)))
    (cond 
     ((= mod 1) (concat str "==="))
     ((= mod 2) (concat str "=="))
     ((= mod 3) (concat str "="))
     (t str))))

;; decode string in URL-safe Base64
(defun base64url-decode-string (str)
  (base64-decode-string (base64url-to-base64 (string-trim str))))

;; decode utf-8-encoded string contained in URL-safe Base64
(defun capture-decode-base64-utf-8 (str)
  (decode-coding-string (base64url-decode-string str) 'utf-8))

;; decode Base64-encoded parameters obtained from iShell 
(defun capture-decode-base64-args (args)
  (let ((format (plist-get args :format)))
    ;; save parameters for later use
    (setf capture-decoded-org-protocol-query
      `(:format ,format
        :todo ,(plist-get args :todo)
        :template ,(plist-get args :template)
        :title ,(capture-decode-base64-utf-8 (plist-get args :title))
        :url ,(capture-decode-base64-utf-8 (plist-get args :url))
        :headline ,(capture-decode-base64-utf-8 (plist-get args :headline))
        :file ,(capture-decode-base64-utf-8 (plist-get args :file))
        :body ,(capture-decode-base64-utf-8 (plist-get args :body))))
      capture-decoded-org-protocol-query))
```

Now you have everything set up for the command to work.
