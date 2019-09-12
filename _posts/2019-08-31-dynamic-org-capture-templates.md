---
layout: post
title: Create dynamic capture templates to use with org-protocol. In Windows!
categories: [UbiquityWE, Emacs, elisp, JavaScript]
---


[org-protocol](https://orgmode.org/manual/Protocols.html) offers a nice
possibility to capture URLs along with some selected text from many web-browsers
into Emacs [org-mode](https://orgmode.org/). But most of the tools you can find
out there allow a little control over the capture process - usually it is
only possible to put plain text into some hardcoded org file. Below we develop an
[UbiquityWE](https://gchristensen.github.io/ubiquitywe/) command which allows to
capture org-formatted text under any
[headline](https://orgmode.org/manual/Headlines.html) in one of the
user-specified org files. If you are interested only in Ubiquity or only in Org
(or only in Windows), you may still skim through the code to find out how things
work.

<video src="/posts/videos/ubiquity-demo.webm" width="100%" type="video/webm" controls></video>

### Creating Ubiquity command

In the following command we refer to two fictional org files: `~/org/foo.org` and
`~/org/bar.org` (relative to the user home directory) available through `foo` and `bar`
shortcuts from Ubiquity. There are also three fictional headlines: "Items", "Things" 
and "Widgets" predefined for Ubiquity autocompletion. The noun-type `noun_open_headlines`
also allows to enter an arbitrary headline name. Although, in theory it is 
[possible](http://kitchingroup.cheme.cmu.edu/blog/2017/01/03/Find-stuff-in-org-mode-anywhere/)
to automatically maintain an index of all org-files and headlines and 
[obtain](https://github.com/eschulte/emacs-web-server)
it in Ubiquity, this is a work for real aficionados.

We need `getArgumentText` helper function to go around two Ubiquity parser quirks:
- It substitutes empty arbitrary-text argument values for selection.
- It requires to specify special `this` keyword if there is a selection and argument 
  values contain spaces.
 
Thanks to `getArgumentText` it is possible to capture text or link (when there is
no selection) under a custom headline with spaces in its name using `this` keyword.
The process is shown at the video above.

By default the command captures selection as plain text, but it could be
captured as org-formatted text, if the corresponding parameter is specified in
the command arguments.

Paste the following code into UbiquityWE command editor: 

```javascript
// list of target org files
let ORG_FILES = {"foo": "~/org/foo.org", 
                 "bar": "~/org/bar.org"
                };

// list of predefined org headlines to place captures under
let ORG_HEADLINES = ["Items", "Things", "Widgets"];

// capture formats
let ORG_FORMATS = ["text", "org"];

// TODO states
let TODO_STATES = ["TODO", "WAITING", "POSTPONED"];

// a noun type that allows to enter not only suggested but also custom headline names
let noun_open_headlines = {
    label: "headline",
    noExternalCalls: true,
    cacheTime: -1,
    suggest: function (text, html, cb, selectionIndices) {
        if (text === CmdUtils.getSelection()) // mute stray selection
            return {};
            
        let matcher = new RegExp(text, "i");
        // make sugestions from predefined headlines
        let suggs = ORG_HEADLINES.map(h => ({name: h}))
                        .filter(i => (i.match = matcher.exec(i.name), !!i.match))
                        .map(i => CmdUtils.makeSugg(i.name, i.name, null,
                                    CmdUtils.matchScore(i.match), selectionIndices));
        // add a suggestion with the argument text as is
        suggs.push(CmdUtils.makeSugg(text, html, null, 
                        suggs.length? .1: 1, selectionIndices));

        return suggs;
    }
};

// a helper function to safely get argument text
let getArgumentText = arg => 
    arg && arg.text && arg.text !== CmdUtils.getSelection() && arg.text !== "this"
        ? arg.text
        : "";

CmdUtils.CreateCommand({
    name: "org-capture",
    uuid: "F36F51E1-60B3-4451-B08E-6A4372DA74DD",
    arguments: [
        {role: "object", nountype: noun_arb_text, label: "title"},
        {role: "time",   nountype: ORG_FILES, label: "file"}, // at
        {role: "format", nountype: noun_open_headlines, label: "headline"}, // in
        {role: "alias",  nountype: ORG_FORMATS, label: "format"}, // as
        {role: "instrument", nountype: TODO_STATES, label: "todo"}, // with
    ],
    description: "Captures the current tab URL or selected text to an org-file.",
    help: `<span class="syntax">Syntax</span>
            <ul class="syntax"><li><b>org-capture</b> [<i>title</i> | <b>this</b>] 
                    [<b>at</b> <i>file</i>] [<b>in</b> <i>headline</i>] 
                    [<b>with</b> <i>todo</i>] [<b>as</b> <i>format</i>]</li>
            </ul>
            <span class="arguments">Arguments</span><br>
            <ul class="syntax"><li>- <i>title</i> - captured URL title.</li></ul>
            <ul class="syntax">
                <li>- <i>org file</i> - org-file to place the capture in.</li>
            </ul><ul class="syntax">
                <li>- <i>headline</i> - headline to palce the capture under.</li>
            </ul><ul class="syntax">
                <li>- <i>todo</i> - todo state: {<b>TODO</b> | <b>WAITING</b> |
                <b>POSTPONED</b>}.</li>
            </ul><ul class="syntax">
                <li>- <i>format</i> - {<b>text</b> | <b>org</b>}.</li>
            </ul>`,
    // preview the capture options
    preview: function(pblock, {object, time, format, alias, instrument}) {
        let html = "";
        let tab = CmdUtils.getActiveTab();
        
        if (!tab) {
            pblock.innerHTML = "The current tab is unsuitable for capture.";
            return;
        }

        let text = getArgumentText(object)? object.text: tab.title;

        html += "Title: <span style='color: #45BCFF;'>" 
             + Utils.escapeHtml(text) + "</span><br>";

        if (time && time.data)
            html += "File: <span style='color: #FD7221;'>" 
                 + Utils.escapeHtml(time.data) + "</span><br>";

        if (text = getArgumentText(format)) // beware of assignments in condition
            html += "Headline: <span style='color: #7DE22E;'>" 
                 + Utils.escapeHtml(text) + "</span><br>";

        if (text = getArgumentText(instrument))
            html += "TODO state: <span style='color: #FC6DAC;'>" 
                 + text + "</span><br>";
                 
        if (text = getArgumentText(alias))
            html += "Format: <span style='color: white;'>" 
                 + text + "</span><br>";

                
        pblock.innerHTML = html;
    },
    execute: function({object, time, format, alias, instrument}) {
        let tab = CmdUtils.getActiveTab();
        
        if (!tab)
            return;
            
        // URL-safe Base 64 encoding
        let b64enc = s => btoa(unescape(encodeURIComponent(s)))
                        .replace(/=+$/g, "")
                        .replace(/\+/g, "-")
                        .replace(/\//g, "_");
        
        // get and pack capture options to an org-protocol URL
        let title = b64enc(getArgumentText(object)? object.text: tab.title);
        let url = b64enc(tab.url);
        let file = time && time.data
                    ? b64enc(time.data)
                    : b64enc(Object.values(ORG_FILES)[0]);
        let headline = b64enc(getArgumentText(format));
        let type = getArgumentText(alias)? alias.text: "text";
        let todo = getArgumentText(instrument);
        let body = b64enc(type === "text"
                            ? CmdUtils.getSelection()
                            : CmdUtils.getHtmlSelection());

        let subprotocol = type === "text"? "capture-ubiquity": "capture-html";
        let orgUrl = `org-protocol://${subprotocol}?template=P`
                   + `&title=${title}&url=${url}&body=${body}&todo=${todo}`
                   + `&file=${file}&headline=${headline}&format=${type}`;
        
        if (orgUrl.length > 32500) {
            CmdUtils.notify("Selection is too long.");
            return;
        }
        
        // launch emacsclient
        // it is assumed that Emacs is running when you are trying to capture
        location.href = orgUrl;
    }
});
```

### org-protocol in Windows

org-protocol URL is altered in various ways on its path to Emacs when capturing
from a browser in Windows:

- A slash is appended to subprotocol name. For an example:<br> `org-protocol://capture?url=...`
becomes `org-protocol://capture/?url=...`<br> Because of this Emacs may not recognize a
subprotocol.
- The URL is encoded into local system character set, so Emacs will get unibyte characters
instead of UTF-8.

In addition, the URL should be no longer than 32kb. All this makes setup of org-protocol
in Windows a non-trivial task.

To address the first problem you may advice `org-protocol-check-filename-for-protocol` function,
for example, in the following way:

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

The second problem requires recoding of the obtained URL components inside capture templates:

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
 
But you may just install [&rho;Emacs](https://rho-emacs.sourceforge.io/) which does all this automatically.

### Configuring Emacs

To pass captured items to Emacs we use two custom org-protocol:// subprotocol names:
- `capture-ubiquity` - custom supbrotocol name used to pass plain UTF-8 text.
- `capture-html` - custom subprotocol name used to process HTML (it is defined by
 [org-protocol-capture-html](https://github.com/alphapapa/org-protocol-capture-html)
library which is included in &rho;Emacs when it is installed with `org-protocol` option).  

NOTE: you need to manually install `org-protocol-capture-html` if you are using 
a regular emacs distribution. It also requires [pandoc](https://pandoc.org/)
binary somewhere on the PATH (pandoc is `not` included in &rho;Emacs). 

Org [capture template](https://orgmode.org/manual/Capture-templates.html) 
used to store links and text is completely dynamic and is composed
out of the org-protocol link parameters. 
The most of URL parameters obtained from Ubiquity are Base64-encoded to preserve UTF-8.
File path, headline name and text format are passed through three non-standard parameters:
`file`, `headline` and `format`.

Paste the following code into your `.emacs` configuration file:

```clojure
;; get the destination org file path specified in Ubiquity
(defun capture-get-destination-file ()
  ;; `capture-decoded-org-protocol-query' global variable contains
  ;; org-protocol url query parameters, stored earlier (see below)
  (plist-get capture-decoded-org-protocol-query :file))

;; find the destination org headline specified in Ubiquity (or insert one
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
(defun capture-org-protocol-ubiquity (args)
  (org-protocol-capture
   (capture-decode-base64-args args)))

;; add `capture-ubiquity' org-protocol subprotocol handler
(add-to-list 'org-protocol-protocol-alist
             '("capture-ubiquity"
               :protocol "capture-ubiquity"
               :function capture-org-protocol-ubiquity
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

;; decode Base64-encoded parameters obtained from Ubiquity 
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