---
layout: post
title: Create dynamic capture templates to use with org-protocol. In Windows!
categories: [UbiquityWE, Emacs, elisp, JavaScript]
---


[org-protocol](https://orgmode.org/manual/Protocols.html) offers a nice
possibility to capture URLs along with some selected text from many web-browsers
into Emacs [org-mode](https://orgmode.org/). But most of the tools you can find
out there allow a little control over the capture process - usually it is
possible to put only plain text into a hardcoded org file. Below we develop an
[UbiquityWE](https://gchristensen.github.io/ubiquitywe/) command which allows to
capture org-formatted text under any
[headline](https://orgmode.org/manual/Headlines.html) in one of the
user-specified org files. If you are interested only in Ubiquity or only in Org
(or only in Windows), you may still skim through the code to find out how things
work.


Work in progress...

### Creating Ubiquity command

In the following command we refer to two fictional org files: `~/org/foo.org` and
`~/org/bar.org` (relative to user home directory) available through `foo` and `bar`
shortcuts from Ubiquity. There are also three fictional headlines: "Items", "Things" 
and "Widgets" predefined for Ubiquity autocompletion. The noun-type `noun_open_headlines`
also allows user to enter an arbitrary headline name. Although, in theory it is 
[possible](http://kitchingroup.cheme.cmu.edu/blog/2017/01/03/Find-stuff-in-org-mode-anywhere/)
to automatically maintain an index of all org-files and headlines and 
[obtain](https://github.com/eschulte/emacs-web-server)
it in Ubiquity, this is a work for real aficionados.

By default the command captures selection as plain text, but it is possible to
capture HTML-selection as org-formatted text, if the corresponding parameter
is specified in the command arguments.

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

CmdUtils.CreateCommand({
    name: "org-capture",
    uuid: "F36F51E1-60B3-4451-B08E-6A4372DA74DD",
    arguments: [
        {role: "object", nountype: noun_arb_text, label: "text"},
        {role: "time",   nountype: ORG_FILES, label: "file"}, // at
        {role: "format", nountype: noun_open_headlines, label: "headline"}, // in
        {role: "alias",  nountype: ORG_FORMATS, label: "type"}, // as
    ],
    description: "Captures the current tab URL or selected text to an org-file.",
    help: `<span class="syntax">Syntax</span>
            <ul class="syntax"><li><b>org-capture</b> [<i>title</i> | <b>this</b>] 
                    [<b>at</b> <i>file</i>] [<b>in</b> <i>headline</i>] 
                    [<b>as</b> <i>format</i>]</li>
            </ul>
            <span class="arguments">Arguments</span><br>
            <ul class="syntax">
                <li>- <i>title</i> - captured URL title.</li>
            </ul>
            <ul class="syntax">
                <li>- <i>org file</i> - org-file to place the capture in.</li>
            </ul>
            <ul class="syntax">
                <li>- <i>headline</i> - headline to palce the capture under.</li>
            </ul>
            <ul class="syntax">
                <li>- <i>format</i> - {<b>text</b> | <b>org</b>}.</li>
            </ul>`,
    // preview the capture options
    preview: function(pblock, {object, time, format, alias}) {
        let html = "";
        let tab = CmdUtils.getActiveTab();
        
        if (!tab) {
            pblock.innerHTML = "The current tab is unsuitable for capture.";
            return;
        }

        this._description = object && object.text
            ? object.text === CmdUtils.getSelection()? tab.title: object.text
            : tab.title;

        if (tab.title)
            html += "Description: <span style='color: #45BCFF;'>" 
                 + Utils.escapeHtml(this._description) + "</span><br>";

        if (time && time.data)
            html += "File: <span style='color: #FD7221;'>" 
                 + Utils.escapeHtml(time.data) + "</span><br>";

        if (format && format.text && format.text !== CmdUtils.getSelection())
            html += "Headline: <span style='color: #7DE22E;'>" 
                 + Utils.escapeHtml(format.text) + "</span><br>";

        if (alias && alias.text)
            html += "Format: <span style='color: #FC6DAC;'>" 
                 + alias.text + "</span><br>";
                
        pblock.innerHTML = html;
    },
    execute: function({object, time, format, alias}) {
        let tab = CmdUtils.getActiveTab();
        
        if (!tab)
            return;
            
        // URL-safe Base 64 encoding
        b64enc = s => btoa(unescape(encodeURIComponent(s)))
                        .replace(/=+$/g, "")
                        .replace(/\+/g, "-")
                        .replace(/\//g, "_");
        
        // get and pack capture options to an org-protocol URL
        let title = b64enc(this._description);
        let url = b64enc(tab.url);
        let file = time && time.data
                    ? b64enc(time.data)
                    : b64enc(Object.values(ORG_FILES)[0]);
        let headline = format && format.text 
                && format.text !== CmdUtils.getSelection()
                    ? b64enc(format.text)
                    : "";
        let type = alias && alias.text? alias.text: "text";
        let subprotocol = type === "text"? "capture-ubiquity": "capture-html";
        let body = b64enc(type === "text"
                            ? CmdUtils.getSelection()
                            : CmdUtils.getHtmlSelection());

        let orgUrl = `org-protocol://${subprotocol}?template=P`
                   + `&title=${title}&url=${url}&body=${body}`
                   + `&file=${file}&headline=${headline}&format=${type}`;
        
        if (orgUrl.length > 32500) {
            CmdUtils.notify("Selection is too long.")
            return;
        }
        
        // launch Emacs client
        // it is assumed that Emacs is running when you are trying to capture
        location.href = orgUrl;
    }
});
```

### org-protocol in Windows

In Windows org-protocol URL is altered in various ways on its path from a browser to
Emacs: 

- A slash is appended to subprotocol name. For an example: `org-protocol://capture?url=...`
becomes `org-protocol://capture/?url=...`. Because of this Emacs may not recognize a
subprotocol.
- The URL is encoded into local system character set, so Emacs will get unibyte characters
instead of UTF-8.

In addition, the URL should be no longer than 32kb. All this makes setup of org-protocol
in Windows a non-trivial task, details of which lie beyond the scope of this post.
We assume that the automatic configuration provided by [&rho;Emacs](https://rho-emacs.sourceforge.io/)
is used.

### Configuring Emacs

To pass captured items to Emacs we use two custom org-protocol:// subprotocol names:
- `capture-ubiquity` - custom supbrotocol name used to pass plain UTF-8 text.
- `capture-html` - custom subprotocol name used to process HTML which is defined by
 [org-protocol-capture-html](https://github.com/alphapapa/org-protocol-capture-html)
library (it is already included in &rho;Emacs, but requires [pandoc](https://pandoc.org/)
somewhere on the PATH).

The most of URL parameters obtained from Ubiquity are Base64-encoded to preserve UTF-8, 
although UTF-8 will be lost in the case of HTML processing, since the text is need to
be passed to `pandoc` command line utility and hence encoded into local coding system.

Org capture template used to store links and text is completely dynamic and composed
out of the org-protocol link parameters.

Paste the following code into your `.emacs` configuration file:

```emacs
;; get the destination org file path specified in Ubiquity
(defun capture-get-destination-file ()
  ;; `capture-decoded-org-protocol-query' global variable contains
  ;; org-protocol url query parameters, stored earlier (see below)
  (plist-get capture-decoded-org-protocol-query :file))

;; get the destination org headline specified in Ubiquity (or insert one
;; if absent) and positon point near it in the buffer
(defun capture-get-destination-headline ()
  (let ((headline (plist-get capture-decoded-org-protocol-query :headline)))
    (if (and headline (not (string= headline "")))
        (progn
          (goto-char (point-min))
          (if (re-search-forward (format org-complex-heading-regexp-format
                                         (regexp-quote headline))
                                 nil t)
              (beginning-of-line)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "* " headline "\n")
            (beginning-of-line 0)))
      (goto-char (point-max)))))

;; create dynamic template - append newline and selection text if it presents
;; in org-protocol url or leave only captured link if none
(defun capture-get-org-capture-template-body ()
  (let ((content (plist-get capture-decoded-org-protocol-query :body)))
    (let ((orglink "* %?[[%(capture-decode-local-string :link)]\
[%(capture-decode-local-string :description)]] %U\n"))
      (let ((finalizer "%(capture-finalize-capture)")
            (template (concat orglink
                              (if (and content (not (string= content "")))
                                  "%(capture-decode-local-string :initial)\n"
                                ""))))
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

;; add `capture-ubiquity' org-protocol subprotocol
(add-to-list 'org-protocol-protocol-alist
             '("capture-ubiquity"
               :protocol "capture-ubiquity"
               :function capture-org-protocol-ubiquity
               :kill-client t))

;; decode Base 64-encoded arguments before passing them into
;; org-protocol-capture-html--with-pandoc
(defun advice-org-protocol-capture-html (orig-fun &rest args)
       (apply orig-fun (list (capture-decode-base64-args (car args)))))

(advice-add 'org-protocol-capture-html--with-pandoc
            :around #'advice-org-protocol-capture-html)

;; helper functions

;; decode an unibyte string stored in system locale
(defun capture-decode-local-string (key)
    (let ((format (plist-get capture-decoded-org-protocol-query :format)))
      (if nil (and format (string= format "org"))
          ;; decode a string obtained from pandoc output
          (decode-coding-string
           (plist-get org-store-link-plist key)
           locale-coding-system)
        ;; or return as is 
        (plist-get org-store-link-plist key))))

;; transform URL-safe Base 64 to the regular Base 64
(defun base64url-to-base64 (str)
  (setq str (replace-regexp-in-string "-" "+" str))
  (setq str (replace-regexp-in-string "_" "/" str))
  (let ((mod (% (length str) 4)))
    (cond 
     ((= mod 1) (concat str "==="))
     ((= mod 2) (concat str "=="))
     ((= mod 3) (concat str "="))
     (t str))))

;; decode string in URL-safe Base 64
(defun base64url-decode-string (str)
  (base64-decode-string (base64url-to-base64 (string-trim str))))

;; decode utf-8-encoded string contained in URL-safe Base 64
(defun capture-decode-base64-utf-8 (str)
  (decode-coding-string (base64url-decode-string str) 'utf-8))

;; decode Base 64-encoded parameters obtained from Ubiquity 
(defun capture-decode-base64-args (args)
  (let ((format (plist-get args :format)))
    ;; save parameters for later use
    (setf capture-decoded-org-protocol-query
      `(:format ,format
        :template ,(plist-get args :template)
        :title ,(capture-decode-base64-utf-8 (plist-get args :title))
        :url ,(capture-decode-base64-utf-8 (plist-get args :url))
        :headline ,(capture-decode-base64-utf-8 (plist-get args :headline))
        :file ,(capture-decode-base64-utf-8 (plist-get args :file))
        :body ,(let ((str (capture-decode-base64-utf-8 (plist-get args :body))))
                     (if (and format (string= format "org"))
                         ;; encode string to pass to pandoc
                         (encode-coding-string str locale-coding-system)
                       str))))
    capture-decoded-org-protocol-query))
```