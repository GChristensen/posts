---
layout: post
title: Create dynamic capture templates to use with org-protocol capture. In Windows!
categories: [Enso Launcher, Python]
---


[org-protocol](https://orgmode.org/manual/Protocols.html) offers a nice possibility to capture URLs 
along with some selected text from many web-browsers into Emacs [org-mode](https://orgmode.org/) files. 
But most of the tools you can find out there allow a little control over the capture process - usually 
it is possible to put only plain text into the default capture file. Below we develop an 
[UbiquityWE](https://gchristensen.github.io/ubiquitywe/) command which allows to capture 
org-formatted text under any headline into the specified org file. If you are interested only in Ubiquity
or only in Org (or only in Windows), you may still skim through the code to find out how things work.


Work in progress...

```javascript
{

// list of target org files
let ORG_FILES = {"foo": "~/org/foo.org", "bar": "~/org/bar.org"};

// list of predefined org headlines to place captures under
let ORG_HEADLINES = ["Items", "Things", "Widgets"];

// capture formats
let ORG_FORMATS = ["text", "org"];

// a noun type that allows to enter not only suggested but also custom headline names
var noun_open_headlines = {
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
        // add a suggestion with argument text as is
        suggs.push(CmdUtils.makeSugg(text, html, null, 
                        suggs.length? .1: 1, selectionIndices));

        return suggs;
    }
};


CmdUtils.CreateCommand({
    name: "org-capture",
    uuid: "F36F51E1-60B3-4451-B08E-6A4372DA74DD",
    arguments: [{role: "object", nountype: noun_arb_text, label: "text"},
                {role: "time",   nountype: ORG_FILES, label: "file"}, // at
                {role: "format", nountype: noun_open_headlines, label: "headline"}, // in
                {role: "alias",  nountype: ORG_FORMATS, label: "type"}, // as
    ],
    description: "A short description of your command.",
    help: "This text is displayed at the command list page.",
    author: "Your Name",
    icon: "http://example.com/favicon.png",
    // preview the capture options
    preview: function(pblock, {object, time, format, alias}) {
        let html = "";
        let tab = CmdUtils.getActiveTab();
        
        if (!tab) {
            pblock.innerHTML = "The current tab is unavailable for capture.";
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

        if (format && format.text)
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
        
        // get and pack capture parameters to org-protocol URL
        let title = b64enc(this._description);
        let url = b64enc(tab.url);
        let file = time && time.data? b64enc(time.data): "";
        let headline = format && format.text? b64enc(format.text): "";
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
        
        console.log(orgUrl);
        
        location.href = orgUrl;
    }
});

}
```

;..

```elisp
;; get the destination org file path specified in Ubiquity
(defun capture-get-destination-file ()
  ;; `capture-decoded-org-protocol-query' global variable contains
  ;; org-protocol url query parameters, stored earlier (see below)
  (plist-get capture-decoded-org-protocol-query :file))

;; get the destination org headline specified in Ubiquity (or insert one
;; if absent) and positon point near it in the buffer
(defun capture-get-destination-headline ()
    (let ((headline (plist-get capture-decoded-org-protocol-query :headline)))
      (goto-char (point-min))
      (if (re-search-forward (format org-complex-heading-regexp-format
                                     (regexp-quote headline))
                             nil t)
          (beginning-of-line)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "* " headline "\n")
        (beginning-of-line 0))))

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
  (capture-decoded-org-protocol-query nil) ; free memory
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