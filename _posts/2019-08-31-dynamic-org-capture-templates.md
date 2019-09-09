---
layout: post
title: Create dynamic capture templates to use with org-protocol. In Windows!
categories: [UbiquityWE, Emacs, elisp, JavaScript]
---


[org-protocol](https://orgmode.org/manual/Protocols.html) offers a nice
possibility to capture URLs along with some selected text from many web-browsers
into Emacs [org-mode](https://orgmode.org/). But most of the tools you can find
out there allow a little control over the capture process - usually it is
possible to put only plain text into one hardcoded org file. Below we develop an
[UbiquityWE](https://gchristensen.github.io/ubiquitywe/) command which allows to
capture HTML transformed to org-formatted text files under any
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
allows user to enter an arbitrary headline name. Although, in theory it is 
[possible](http://kitchingroup.cheme.cmu.edu/blog/2017/01/03/Find-stuff-in-org-mode-anywhere/)
to automatically maintain an index of all org-files and headlines and 
[obtain](https://github.com/eschulte/emacs-web-server)
them in Ubiquity, this is a work for real aficionados.

By default the command captures selection as plain text, but it is possible to
capture HTML-selection as org-formatted text, if the corresponding parameter
is specified in the command arguments.

To pass captured items to Emacs we use two custom org-protocol:// subprotocol names:
- `capture-ubiquity` - custom supbrotocol name defined in the 
section "Configuring Emacs" and used to pass plain UTF-8 text.
- `capture-html` - custom subprotocol name used to process HTML which is defined by
 [org-protocol-capture-html](https://github.com/alphapapa/org-protocol-capture-html)
library.

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
        let file = time && time.data? b64enc(time.data): "";
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