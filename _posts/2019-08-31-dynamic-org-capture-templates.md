---
layout: post
title: Create dynamic capture templates to use with org-protocol. In Windows!
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