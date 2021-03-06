---
layout: post
title: Reading huge files in plain JavaScript with the help of ES6 async generators
categories: [HTML, JavaScript]
---

It sometimes happens that you need to read gigabyte-long files on the client
side of a web application, and placing them as a whole into memory is not an
option. Of course, there is
[File.slice](https://developer.mozilla.org/en-US/docs/Web/API/Blob/slice), but
things get complicated when you have to read, for example, an
[UTF-8-encoded](https://en.wikipedia.org/wiki/UTF-8#History) text file. You need
to do some byte juggling, and ES6 [async
generators](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/for-await...of)
allow to perform this in a pretty elegant way.

In other words, is it possible to read a standard JavaScript
[File](https://developer.mozilla.org/en-US/docs/Web/API/File) line by line as
shown in the snippet below?

```javascript
for await (let line of ReadLine(file).lines()) {
    // do something...
}
```

Beyond the question! You just need something like that:

```javascript
// ReadLine in ES6
export class ReadLine {
    /* options:
         chunk_size:          Chunk size in bytes. Default is 256K.
    */
    constructor(file, options) {
        this.file           = file;
        this.offset         = 0;
        this.fileSize       = file.size;
        this.decoder        = new TextDecoder();
        this.reader         = new FileReader();

        this.chunkSize  = !options || typeof options.chunk_size === 'undefined' 
                            ?  256 * 1024 
                            : parseInt(options.chunk_size);
    }

    // async generator function 
    async *lines() {
        let remnantBytes;
        let remnantCharacters = "";

        for (let offset = 0; offset < this.fileSize; offset += this.chunkSize) {
            let chunk = await this.readChunk(offset);
            let bytes = new Uint8Array(chunk);
            let point = bytes.length - 1;
            let split = false;
            let remnant;

            // check if we landed not at the beginning of an UTF-8 character sequence
            if ((bytes[point] & 0b11000000) === 0b11000000)
                split = true;
            else {
                while (point && (bytes[point] & 0b11000000) === 0b10000000) {
                    point -= 1;
                }

                if (point !== bytes.length - 1)
                    split = true;
            }

            if (split) { // Yes, we landed not at the beginning
                // save remnant bytes
                remnant = bytes.slice(point);
                bytes = bytes.slice(0, point);

                if (remnantBytes) {
                    let newBytes = new Uint8Array(remnantBytes.length + bytes.length);
                    newBytes.set(remnantBytes);
                    newBytes.set(bytes, remnantBytes.length);
                    bytes = newBytes;
                }

                remnantBytes = remnant;
            }
            else {
                // use remnant bytes from the previous chunk to complete the line
                if (remnantBytes) {
                    let newBytes = new Uint8Array(remnantBytes.length + bytes.length);
                    newBytes.set(remnantBytes);
                    newBytes.set(bytes, remnantBytes.length);
                    bytes = newBytes;
                }

                remnantBytes = null;
            }

            // convert bytes to text
            let lines = this.decoder.decode(bytes).split("\n");

            if (lines.length === 1) { // save characters of incomplete line
                remnantCharacters = remnantCharacters + lines[0];
            }
            else if (lines.length) {
                // use remnant characters from the previous chunk
                if (remnantCharacters)
                    lines[0] = remnantCharacters + lines[0];

                remnantCharacters = lines[lines.length - 1];
                lines.length = lines.length - 1;

                // return obtained lines through yield delegation
                yield* lines;
            }
        }

        yield remnantCharacters;
    }

    // wrap FileReader into a promise to use with ES6 async/await
    readChunk(offset) {
        return new Promise((resolve, reject) => {
            this.reader.onloadend = () => {
                resolve(this.reader.result);
            };
            this.reader.onerror = e => {
                reject(e);
            };

            let chunk = this.file.slice(offset, offset + this.chunkSize);
            this.reader.readAsArrayBuffer(chunk);
        });
    }
}
```