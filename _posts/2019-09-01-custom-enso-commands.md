---
layout: post
title: The new life of command interfaces
categories: [Enso Launcher, Python]
---

Command interfaces - an integral part of life in 1980s -  gain a new breath with
the advent of [Enso Launcher](https://gchristensen.github.io/enso-portable/) and
[iShell Extension](https://gchristensen.github.io/ishell/). Although
graphical user interfaces have opened a new world in computer industry, in
certain domains text commands still offer advantages in usability and
accessibility. With the tools mentioned above you can create powerful
commands that will free you from routine GUI interactions.

### A little example

Let's assume that you have a large random dump of music videos which you want to
sort to watch on your media-center PC. Normally, to sort files you open a video
in a player, assess it, stop playback, close video and navigate to the file explorer
to move the video into the corresponding destination directory. Obviously, this
takes a fair amount of manual actions.

Enso Launcher (v0.4.5+) allows to create a command, let's call it
'mv', that will automatically move a file opened in [Media Player
Classic](https://en.wikipedia.org/wiki/Media_Player_Classic) to the directory
specified as a command argument. Moreover, by using Enso
[mediaprobes](https://github.com/GChristensen/enso-portable#Mediaprobes) you can
populate MPC playlist from filesystem with no more than one command.

To be more specific, let's assume that you have made the following directory
tree of the destination music video categories:

```
D:/music
    ├───live
    │   ├───classical
    │   ├───heavy metal
    │   └───pop music
    └───music videos
        ├───classical
        ├───heavy metal
        └───pop music
```

Below we create all the necessary commands that will help us to move files
currently displayed in MPC to the directories shown above and open the result with 
mediaprobes:

<video src="/posts/videos/enso-demo.webm" width="100%" type="video/webm" controls></video>

You may explore the code in Enso command editor. The commands are completely
data-driven - the result will change whenever you modify the directory tree.

#### Obtaining category directories as command arguments

Since there are two levels of categories, we need to pack the both into a
single argument name. Let's take the first letter of the first level (for
example 'l' for 'live) and prepend it to the full name of the second level, so
we get 'lclassical', 'mclassical', etc. as command arguments. The same approach
is applicable if there are more levels, but it may utilize some separator
character.

```python
import os

MEDIA_ROOT = "d:/music"
 
def generate_category_args():  
    # get 'live', 'music videos' and any other folders
    supercats = os.listdir(MEDIA_ROOT)
    
    result = []
    
    for supercat in supercats:
        # get 'classical', 'heavy metal' and any other folders
        cats = os.listdir(os.path.join(MEDIA_ROOT, supercat))

        # the resulting argument names will consist of the first letter of
        # a super-category and the full sub-category name
        # if some super-categories begin with the same letter, add more   
        # starting letters to the argument name
        for cat in cats:
            result += [supercat[0] + cat]

    return result
```

#### Creating the 'mv' command

It is possible to install Enso with a set of MPC-related commands bundled with 
[mpcapi](https://github.com/Grokzen/mpcapi) library. We will use it in our 'mv' command below. 
NOTE: you need to enable Web UI in MPC settings.

```python
import re, requests, shutil, time
import mpcapi # you need to install Enso with MPC option enabled

MPC_HOST = "127.0.0.1"
MPC_PORT = "13579"

def cmd_mv(ensoapi, cat):
    # map the first letter of all super-categories to their full names
    supercats = dict(((sc[0], sc) for sc in os.listdir(MEDIA_ROOT)))
    
    # get super-category of argument
    supercat = supercats[cat[0]]
    # compose destination directory path from the argument name
    dest = os.path.join(MEDIA_ROOT, supercat, cat[1:])
    
    # get the full path of the file currently opened in MPC
    page = requests.get("http://" + MPC_HOST + ":" + MPC_PORT + "/variables.html")
    file = re.search("id=\"filepath\">([^<]*)<", page.text).group(1)
    # the full destination path with file name
    dest_file = os.path.join(dest, os.path.basename(file))
    
    # instantiate MPC client
    mpc = mpcapi.MpcAPI(host=MPC_HOST, port=MPC_PORT)
    
    if not os.path.exists(dest_file):
        mpc.close()      # stop playback and close the current file
        time.sleep(1)
        shutil.move(file, dest)
        mpc.play()       # play the next file

cmd_mv.valid_args = generate_category_args()

# a little helper command which lists available video categories
def cmd_wheremv(ensoapi):
    ensoapi.display_message(", ".join(cmd_mv.valid_args))

# MPC is also able to delete videos, so here is 'rm' command
def cmd_rm(ensoapi):
    mpcapi.MpcAPI(host=MPC_HOST, port=MPC_PORT).move_to_recycle_bin()
```

#### Creating mediaprobes for 'd:/music' subfolders

In the snippet below we create two commands named: 'live' and 'music videos'
which automatically obtain subcategories from the corresponding folders as
their own arguments and pass them to MPC when called. There may be as many probe commands
as subdirectories in 'd:/music'. See Enso tutorial and API documentation for
more details.

```python
from enso.user import mediaprobe

# the full MPC path may be different on your system
PLAYER = "C:/Program Files (x86)/MPC-HC64/mpc-hc64.exe"

mkcat = lambda c: os.path.join(MEDIA_ROOT, c)

for supercat in os.listdir(MEDIA_ROOT):
    cmdname = "cmd_" + supercat.replace(" ", "_")
    globals()[cmdname] = mediaprobe.directory_probe("cat", mkcat(supercat), PLAYER)
```