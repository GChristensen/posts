---
layout: post
title: The new life of command interfaces
categories: [Enso Launcher, Python]
---

Command interfaces - an integral part of life in 1980s -  gain a new breath with the advent of 
[Enso Launcher](https://gchristensen.github.io/enso-portable/) and 
[Ubiquity Web Extension](https://gchristensen.github.io/ubiquitywe/). Although graphical user interfaces
have opened a new world in computer industry, in certain domains text commands still offer
advantages in usability and accessibility. If your subject area could be relatively easily formalized 
and you have some API to act on, with the tools mentioned above you can create powerful commands
that will free you from routine GUI interactions.

### A little example

Let's assume that you have a large random dump of music videos which you want to sort to watch on your
media-center PC using Enso Launcher [mediaprobes](https://github.com/GChristensen/enso-portable#Mediaprobes).
Normally, to sort files you open a video in the player, assess it, stop playback and navigate to 
file explorer to move the video into the corresponding destination directory.
To be more specific, let's assume that you have made the following directory tree of destination categories:

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

Enso Launcher 0.4.5+ allows to create a command that will automatically move the file opened in 
[Media Player Classic](https://en.wikipedia.org/wiki/Media_Player_Classic) to the directory 
specified as a command argument. NOTE: you need to enable
Web UI in MPC settings.

<video src="videos/enso-demo.webm" width="100%" type="video/webm" muted autoplay loop></video>

You may play with the following code in Enso command editor.

#### Obtaining category directories as command arguments

Since there are two levels of categories, we need to pack the both levels into a single argument name.
Let's take the first letter of the first level (for example 'l' for 'live) and prepend it to the full
name of the second level, so we get 'lclassical', 'mclassical', etc. as command arguments.
The same approach is applicable if there are more levels, but it may utilize some separator character. 

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
    # compose full destination path from the argument name removing 
    # command first letter
    dest = os.path.join(MEDIA_ROOT, supercat, cat[1:])
    
    # get the full path of the file currently opened in MPC
    page = requests.get("http://" + MPC_HOST + ":" + MPC_PORT + "/variables.html")
    file = re.search("id=\"filepath\">([^<]*)<", page.text).group(1)
    # the full destination path
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

In the snippet below we create two commands named: 'live' and 'music video' which automatically obtain 
subcategories from the corresponding folders as arguments and pass them to MPC when called. 

```python
from enso.user import mediaprobe

# the full MPC path may be different on your system
PLAYER = "C:/Program Files (x86)/MPC-HC64/mpc-hc64.exe"

mkcat = lambda c: os.path.join(MEDIA_ROOT, c)

cmd_live = mediaprobe.directory_probe("cat", mkcat("live"), PLAYER)
cmd_music_videos = mediaprobe.directory_probe("cat", mkcat("music videos"), PLAYER)
```