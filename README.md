lxpanel-window-title
====================

Simple and minimalistic plugin for lxpanel to show title of current window.
Intended for use with [xmonad](http://xmonad.org/) as WM.

Based on example plugin from lxpanel plugins HOWTO.

![How does it look like.](screenshots/1.png)

Minimal `xmonad.hs` to work with lxpanel and lxpanel-window-title:

~~~ { .haskell }
module Main ( main ) where

import XMonad
import XMonad.Config.Desktop ( desktopConfig )
import XMonad.Hooks.ManageDocks ( manageDocks )

main :: IO ()
main = xmonad $ desktopConfig
    { manageHook = manageDocks <+> manageHook desktopConfig
    }
~~~

Installation
------------

Usual sequence should suffice:

~~~ { .bash }
./autogen.sh
./configure
make && make install
~~~

Development
-----------

Refer to [LXPanel plugin development](http://wiki.lxde.org/en/LXPanel_plugin_development).

~~~ { .bash }
./configure CFLAGS='-Wall -pedantic -std=gnu99 -Werror'
~~~
