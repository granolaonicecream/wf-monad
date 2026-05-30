# About

This repo is an experiment after discussions around the difficulty of supporting a real Wayland compositor, even one using wl-roots, that has capabilities akin to XMonad.  wf-monad is an attempt to use [Wayfire](https://wayfire.org/)'s IPC plugin to manage window state, somewhat analagous to xlib, but with only the capability implemented by the compositor.

# Disclaimer

This has all been AI slop coded with some light manual review to keep Claude on track.  Don't take the code organization or names too seriously.  Any serious attempt at this approach should interrogate the design choices, though many are copied directly from XMonad.

# Features

I copied some of XMonad's core concepts, including Layout, ManageHook, StackSet.  The event loop will look vaguely similar to handling XEvent, but instead reads JSON messages over IPC.  The user configuration section in Main.hs should be familiar to XMonad users.

# Installation

No effort has been spent on making this reproducible. Hopefully cabal install works.

Wayfire will need to be installed and configured to use the `ipc` and `ipc-rules` plugins.

Your Wayfire configuration should set only a single workspace, since wf-monad manages its own off-screen coordinates.
```
[core]
vheight = 1
vwidth = 1
```

# Running

This is currently just an executable that connects to the `$WAYFIRE_SOCKET` set up by the IPC plugin.  cabal run in the installation repo to start the process.

# Issues

* Floating windows are kind of weird.  Wayfire doesn't seem to support letting another process handle click and drag, but also doesn't tell us when a drag event starts for us to appropriately mark a window as untiled.  The workaround is binding a key to manually untile a window.
* Wayfire doesn't send event bindings if they get handled by another plugin.  e.g. super + BTN_LEFT won't be sent to us if Wayfire is configured to use it for moving windows.  This means we can't add extra stuff around that action
* Fullscreen works differently than in X11. It's more a compositor native feature now that we don't really control.
* No error handling in our dispatch loop :^)
