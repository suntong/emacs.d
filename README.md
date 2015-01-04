This is my *modularized* `.emacs.d`. The humongous init file has been broken down into manageable building blocks, with "unnecessary" code trimmed as well.

It's offered to the public for educational purposes only. If you mean to use it as your own, no support is offered whatsoever.

To install it,

```bash
rm -rf ~/.emacs.d # CAREFUL! Do once and backup first!
# do this every time when file(s) are newly added/deleted
lndir `pwd` ~/.emacs.d && symlinks -rd ~/.emacs.d/
```
