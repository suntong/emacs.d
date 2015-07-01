This is my *modularized* `.emacs.d`. Two key niches separate mine with others:

* The humongous init file has been broken down into manageable building blocks.

  - Thus, it is very easy to disable the customization you don't need, with a single comment character, without the need to commenting out huge blocks.
  - That in turn makes trying out new feature just a flip of a single character, without the arduous efforts of commenting blocks of code and un-commenting them back and forth.
  - This also makes further customization limit to logical-and-reasonable *"local"* places, instead of figuring out which place in the *"global"* configuration should I put the new features in. 

* The customization are done with [minimalism](http://en.wikipedia.org/wiki/Minimalism_(computing)) as a prime goal, with all "unnecessary" code trimmed. I.e., use original features provided by Emacs out of box as much as possible. Only do further customization when Emacs fails to reach that whole night yards, e.g., showing the column number where the cursor is, or enabling selecting rectangular regions using mouse. BTW, if you are also into  [minimalism](http://en.wikipedia.org/wiki/Minimalism_(computing)) like me, check out my [emacs-traditional](http://sfxpt.wordpress.com/2014/12/13/bring-back-the-traditional-emacs/) as well.

It's offered to the public for educational purposes only. If you mean to use it as your own, no support is offered whatsoever because I'm not a elisp developer at all.

To install it,

```bash
rm -rf ~/.emacs.d # CAREFUL! Do once and backup first!
# do this every time when file(s) are newly added/deleted
[ -d lisp ] && [ -d site-lisp ] && lndir `pwd` ~/.emacs.d && symlinks -rd ~/.emacs.d/
```
