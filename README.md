## Niches

This is my *modularized* `.emacs.d`. Two key niches separate mine with others:

* The humongous init file has been broken down into manageable building blocks.

  - Thus, it is very easy to disable the customization you don't need, with a single comment character, without the need to commenting out huge blocks.
  - That in turn makes trying out new feature just a flip of a single character, without the arduous efforts of commenting blocks of code and un-commenting them back and forth.
  - This also makes further customization limit to logical-and-reasonable *"local"* places, instead of figuring out which place in the *"global"* configuration should I put the new features in. 

* The customization are done with [minimalism](http://en.wikipedia.org/wiki/Minimalism_(computing)) as a prime goal, with all "unnecessary" code trimmed. I.e., use original features provided by Emacs out of box as much as possible. Only do further customization when Emacs fails to reach that whole night yards, e.g., showing the column number where the cursor is, or enabling selecting rectangular regions using mouse. BTW, if you are also into  [minimalism](http://en.wikipedia.org/wiki/Minimalism_(computing)) like me, check out my [emacs-traditional](http://sfxpt.wordpress.com/2014/12/13/bring-back-the-traditional-emacs/) as well.

It's offered to the public for educational purposes only. If you mean to use it as your own, no support is offered whatsoever because I'm not a elisp developer at all.

## For Emacs Traditional

This is a companion for [Emacs Traditional](https://github.com/suntong/emacs-traditional/wiki/), as it is using packages directly from MELPA  as oppose to the OS level packages. It has many advantages, including but not limited to, [Emacs Traditional Precise Scrolling]( https://github.com/suntong/emacs-traditional/wiki/Emacs-Traditional-Precise-Scrolling/):

![Emacs Traditional Precise Scrolling](https://user-images.githubusercontent.com/422244/124361925-e2042d00-dbff-11eb-8d50-505d5d815cfc.gif)

Here is the video version of the above screencast:

https://user-images.githubusercontent.com/422244/124361941-0233ec00-dc00-11eb-9861-935d873c55c4.mp4

## IDE out of the box

- My recent experience working on Java code made me realize how important an IDE is to code editing. 
- Prior to that, I was just editing my Go code using the plain Emacs. 
- So I decided to turn my Emacs to a code IDE, at least to my Go, and the result is quite satisfying. 


https://user-images.githubusercontent.com/422244/124659805-598dc280-de73-11eb-973d-bd7132ab1918.mp4


## Install

To install it,

```bash
rm -rf ~/.emacs.d # CAREFUL! Do once and backup first!
mkdir ~/.emacs.d
# do this every time when file(s) are newly added/deleted
[ -d lisp ] && [ -d site-lisp ] && lndir `pwd` ~/.emacs.d && symlinks -rd ~/.emacs.d/
```
