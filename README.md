# Emacs Nova Theme

This is a dark, pastel color theme for Emacs based on Trevor Miller's [Nova](https://trevordmiller.com/projects/nova) color scheme.

## Installation from MELPA

1. `M-x package-refresh-contents` if necessary
1. `M-x package-install RET nova-theme`
1. `M-x load-theme RET nova`

Add `(load-theme 'nova t)` to your config to load at startup.

## Samples

![golang sample](https://github.com/muirmanders/emacs-nova-theme/raw/master/screenshot-go.png "golang sample")

## Philosophy
 * favor more color over less color where it helps to usefully distinguish things
 * but don't style to the point where _everything_ has a color
 * don't use bold in any prog modes, use sparingly otherwise
 * use underline very sparingly, perhaps only for links (I think it looks cheesy when used for emphasis)

## Customizing

I tried to make it easy to tweak faces and add new faces. Assuming you have the theme loaded, you can change or add new faces like this:

``` emacs-lisp
;; colors in nova-base-colors are available as variables
(nova-set-faces
  (some-face :foreground cyan)
  (some-other-face :background blue :inherit 'unspecified))
```

## Contributing

With few exceptions, I will only style modes I use myself. You are more than welcome to contribute styling for additional modes. Please open a PR and include before and after screenshots.

If you don't like existing styling or find a bug, please open an issue with a screenshot.
