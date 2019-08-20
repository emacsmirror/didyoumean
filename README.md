# DidYouMean.el

Ask for the right file to open.

Port of [EinfachToll/DidYouMean](https://github.com/EinfachToll/DidYouMean).

![demo](https://kisaragi-hiu.com/static/emacs-didyoumean.gif)

## Why

You want to edit `hello.c` from the terminal. You type `emacs he` <kbd>TAB</kbd> <kbd>RET</kbd> expecting your shell to expand it to the right file name. Emacs opens up an empty `hello.` because there is a `hello.h` in the same folder. This package makes Emacs ask you which of these files you actually want to edit.

## Install

### [`straight.el`](https://github.com/raxod502/straight.el)

```emacs-lisp
(straight-use-package '(didyoumean :type git :host gitlab :repo "kisaragi-hiu/didyoumean.el"))
```

### [`quelpa`](https://framagit.org/steckerhalter/quelpa)

```emacs-lisp
(quelpa '(didyoumean :fetcher gitlab :repo "kisaragi-hiu/didyoumean.el"))
```

### Manually

Put `doyoumean.el` under your `load-path`, then `(require 'doyoumean)` in your init file.

## Customize

- `didyoumean-ignored-extensions`: List of suffixes that will not be suggested.
- `didyoumean-custom-ignore-function`: Custom predicate to not suggest some files.

## License

MIT

# Credits

The introduction paragraph, the core function (`didyoumean`), and the idea itself are all ported from the original ([EinfachToll/DidYouMean](https://github.com/EinfachToll/DidYouMean)).
