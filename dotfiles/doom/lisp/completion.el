;;; lisp/completion.el -*- lexical-binding: t; -*-

(with-eval-after-load 'orderless
  (setq orderless-component-separator #'orderless-escapable-split-on-space))
