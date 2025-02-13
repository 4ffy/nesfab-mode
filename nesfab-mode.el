;;; nesfab-mode.el --- Major mode for editing NESFab source code.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Cameron Norton

;; Author: Cameron Norton <cameron.norton@gmail.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; NESFab is found at https://pubby.games/nesfab.html
;; Currently providing syntax highlighting and basic indentation.

;;; Code:

(eval-when-compile
  (require 'rx))

;;; Customization ==============================================================

(defgroup nesfab nil
  "Major mode for editing NESFab source code."
  :group 'languages)

(defcustom nesfab-indent-width tab-width
  "Indent width for NESFab."
  :type '(natnum)
  :safe 'natnump
  :group 'nesfab)

;;; Syntax table ===============================================================

(defvar nesfab-syntax-table
  (eval-when-compile
    (let ((tab (make-syntax-table prog-mode-syntax-table)))
      ;; Whitespace
      (modify-syntax-entry ?\n "> b" tab)
      ;; Identifiers
      (modify-syntax-entry '(?0 . ?9) "w" tab)
      (modify-syntax-entry '(?A . ?Z) "w" tab)
      (modify-syntax-entry '(?a . ?z) "w" tab)
      (modify-syntax-entry ?_ "_" tab)
      ;; Operators
      (modify-syntax-entry ?* ". 23n" tab)
      (modify-syntax-entry ?/ ". 124b" tab)
      ;; Strings
      (modify-syntax-entry ?\" "\"" tab)
      (modify-syntax-entry ?' "\"" tab)
      (modify-syntax-entry ?` "\"" tab)
      (modify-syntax-entry ?\\ "\\" tab)
      tab)))



;;; Constants ==================================================================

(defconst nesfab-keywords
  '("asm"
    "break"
    "case"
    "continue"
    "ct"
    "default"
    "do"
    "else"
    "fn"
    "for"
    "goto"
    "if"
    "label"
    "mode"
    "nmi"
    "return"
    "struct"
    "switch"
    "vars"
    "while")
  "NESFab keywords.")

(defconst nesfab-constants '("true" "false")
  "NESFab constant values.")

(defconst nesfab-increase-indent-tokens
  '("asm"
    "case"
    "default"
    "do"
    "else"
    "fn"
    "for"
    "if"
    "label"
    "mode"
    "nmi"
    "struct"
    "switch"
    "vars"
    "while")
  "NESFab tokens that should start a new block.")

(defconst nesfab-asm-keywords
  '("adc"
    "and"
    "asl"
    "bcc"
    "bcs"
    "beq"
    "bit"
    "bmi"
    "bne"
    "bpl"
    "brk"
    "bvc"
    "bvs"
    "clc"
    "cld"
    "cli"
    "clv"
    "cmp"
    "cpx"
    "cpy"
    "dec"
    "dex"
    "dey"
    "eor"
    "inc"
    "inx"
    "iny"
    "jmp"
    "jsr"
    "lda"
    "ldx"
    "ldy"
    "lsr"
    "nop"
    "ora"
    "pha"
    "php"
    "pla"
    "plp"
    "rol"
    "ror"
    "rti"
    "rts"
    "sbc"
    "sec"
    "sed"
    "sei"
    "sta"
    "stx"
    "sty"
    "tax"
    "tay"
    "tsx"
    "txa"
    "txs"
    "tya")
  "6502 assembly instructions.")



;;; Font locking ===============================================================

(defconst nesfab-identifier-regex
  (eval-when-compile
    (rx symbol-start (any "_" alpha) (* (any "_" alnum)) symbol-end))
  "Regex matching NESFab identifiers.")

(defconst nesfab-keyword-regex
  (eval-when-compile
    (rx symbol-start (eval `(or ,@nesfab-keywords)) symbol-end))
  "Regex matching NESFab keywords.")

(defconst nesfab-constant-regex
  (eval-when-compile
    (rx symbol-start (eval `(or ,@nesfab-constants)) symbol-end))
  "Regex matching NESFab constants.")

(defconst nesfab-asm-keyword-regex
  (eval-when-compile
    (rx symbol-start (eval `(or ,@nesfab-asm-keywords)) symbol-end))
  "Regex matching NESFab assembly instructions.")

(defconst nesfab-builtin-type-regex
  (eval-when-compile
    (rx
     symbol-start
     (or
      (** 1 3 "F") ; Floating
      (seq (** 1 3 (any "U" "S")) (** 0 3 "F")) ; Integer / fixed
      (seq (** 2 3 (any "C" "M" "P")) (* "/" (+ (any alnum "_")))) ; Pointer
      (** 2 3 "A") ; Address
      "Bool" "Int" "Real" "Void")
     symbol-end))
  "Regex matching NESFab builtin types.")

(defconst nesfab-variable-declaration-regex
  (eval-when-compile
    (rx
     (eval `(regexp ,nesfab-builtin-type-regex))
     (+ space)
     (group (eval `(regexp ,nesfab-identifier-regex))))))

(defconst nesfab-function-name-regex
  (eval-when-compile
    (rx
     line-start
     (* space)
     (or "asm fn" "fn" "mode" "nmi")
     (+ space)
     (group (eval `(regexp ,nesfab-identifier-regex)))))
  "Regex matching the function name in NESFab function declarations.")

(defconst nesfab-struct-name-regex
  (eval-when-compile
    (rx
     line-start
     (* space)
     "struct"
     (+ space)
     (group (eval `(regexp ,nesfab-identifier-regex)))))
  "Regex matching the struct name in NESFab struct declarations.")



;;; Font lock levels ===========================================================

;;; See (info "(elisp) Levels of Font Lock")

(defvar nesfab-font-lock-keywords-1 nil)

(defvar nesfab-font-lock-keywords-2
  `(,@nesfab-font-lock-keywords-1
    (,nesfab-keyword-regex . font-lock-keyword-face)
    (,nesfab-constant-regex . font-lock-constant-face)
    (,nesfab-asm-keyword-regex . font-lock-keyword-face)))

(defvar nesfab-font-lock-keywords-3
  `(,@nesfab-font-lock-keywords-2
    (,nesfab-builtin-type-regex . font-lock-type-face)
    (,nesfab-variable-declaration-regex 1 font-lock-variable-name-face)
    (,nesfab-function-name-regex 1 font-lock-function-name-face)
    (,nesfab-struct-name-regex 1 font-lock-type-face)))

(defvar nesfab-font-lock-keywords nesfab-font-lock-keywords-1)



;;; Indentation ================================================================

(defun nesfab--in-comment-p ()
  "Determine whether point is inside a comment."
  (save-excursion (elt (syntax-ppss (point)) 4)))

(defun nesfab--source-line-p ()
  "Determine whether point is on a nonblank source line."
  (save-excursion
    (move-beginning-of-line nil)
    (not
     (or (nesfab--in-comment-p)
         (looking-at "^[[:space:]]*$")
         (looking-at "^[[:space:]]*/[/*]")))))

(defun nesfab--annotation-line-p ()
  "Determine whether the current line has a function or loop annotation."
  (save-excursion
    (back-to-indentation)
    (looking-at ":")))

(defun nesfab--goto-last-source-line ()
  "Move point to the previous nonblank line, or to the beginning of the buffer."
  (forward-line -1)
  (while (and (not (nesfab--source-line-p)) (not (bobp)))
    (forward-line -1)))

(defun nesfab--first-token-of-line ()
  "Return the first token (up to the first space) of the current line."
  (save-excursion
    (back-to-indentation)
    (re-search-forward "[^[:space:]]+")
    (string-trim (match-string 0))))

(defun nesfab--block-start-p ()
  "Determine whether the current line starts a block."
  (member (nesfab--first-token-of-line) nesfab-increase-indent-tokens))

(defun nesfab--goto-last-block-start ()
  "Move point to the last block-starting line, or to the beginning of the buffer."
  (nesfab--goto-last-source-line)
  (while (and (not (nesfab--block-start-p)) (not (bobp)))
    (nesfab--goto-last-source-line)))

(defun nesfab--root-block-p ()
  "Determine whether there are no previous block-starting lines."
  (save-excursion
    (nesfab--goto-last-block-start)
    (and (bobp) (not (nesfab--block-start-p)))))

(defun nesfab--last-block-start-indent ()
  "Return the indentation of the previous block-starting line."
  (save-excursion
    (nesfab--goto-last-block-start)
    (current-indentation)))

(defun nesfab--calculate-block-indent ()
  "Use the last block-starting line to calculate an indent."
  (cond
   ((nesfab--root-block-p)
    0)
   ((nesfab--annotation-line-p)
    (nesfab--last-block-start-indent))
   (t
    (+ nesfab-indent-width (nesfab--last-block-start-indent)))))

(defun nesfab--indent-line (repeat)
  "Indent the current line.
If REPEAT is non-nil, decrease the current indent. Otherwise,
calculate indent according to block structure."
  (let ((block-indent (nesfab--calculate-block-indent))
        (decrease-indent (- (current-indentation) nesfab-indent-width)))
    (indent-line-to
     (if (and repeat (>= decrease-indent 0))
         decrease-indent
       block-indent))))

(defun nesfab-indent-line-function ()
  "Main indent function for NESFab."
  (nesfab--indent-line
   (and (eq this-command 'indent-for-tab-command)
        (eq last-command this-command))))

;;; Mode declaration ===========================================================

;;;###autoload
(define-derived-mode
 nesfab-mode prog-mode "NESFab" "Edit NESFab source code."
 :syntax-table nesfab-syntax-table
 (setq-local
  comment-end ""
  comment-start "//"
  electric-indent-inhibit t
  font-lock-defaults
  '((nesfab-font-lock-keywords
     nesfab-font-lock-keywords-1
     nesfab-font-lock-keywords-2
     nesfab-font-lock-keywords-3)
    nil nil nil nil)
  indent-line-function #'nesfab-indent-line-function
  indent-region-function (lambda (x y)) ; noop, not smart enough.
  indent-tabs-mode nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fab\\'" . nesfab-mode))

(provide 'nesfab-mode)
;;; nesfab-mode.el ends here
