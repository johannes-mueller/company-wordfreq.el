;;; company-wordfreq.el --- Company backend for human language texts

;; Copyright (C) 2021 Johannes Mueller

;; Author: Johannes Mueller <github@johannes-mueller.org>
;; URL: https://github.com/johannes-mueller/company-wordfreq.el
;; Version: 0.1.0
;; Keywords: company, convenience, matching
;; Package-Requires: ((emacs "27.1"))

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation version 2. <https://www.gnu.org/licenses/>

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; `company-wordfreq' is a company backup intended for writing texts in a human
;; language.  The completions it proposes are words already used in the current
;; (or another open) buffer and matching words from a word list file.  This
;; word list file is supposed to be a simple list of words ordered by the
;; frequency the words are used in the language.  So the first completions are
;; words already used in the buffer followed by matching words of the language
;; ordered by frequency.
;;
;; `company-wordfreq' does not come with the word list files, so you have to
;; provide them yourself.  A good source for many languages is
;; <https://github.com/hermitdave/FrequencyWords>.  In the files that are
;; provided in this repo the words come with a frequency value.  You have to
;; clean those out, for example by
;;
;; $ sed -i 's/ [0-9].*$//' <filename>
;;
;; The directory where the word list files reside is determined by the variable
;; `=company-word-freq-path=', default `~/.emacs.d/wordfreq-dicts'.  Their
;; names must follow the pattern `<language>.txt' where language is the
;; `ispell-local-dictionary' value of the current language.
;;
;; You need `rg' alias ripgrep in your `$PATH' as `company-wordfreq' uses it to
;; grep into the word list files.
;;
;; `company-wordfreq' is supposed to be the one and only company backend and
;; `company-mode' should not transform or sort its candidates.  This can be
;; achieved by setting the variables `company-backends' and
;; `company-transformers' buffer locally in `text-mode' buffers by
;;
;;     (add-hook 'text-mode-hook (lambda ()
;;                              (setq-local company-backends '(company-wordfreq))
;;                              (setq-local company-transformers nil)))

;;; Code:

(require 'cl-macs)
(require 'company)

(defun company-wordfreq--default-path ()
  "Set up the default for company-wordfreq-path."
  (concat (file-name-as-directory user-emacs-directory) "wordfreq-dicts"))

(defcustom company-wordfreq-path (company-wordfreq--default-path)
  "Path where the dictionary files reside.

The dictionary files are expected to have the name <language>.txt
where <language> is the contents of `ispell-local-dictionary' in
the current buffer."
  :type 'string
  :group 'company)

(defconst company-wordfreq--rg-executable
  "The PATH of the `rg' executable.

A warning is issued if it can't be found on loading."
  (if-let ((executable (executable-find "rg")))
      executable
    (warn "No rg executable found in PATH.")))

(defun company-wordfreq--dictionary ()
  "Determine the path of the word list file."
  (concat (file-name-as-directory company-wordfreq-path) ispell-local-dictionary ".txt"))

(defun company-wordfreq--candidates (prefix)
  "Fetches the candidates."
  (split-string (shell-command-to-string (concat
					  (executable-find "rg")
					  " -i -N ^" prefix " " (company-wordfreq--dictionary)))
		"\n"))

(defun company-wordfreq (command &optional arg &rest ignored)
  "A company backup intended for writing texts in a human language.

The completions it proposes are words already used in the
current (or another open) buffer and matching words from a word
list file.  This word list file is supposed to be a simple list
of words ordered by the frequency the words are used in the
language.  So the first completions are words already used in the
buffer followed by matching words of the language ordered by
frequency."
  (interactive (list 'interactive))
  (cl-case command
    (prefix (when-let ((prefix (company-grab-word)))
	      (substring-no-properties prefix)))
    (sorted t)
    (duplicates nil)
    (ignore-case 'keep-prefix)
    (candidates (let ((candidates (append (company-dabbrev 'candidates arg)
					  (company-wordfreq--candidates arg)))
		      (completion-ignore-case t))
		  (all-completions arg (delete-dups candidates))))))

(provide 'company-wordfreq)
;;; company-wordfreq.el ends here
