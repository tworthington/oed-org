;;; oed-org.el --- A package to interrogate the OED's API for the definition of the word at point  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Thomas Worthington

;; Author: Thomas Worthington <thomas.worthington@london.ac.uk>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Code to look up the word at point in the OED on-line API and
;; display it in an org-mode temp buffer.
;;

;;; Code:

(provide 'oed-org)

;;; Customisation
(defgroup oed-org nil
  "API credentials for accessing the OED online"
  :group 'convenience)

(defcustom oed-app-id ""
  "App ID obtained from OED"
  :type 'string
  :group 'oed-org)

(defcustom oed-app-key ""
  "App key obtained from OED"
  :type 'string
  :group 'oed-org)

(defun unescape-string(s)
  (decode-coding-string s 'utf-8))

(defun oed-lookup-word (word)
  "A test"
  (interactive "s")
  (request
   (concat "https://od-api.oxforddictionaries.com:443/api/v1/entries/en/" word)
   :headers `(("app_id" . ,oed-app-id)
              ("app_key" . ,oed-app-key)
              ("Accept" . "application/json"))
   :parser 'json-read
   :sync t
   :timeout 10
   :success (cl-function (lambda (&key data &allow-other-keys)
                           (let-alist data
                             (setq cache .results)
                             )
                           ))))

(defun oed-lookup-lemmatron (word)
  "Find the root word of an inflection"
  (interactive "s")
  (request
   (concat "https://od-api.oxforddictionaries.com:443/api/v1/inflections/en/" word)
   :headers `(("app_id" . ,oed-app-id)
              ("app_key" . ,oed-app-key)
              ("Accept" . "application/json"))
   :parser 'json-read
   :sync t
   :timeout 10
   :error (cl-function (lambda (&key data &allow-other-keys)
                         (message (pp-to-string data))
                           ))
   :success (cl-function (lambda (&key data &allow-other-keys)
                           (let-alist data
                             (setq cache .results)
                             )
                           ))))

(defun oed-jpath (data path)
  "Access element of data using a path of the form (index index....) where an index is either an array index or a string"
  (let ((dp (copy-tree data))
        (idx 0))
    (when dp
      (dolist (idx path dp)
        (setq idx (pop path))
        (if (numberp idx)
            (setq dp (aref dp idx))
          (setq dp (cdr (assoc idx dp)))
          )
        )
      )
    )
  )

(defun oed-lemma (word)
  "Return the root word for the given word"
  (oed-lookup-lemmatron word)
  (let ((result (oed-jpath cache '(0 lexicalEntries 0))))
    (if (assoc 'inflectionOf result)
        (oed-jpath result '(inflectionOf 0 text))
      (oed-jpath result '(derivativeOf 0 text))
  )))

(defun oed-cprint (&rest things)
  "Concatenate and print things"
  (mapc 'princ things)
  (terpri))

(defun oed-printcdrs(things &optional label pre post)
  "Print the cdrs of things (which is assumed to be an associative list) one per line labeled with label"
  (or label (setq label ""))
  (or pre (setq pre ""))
  (mapc (lambda (x)
          (oed-cprint   (concat label (oed-wrap (decode-coding-string (cdr x) 'utf-8) pre post)))) things )
  )

(defun oed-listselect (list filter)
  "Pass back a version of the list with only the key->value pairs in filter, in the order of the filter"
  (mapcar (lambda (item)
            (assoc item list)
            ) filter)
  )

(defun oed-vhead (vector)
  "Return the first item in an array"
  (if (< 0 (length vector))
      (aref vector 0)
    nil))

(defun oed-wrap(thing pre &optional post)
  (or post (setq post pre))
  (concat pre thing post))

(defun oed-expand-examples(raw depth)
  (let ((examples raw)
        (extext))
    (mapc (lambda(e)(let-alist (oed-listselect e '(registers text))
                       (setq extext (concat "/" .text "/"))
                       (if .registers
                           (setq extext (concat "[" (string-join (append .registers nil) ",") "] " extext)))
                       (oed-cprint (make-string depth ? ) "- " (unescape-string extext)))) examples)))

(defun oed-expand-sense (raw &optional depth)
  "Print the definition of a sense, the examples associated with it, and recursively expand any sub-senses"
  (or depth (setq depth 2))
  (let-alist (oed-listselect raw '(definitions examples subsenses registers domains crossReferenceMarkers regions))
    (setq .definitions (or .definitions .crossReferenceMarkers))
    (when .definitions
      (oed-cprint
       (make-string depth ?*)
       " "
       (if (or .regions .domains) (concat "[" (string-join (append .domains (append .regions nil)) ", " ) "] ") "" )
       (unescape-string (oed-vhead .definitions))
       (if .registers (concat " (" (string-join (append .registers nil) ", " ) ")") "" )
       )
      (cond ((< 0 (length .examples))
             (oed-expand-examples .examples depth)
             (oed-cprint "")
             ))
      (cond ((< 0 (length .subsenses))
             (mapc (lambda(s)(oed-expand-sense s (1+ depth))) .subsenses)
             ))
      (oed-cprint "")
      )))

(defun oed-expand-pronunciations(list)
  (mapc (lambda(p)
          (let-alist p
            (oed-cprint "   - Dialects: " (string-join .dialects ", "))
            (if .audioFile
                (oed-cprint "     - [[" .audioFile "][" (unescape-string .phoneticSpelling) "]]")
              (oed-cprint "     - " (unescape-string .phoneticSpelling) " "))
            )
          ) list)
  )

(defun oed-listcollect(data key)
  (let (result)
    (setq result
          (mapcar (lambda(i)
                    (alist-get key i)) data))
    (string-join result ", ")))

(defun oed-expand-entry (raw)
  "Print the basics information about an individual word usage"
  (let-alist (oed-listselect raw '(etymologies senses pronunciations notes grammaticalFeatures))
    (if .grammaticalFeatures
        (oed-cprint "  - " (unescape-string (oed-listcollect .grammaticalFeatures 'text))))
    (when .pronunciations
      (oed-cprint "  - Pronunciation: ")
      (oed-expand-pronunciations .pronunciations))
    (if .etymologies
        (oed-cprint "  - Etymology: " (decode-coding-string (oed-vhead .etymologies ) 'utf-8)))

    (oed-cprint "")
    (mapc 'oed-expand-sense .senses)
    (if .notes
        (oed-cprint (unescape-string (cdr (car (aref .notes 0)))) "\n"))
  ))

(defun oed-bufferset()
  "Things to do to the buffer after it's filled with the OED data."
    (indent-region (point-min) (point-max))
  )

(defun oed-quickword ()
  "Look up the word at point and put the result in the mini-buffer fence"
  (interactive)
  (setq cache nil)
  (let ((theword (downcase (word-at-point)))
        (temp-buffer-setup-hook)
        (temp-buffer-show-hook)
        )
    (add-hook 'temp-buffer-setup-hook 'org-mode)
    (add-hook 'temp-buffer-show-hook 'oed-bufferset)
    (with-output-to-temp-buffer (concat "*word-meanings*")
      (oed-cprint "#+TITLE: " theword)
      (oed-lookup-word theword)
      (setq firstlookup cache)
      (unless cache
        (sleep-for 1)
        (setq theword (oed-lemma theword))
        (if theword
            (oed-lookup-word theword))
        )
      (if cache
          (progn
            (oed-cprint "#+SUBTITLE: " theword)
            (let ((bits (oed-jpath cache '(0 lexicalEntries))))
              (mapc (lambda(x)
                      (let ((entries (oed-jpath x '(entries)))
                            (speech (oed-jpath x '(pronunciations)))
                            )
                        (mapc (lambda(e)
                                (oed-cprint "* " (oed-jpath x '(lexicalCategory)))
                                (when speech
                                  (oed-cprint "  - Pronunciation: ")
                                  (oed-expand-pronunciations speech))
                                (oed-expand-entry e)
                                ) entries)
                        )
                      ) bits)
              ;;              (org-mode)

              (oed-cprint "* Raw")
              (oed-cprint (oed-wrap (pp-to-string bits) "#+BEGIN_SRC javascript\n" "#+END_SRC"))
              )
            )
        (oed-cprint theword " not found!")
        ))))

(defun mplayer-mp3-link()
  "See if we're on an org-link to an mp3 and, if so, play it using mplayer"
  (save-excursion
    (when (and (looking-back (concat "\\[\\[\\(?:\\(http:[^][\n\r]+\\.mp3\\)?\\]\\[\\)?[^]]*\\(\\]\\)?"))
           (goto-char (match-beginning 0))
           (looking-at (concat "\\[\\[\\(http:[^][\n\r]+\\.mp3\\)\\]")))
      (if (match-string-no-properties 1)
          (progn
            (start-process "" nil "mplayer" (match-string-no-properties 1))
            t)
        (nil)
      )))
  )

(add-hook 'org-open-at-point-functions 'mplayer-mp3-link)

(provide 'oed-org)
;;; oed-org.el ends here
