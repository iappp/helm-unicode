;;; helm-unicode.el --- Helm command for unicode characters. -*- lexical-binding: t -*-

;; Copyright © 2015 Emanuel Evans

;; Version: 0.0.4
;; Package-Requires: ((helm "1.9.8") (emacs "24.4"))

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
;; A helm command for looking up unicode characters by name 😉.

;;; Code:

(require 'helm)
(require 'helm-utils)

(defvar *helm-unicode/names* '()
  "Internal cache variable for unicode characters.  Should not be changed by the user.")

(defconst +helm-unicode/ucs-names+ (ucs-names))
(defconst +helm-unicode/ucs-names-keys+ (hash-table-keys +helm-unicode/ucs-names+))
(defvar helm-unicode/progress-reporter nil)

(defun helm-unicode-format-char-pair (char-pair)
  "Formats a char pair for helm unicode search."
  (let ((name (car char-pair))
        (symbol (cdr char-pair)))
    (format "%s %c" name symbol)))

;; (defun helm-unicode-build-candidates ()
;;   "Builds the candidate list."
;;   (when (timerp helm-unicode-timer)
;;     (cancel-timer helm-unicode-timer)
;;     (setq helm-unicode-name '()))
;;   (let* ((candidate-list '())
;;          (pr (make-progress-reporter "Collecting Unicode symbols… "
;;                                      0 (length +helm-unicode/ucs-names-keys+)))
;;          (unames (when (hash-table-p un)
;;                    (maphash
;;                     #'(lambda (k v)
;;                         (add-to-list 'candidate-list
;;                                      (helm-unicode-format-char-pair (cons k  v)))
;;                         (progress-reporter-update pr (length candidate-list))
;;                         )
;;                     un))))
;;     (setq *helm-unicode/names* (sort candidate-list #'string-lessp))))

(defun helm-unicode-build-candidates-incrementally (increment &optional show-progress?)
  "Builds the candidate list."
  (cond ((and (>= (length *helm-unicode/names*) (length +helm-unicode/ucs-names-keys+))
              (timerp helm-unicode-timer))
         (cancel-timer helm-unicode-timer))
        ((< (length *helm-unicode/names*) (length +helm-unicode/ucs-names-keys+))
         (let ((candidate-list))
           (and show-progress?
                (null helm-unicode/progress-reporter)
                (setq helm-unicode/progress-reporter
                      (make-progress-reporter "Collecting Unicode symbols… "
                                              (length *helm-unicode/names*)
                                              (length +helm-unicode/ucs-names-keys+))))
           (dolist (k (subseq +helm-unicode/ucs-names-keys+
                              (length *helm-unicode/names*)
                              (+ (length *helm-unicode/names*) increment)))
             (add-to-list 'candidate-list
                          (helm-unicode-format-char-pair
                           (cons k (gethash k +helm-unicode/ucs-names+)))))
           (nconc *helm-unicode/names* candidate-list)
           (and show-progress?
                (progress-reporter-update helm-unicode/progress-reporter (length *helm-unicode/names*)))))))

(defun helm-unicode-build-candidates ()
  "Forces building the rest of the candidate list synchronously"
  (helm-unicode-build-candidates-incrementally (- (length +helm-unicode/ucs-names-keys+)
                                                  (length *helm-unicode/names*))
                                               t ;; show progress
                                               ))

(defun helm-unicode-load-in-background ()
  (setq helm-unicode-timer
        (run-with-idle-timer 2 t #'helm-unicode-build-candidates-incrementally 1000)))

(defun build-helm-source ()
  (helm-build-sync-source "unicode-characters"
    :candidates *helm-unicode/names*
    :filtered-candidate-transformer (lambda (candidates _source) (sort candidates #'helm-generic-sort-fn))
    :action '(("Insert Character" . helm-unicode-insert-char))))

(defun helm-unicode-source ()
  "Builds the helm Unicode source.  Initialize the lookup cache if necessary."
  (if (>= (length *helm-unicode/names*) (length +helm-unicode/ucs-names-keys+))
      (build-helm-source)
    (progn (helm-unicode-build-candidates)
           (build-helm-source))))

(defun helm-unicode-insert-char (candidate)
  "Insert CANDIDATE into the main buffer."
  (insert (substring candidate -1)))

;;;###autoload
(defun helm-unicode (arg)
  "Preconfigured `helm' for looking up unicode characters by name.

With prefix ARG, reinitialize the cache."
  (interactive "P")
  (when arg (setq *helm-unicode/names* nil))
  (helm :sources (helm-unicode-source)
        :buffer "*helm-unicode-search*"))

(provide 'helm-unicode)

(helm-unicode-load-in-background)

;;; helm-unicode.el ends here
