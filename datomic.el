;;; datomic.el --- Utility functions for working with Datomic projects  -*- lexical-binding: t -*-

;; Copyright (c) 2019 Daniel Kraus <daniel@kraus.my>

;; Author: Daniel Kraus <daniel@kraus.my>
;; URL: https://github.com/dakra/datomic.el
;; Keywords: clojure, datomic, ions, convenience, tools, processes
;; Version: 0.1
;; Package-Requires: ((emacs "25.2") (async "1.9") (parseedn "0.1") (projectile "2.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

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
;;
;; `datomic.el' provides utilities for working with the Datomic.

;;; Code:

(require 'async)
(require 'compile)
(require 'parseedn)
(require 'projectile)


;;; Customization

(defgroup datomic nil
  "Datomic utilities"
  :prefix "datomic-"
  :group 'tools)

(defcustom datomic-access-program "datomic-access"
  "Path to the datomic-access bash script."
  :type 'file)

(defcustom datomic-analytics-program "datomic-analytics"
  "Path to the datomic-analytics bash script."
  :type 'file)

(defcustom datomic-gateway-program "datomic-gateway"
  "Path to the datomic-gateway bash script."
  :type 'file)

(defcustom datomic-systems nil
  "List of datomic system names."
  :type 'list)


;;; Variables

(defvar datomic-ion-deploy-group)
(defvar datomic-ion-last-execution-arn)
(defvar datomic-ion-last-rev)


;;; Functions

(defun datomic--read-system ()
  "Read Datomic system name from user."
  (if (and (car datomic-systems) (not (cdr datomic-systems)))
      (car datomic-systems)
    (if (not (car datomic-systems))
        (read-string "System name: ")
      (completing-read "System name: " datomic-systems))))

;; FIXME: -p aws-profile and -r aws-region --port port
;;;###autoload
(defun datomic-access-client (system)
  "Start the datomic-access client tunnel to SYSTEM."
  (interactive (list (datomic--read-system)))
  (let ((cmd (concat datomic-access-program " client " system))
        (name-fn (lambda (_mode) (concat "*" (file-name-base datomic-access-program) " client " system "*"))))
    (compilation-start cmd nil name-fn)))


;;;###autoload
(defun datomic-ion-push ()
  "Push the current project Datomic Ion."
  (interactive)
  (let* ((default-directory (projectile-project-root))
         (name "ion push")
         (program "clojure")
         (args '("-A:dev" "-m" "datomic.ion.dev" "{:op :push}"))
         (finish-func (lambda (proc)
                        (save-excursion
                          (with-current-buffer
                              (process-buffer proc))
                          (goto-char (point-min))
                          (kill-line 3)
                          (let ((edn (car (parseedn-read))))
                            (setq datomic-ion-deploy-group
                                  (car (gethash ':deploy-groups edn)))
                            (setq datomic-ion-last-rev (gethash ':rev edn)))
                          (message "deploy command %s"
                                   (format "'{:op :deploy, :group %s, :rev \"%s\"}'"
                                           datomic-ion-deploy-group datomic-ion-last-rev))))))
    (apply #'async-start-process name program finish-func args)))


(defun datomic-ion-deploy ()
  "Deploy the last pushed Datomic Ion of the current project."
  (interactive)
  (when (yes-or-no-p
         (format "Deploy %s revision '%s' to group %s? "
                 (projectile-project-name) datomic-ion-last-rev datomic-ion-deploy-group))
    (let* ((default-directory (projectile-project-root))
           (name "ion deploy")
           (program "clojure")
           (args `("-A:dev" "-m" "datomic.ion.dev"
                   ,(format "{:op :deploy, :group %s, :rev \"%s\"}"
                            datomic-ion-deploy-group datomic-ion-last-rev)))
           (finish-func (lambda (proc)
                          (save-excursion
                            (with-current-buffer
                                (process-buffer proc))
                            (goto-char (point-min))
                            (let ((edn (car (parseedn-read))))
                              (setq datomic-ion-last-execution-arn (gethash ':execution-arn edn)))))))
      (apply #'async-start-process name program finish-func args))))

(defun datomic-ion-status ()
  "Get the status of the last Datomic Ion deploy."
  (interactive)
  (let* ((default-directory (projectile-project-root))
         (name "ion deploy")
         (program "clojure")
         (args `("-A:dev" "-m" "datomic.ion.dev"
                 ,(format "{:op :deploy-status, :execution-arn %s}"
                          datomic-ion-last-execution-arn)))
         (finish-func (lambda (proc)
                        (save-excursion
                          (with-current-buffer
                              (process-buffer proc))
                          (goto-char (point-min))
                          (let* ((edn (car (parseedn-read)))
                                 (status (gethash ':deploy-status edn))
                                 (code-status (gethash ':code-deploy-status edn)))
                            (if (and (or (string= status "RUNNING") (string= code-status "RUNNING"))
                                     (yes-or-no-p "Deploy still running. Check status again? "))
                                (datomic-ion-status)
                              (message "Deploy status: %s - Code Deploy status: %s"
                                       status code-status)))))))
    (apply #'async-start-process name program finish-func args)))

(provide 'datomic)
;;; datomic.el ends here
