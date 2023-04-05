;;; datomic.el --- Utility functions for working with Datomic projects  -*- lexical-binding: t -*-

;; Copyright (c) 2019-2023 Daniel Kraus <daniel@kraus.my>

;; Author: Daniel Kraus <daniel@kraus.my>
;; URL: https://github.com/dakra/datomic.el
;; Keywords: clojure, datomic, ions, convenience, tools, processes
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (async "1.9.7") (parseedn "1.0.6") (project "0.9.5"))
;; SPDX-License-Identifier: GPL-3.0-or-later

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
(require 'project)


;;; Customization

(defgroup datomic nil
  "Datomic utilities."
  :prefix "datomic-"
  :group 'tools)

(defcustom datomic-ion-auto-deploy t
  "Whether or not to automatically deploy after a Ion push."
  :type 'boolean)

(defcustom datomic-ion-auto-check-status t
  "Whether or not to automatically poll the status after a Ion deploy."
  :type 'boolean)

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
  :type '(repeat string)
  :safe (lambda (value)
          (and (listp value)
               (cl-every 'stringp value))))

(defcustom datomic-cli-command "clojure -Sdeps '{:deps {com.datomic/tools.ops {:mvn/version \"1.0.91\"}}}' -M -m datomic.tools.ops"
  "Command for the datomic CLI Tools.
Can be just \"datomic\" if you installed the datomic CLI tools or the one-line
content of the datomic shell script.
See https://docs.datomic.com/cloud/operation/cli-tools.html#client-acces"
  :type 'string)


;;; Variables

(defvar datomic-ion-deploy-group)
(defvar datomic-ion-last-execution-arn)
(defvar datomic-ion-last-rev)


;;; Functions

(defun datomic--get-systems ()
  "Return a list of Datomic systems."
  (let* ((cmd (concat datomic-cli-command " cloud list-systems"))
         (out (shell-command-to-string cmd))
         (systems (json-parse-string out)))
    (mapcar (lambda (x)
              (gethash "name" x))
            systems)))

(defun datomic--read-system ()
  "Read Datomic system name from user."
  (unless datomic-systems
    (message "Variable `datomic-systems' not set. Fetching systems from cloud.")
    (setq datomic-systems (datomic--get-systems)))
  (if (length= datomic-systems 1)
      (car datomic-systems)
    (if (not (car datomic-systems))
        (read-string "System name: ")
      (completing-read "System name: " datomic-systems))))

(defun datomic--yes-or-no-conflicts-p (conflicts)
  "List dependency CONFLICTS and ask user if he really wants to continue."
  (yes-or-no-p
   (format (concat "You have dependency conflicts:\n"
                   "%s\n"
                   "Relly deploy? ")
           (let (dep-list)
             (maphash (lambda (k v)
                        (add-to-list 'dep-list (format "%s%s" k (if v (concat ": " v) ""))))
                      (gethash :deps conflicts))
             (string-join dep-list ", ")))))

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
  "Push the current Datomic Ion project.
Automatically deploy after a successful push if
`datomic-ion-auto-deploy' is non-nil."
  (interactive)
  (let* ((default-directory (project-root (project-current)))
         (name "ion push")
         (program "clojure")
         (args '("-M:ion-dev" "{:op :push}"))
         (finish-func
          (lambda (proc)
            (save-excursion
              (with-current-buffer
                  (process-buffer proc))
              (goto-char (point-max))
              (backward-sexp)
              (kill-region (point-min) (point))
              (let* ((edn (car (parseedn-read)))
                     (group (car (gethash :deploy-groups edn)))
                     (rev (gethash :rev edn))
                     (conflicts (gethash :dependency-conflicts edn)))
                (setq datomic-ion-deploy-group group)
                (setq datomic-ion-last-rev rev)
                (when (or datomic-ion-auto-deploy
                          (yes-or-no-p
                           (format "Deploy %s revision '%s' to group %s? "
                                   (project-name (project-current)) rev group)))
                  (unless (and conflicts (not (datomic--yes-or-no-conflicts-p conflicts)))
                    (datomic-ion-deploy group rev))))))))
    (message "%s: %s %s" name program (string-join args " "))
    (apply #'async-start-process name program finish-func args)))


(defun datomic-ion-deploy (&optional group rev)
  "Deploy the last pushed Datomic Ion of the current project.
Optional use system GROUP and revision REV for deployment."
  (interactive)
  (when (or (and group rev)
            (yes-or-no-p
             (format "Deploy %s revision '%s' to group %s? "
                     (project-name (project-current)) datomic-ion-last-rev datomic-ion-deploy-group)))
    (let* ((default-directory (project-root (project-current)))
           (name "ion deploy")
           (program "clojure")
           (args `("-M:ion-dev"
                   ,(format "{:op :deploy, :group %s, :rev \"%s\"}"
                            datomic-ion-deploy-group datomic-ion-last-rev)))
           (finish-func (lambda (proc)
                          (save-excursion
                            (with-current-buffer
                                (process-buffer proc))
                            (goto-char (point-max))
                            (backward-sexp)
                            (kill-region (point-min) (point))
                            (let* ((edn (car (parseedn-read)))
                                   (arn (gethash ':execution-arn edn)))
                              (setq datomic-ion-last-execution-arn arn)
                              (when datomic-ion-auto-check-status
                                (datomic-ion-status arn)))))))
      (message "%s: %s %s" name program (string-join args " "))
      (apply #'async-start-process name program finish-func args))))

(defun datomic-ion-status (&optional arn)
  "Get the status of the last Datomic Ion deploy.
Optionally specify execution ARN."
  (interactive)
  (let* ((default-directory (project-root (project-current)))
         (name "ion deploy")
         (program "clojure")
         (args `("-M:ion-dev"
                 ,(format "{:op :deploy-status, :execution-arn %s}"
                          (or arn datomic-ion-last-execution-arn))))
         (finish-func (lambda (proc)
                        (save-excursion
                          (with-current-buffer
                              (process-buffer proc))
                          (goto-char (point-max))
                          (backward-sexp)
                          (kill-region (point-min) (point))
                          (let* ((edn (car (parseedn-read)))
                                 (status (gethash ':deploy-status edn))
                                 (code-status (gethash ':code-deploy-status edn)))
                            (if (and (or (string= status "RUNNING") (string= code-status "RUNNING"))
                                     (or datomic-ion-auto-check-status
                                         (yes-or-no-p "Deploy still running.  Check status again?")))
                                (datomic-ion-status)
                              (message "Deploy status: %s - Code Deploy status: %s"
                                       status code-status)))))))
    (apply #'async-start-process name program finish-func args)))

(defun datomic-list-systems ()
  "List Datomic cloud systems."
  (interactive)
  (let* ((cmd (concat datomic-cli-command " cloud list-systems"))
         (out (shell-command-to-string cmd))
         (x (json-parse-string out)))
    (with-temp-buffer
      (insert "Datomic cloud systems:\n\n")
      (mapc (lambda (groups)
              (maphash (lambda (k v)
                         (insert (format "%25s: %s\n" k v)))
                       groups))
            x)
      (message (buffer-substring (point-min) (point-max))))))

(defun datomic-system-list-instances (system)
  "List EC2 instances for SYSTEM.

Displays the EC2 Instances, with their instance-id and current status,
that are utilized by the supplied system name."
  (interactive (list (datomic--read-system)))
  (let* ((cmd (concat datomic-cli-command " system list-instances " system))
         (out (shell-command-to-string cmd))
         (x (json-parse-string out)))
    (with-temp-buffer
      (insert "EC2 Instances for system '" system "'\n\n")
      (mapc (lambda (groups)
              (maphash (lambda (k v)
                         (insert (format "%25s: %s\n" k v)))
                       groups))
            x)
      (message (buffer-substring (point-min) (point-max))))))

(defun datomic-system-describe-groups (system)
  "List EC2 instances for SYSTEM.

Displays information about the compute groups of the supplied
Datomic Cloud system."
  (interactive (list (datomic--read-system)))
  (let* ((cmd (concat datomic-cli-command " system describe-groups " system))
         (out (shell-command-to-string cmd))
         (x (json-parse-string out)))
    (with-temp-buffer
      (insert "Information about the compute groups of system '" system "'\n\n")
      (mapc (lambda (groups)
              (maphash (lambda (k v)
                         (insert (format "%25s: %s\n" k v)))
                       groups))
            x)
      (message (buffer-substring (point-min) (point-max))))))

(provide 'datomic)
;;; datomic.el ends here
