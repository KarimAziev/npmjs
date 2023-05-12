;;; npmjs.el --- Command dispatcher for NPM -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>\

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/npmjs
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Command dispatcher for NPM

;;; Code:

(require 'transient)
(require 'compile)

(defcustom npmjs-common-buffer-name-function 'npmjs-common-create-unique-buffer-name
  "Buffer name for `npm' command, or function which return buffer name.
The function takes three arguments, ROOT, NPM-COMMAND, ARGS.
ROOT is project root directory.  NPM-COMMAND is npm command string.
ARGS is list of arguments passed to npm command.

You can use `npmjs-common-create-unique-buffer-name' to use unique buffer name
among all sesstions."
  :group 'yarn
  :type '(choice
          (string :tag "Use same buffer through all sessions")
          (const :tag "Use unique buffer name among all sessions"
                 npmjs-common-create-unique-buffer-name)
          function))

;; nvm
(defcustom npmjs-nvm-dir (or (getenv "NVM_DIR")
                            (expand-file-name "~/.nvm"))
  "Full path to Nvm installation directory."
  :group 'nvm
  :type 'directory)

(defvar-local npmjs--current-command nil)
(defvar-local npmjs-arguments nil)
(defvar-local npmjs--current-node-version nil)

;;;###autoload
(defun npmjs-exec-with-args (command &rest args)
  "Run a shell COMMAND with ARGS."
  (let ((cmdline (mapconcat (lambda (it)
                              (if (string-match-p "\s\t\n" it)
                                  (shell-quote-argument it)
                                it))
                            (append (list command)
                                    (flatten-list args))
                            "\s")))
    (with-temp-buffer
      (shell-command cmdline (current-buffer))
      (let ((output (string-trim (buffer-string))))
        (unless (string-empty-p output)
          output)))))


;;;###autoload
(defun npmjs-install-nvm ()
  "Install nvm."
  (interactive)
  (when (yes-or-no-p "Download and install nvm?")
    (message
     "npmjs: Loading https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh")
    (let ((script
           (with-current-buffer
               (url-retrieve-synchronously
                "https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh"
                'silent 'inhibit-cookies)
             (goto-char (point-min))
             (re-search-forward "^$" nil 'move)
             (forward-char 1)
             (delete-region (point-min)
                            (point))
             (buffer-string))))
      (message "Installing nvm-sh: %s"
               (with-temp-buffer
                 (insert script)
                 (shell-command-on-region (point-min)
                                          (point-max)
                                          "bash")))
      (npmjs-exec-in-dir (string-join
                          (list "source" (npmjs-nvm-path) "&&" "nvm"
                                "install"
                                "node")
                          "\s")))))


;;;###autoload
(defun npmjs-ensure-nvm-install ()
  "Install nvm, node and yarn."
  (interactive)
  (unless (npmjs-nvm-path)
    (npmjs-install-nvm)))

(defun npmjs-nvm-path ()
  "Return path to NVM_DIR if exists."
  (when-let* ((nvm-dir (or (getenv "NVM_DIR")
                           (when (file-exists-p "~/.nvm/")
                             "~/.nvm/")))
              (file (expand-file-name "nvm.sh" nvm-dir)))
    (when (file-exists-p file)
      file)))
(defconst npmjs-nvm-version-re
  "v[0-9]+\.[0-9]+\.[0-9]+"
  "Regex matching a Node version.")

(defvar npmjs-nvm-remote-node-versions-alist nil)

(defun npmjs-nvm-ls-remote ()
  "Exec $NVM_DIR/nvm.sh and run nvm ls-remote command.
Return alist of node versions and aliases."
  (when-let* ((nvm-path (npmjs-nvm-path))
              (node-versions
               (npmjs-exec-with-args "source" nvm-path
                                     "&&" "nvm"
                                     "ls-remote"
                                     "--no-colors")))
    (nreverse
     (mapcar
      (lambda (it)
        (let* ((parts (delete "*" (split-string it nil t)))
               (found (seq-find
                       (apply-partially
                        #'string-match-p
                        npmjs-nvm-version-re)
                       parts))
               (meta (string-join (seq-drop (member found parts) 1) "\s")))
          (cons found meta)))
      (split-string
       node-versions
       "[\n]" t)))))

(defun npmjs-nvm-read-remote-node-version ()
  "Install node VERSION with nvm."
  (when-let ((node-versions
              (or npmjs-nvm-remote-node-versions-alist
                  (setq npmjs-nvm-remote-node-versions-alist
                        (npmjs-nvm-ls-remote)))))
    (let* ((installed (mapcar #'car (npmjs-nvm--installed-versions)))
           (default-global (npmjs-nvm-strip-prefix
                            (npmjs-current-default-node-version)))
           (current-global
            (when (get-buffer (npmjs--get-global-buffer-name))
              (npmjs-nvm-strip-prefix
               (buffer-local-value
                'npmjs--current-node-version
                (get-buffer
                 (npmjs--get-global-buffer-name))))))
           (project-node
            (npmjs-nvm-strip-prefix
             (npmjs-nvm-get-nvmrc-required-node-version)))
           (curr-node (npmjs-nvm-strip-prefix
                       npmjs--current-node-version))
           (items (mapcar (lambda (it)
                            (seq-copy (car it)))
                          node-versions))
           (annotf
            (lambda (item)
              (let ((it (npmjs-nvm-strip-prefix item)))
                (let ((status
                       (cond ((equal it default-global)
                              "(System global)")
                             ((equal it current-global)
                              "(Current global)")
                             ((equal it project-node)
                              "(nvmrc) ")
                             ((equal it curr-node)
                              "(Buffer local node) ")))
                      (install-status (when (member it
                                                    installed)
                                        " (installed)"))
                      (meta (cdr (assoc it node-versions))))
                  (concat " "
                          (string-join (delete nil (list status install-status
                                                         meta))
                                       "\s")))))))
      (completing-read "Version: "
                       (lambda (str pred action)
                         (if (eq action 'metadata)
                             `(metadata
                               (annotation-function . ,annotf))
                           (complete-with-action action
                                                 items
                                                 str pred)))))))

(defun npmjs-nvm-strip-prefix (version)
  "Stip prefix from VERSION, e.g. v1.0.0 to 1.0.0."
  (if (and version (string-match-p npmjs-nvm-version-re version))
      (substring-no-properties version 1)
    version))

;;;###autoload
(defun npmjs-nvm-install-node-version ()
  "Install new node VERSION with CALLBACK."
  (interactive)
  (when-let ((nvm-path (npmjs-nvm-path))
             (version (npmjs-nvm-strip-prefix
                       (npmjs-nvm-read-remote-node-version))))
    (if (member version (mapcar #'car (npmjs-nvm--installed-versions)))
        (message "This version is installed")
      (npmjs-exec-in-dir (read-string "Run?" (string-join
                                              (list "source" nvm-path "&&"
                                                    "nvm"
                                                    "install"
                                                    version
                                                    "--reinstall-packages-from=current")
                                              "\s"))
                         default-directory
                         (lambda (&rest _)
                           (npmjs-run-as-comint "node -v" (npmjs-nvm-get-env-for-node
                                                           version)))))))

(defun npmjs-expand-when-exists (filename &optional directory)
  "Expand FILENAME to DIRECTORY and return result if exists."
  (when-let ((file (expand-file-name filename directory)))
    (when (file-exists-p file)
      file)))


(defun npmjs-nvm--installed-versions-dirs ()
  "Return list of directories with installed versions of node."
  (let* ((files (mapcan
                 (lambda (versions-dir)
                   (directory-files versions-dir t
                                    directory-files-no-dot-files-regexp))
                 (delq nil
                       (append
                        (list (npmjs-expand-when-exists
                               npmjs-nvm-dir))
                        (list (npmjs-expand-when-exists
                               "versions"
                               npmjs-nvm-dir)))))))
    (mapcan
     (lambda (it)
       (when-let ((name (when (file-directory-p it)
                          (file-name-nondirectory
                           (directory-file-name
                            it)))))
         (if (string-match-p (concat npmjs-nvm-version-re "$")
                             name)
             (list it)
           (directory-files it t npmjs-nvm-version-re))))
     files)))

;;;###autoload
(defun npmjs-nvm-jump-to-installed-node ()
  "Read and jump to installed node version."
  (interactive)
  (let ((cell (npmjs-nvm-read-installed-node-versions)))
    (if (and (cdr cell)
             (file-exists-p (cdr cell))
             (find-file (cdr cell)))
        (user-error "Not found %s" cell))))

;;;###autoload
(defun npmjs-nvm-use-other-version ()
  "Read and jump to installed node version."
  (interactive)
  (let ((cell (npmjs-nvm-read-installed-node-versions)))
    (if (and (cdr cell)
             (file-exists-p (cdr cell))
             (find-file (cdr cell)))
        (user-error "Not found %s" cell))))

(defun npmjs-nvm--installed-versions ()
  "Return list of directories with installed versions of node."
  (mapcar (lambda (it)
            (cons (file-name-nondirectory it) it))
          (npmjs-nvm--installed-versions-dirs)))

(defun npmjs-nvm--version-from-string (version-string)
  "Split a VERSION-STRING into a list of (major, minor, patch) numbers."
  (mapcar #'string-to-number (split-string version-string "[^0-9]" t)))

(defun npmjs-nvm--version-match-p (matcher version)
  "Does this VERSION satisfy the requirements in MATCHER?"
  (or (eq (car matcher) nil)
      (and (eq (car matcher)
               (car version))
           (npmjs-nvm--version-match-p (cdr matcher)
                                      (cdr version)))))
(defun npmjs-nvm-version-compare (a b)
  "Comparator for sorting NVM versions, return t if A < B."
  (if (eq (car a)
          (car b))
      (npmjs-nvm-version-compare (cdr a)
                                (cdr b))
    (< (car a)
       (car b))))

(defun npmjs-nvm-find-exact-version-for (short)
  "Find most suitable version for SHORT.

SHORT is a string containing major and optionally minor version.
This function will return the most recent version whose major
and (if supplied, minor) match."
  (when (and short
             (string-match-p "v?[0-9]+\\(\.[0-9]+\\(\.[0-9]+\\)?\\)?$" short))
    (unless (or (string-prefix-p "v" short)
                (string-prefix-p "node" short)
                (string-prefix-p "iojs" short))
      (setq short (concat "v" short)))
    (let* ((versions (npmjs-nvm--installed-versions))
           (requested (npmjs-nvm--version-from-string short))
           (first-version
            (seq-find (lambda (it)
                        (string= (car it) short))
                      versions)))
      (or
       first-version
       (let ((possible-versions
              (seq-filter
               (lambda (version)
                 (npmjs-nvm--version-match-p
                  requested
                  (npmjs-nvm--version-from-string (car version))))
               versions)))
         (if (eq possible-versions nil)
             nil
           (car (sort possible-versions
                      (lambda (a b)
                        (not (npmjs-nvm-version-compare
                              (npmjs-nvm--version-from-string (car a))
                              (npmjs-nvm--version-from-string (car b)))))))))))))

(defun npmjs-nvm-get-nvmrc-required-node-version ()
  "Lookup and read .nvmrc file."
  (when-let ((nvmrc (locate-dominating-file default-directory ".nvmrc")))
    (with-temp-buffer (insert-file-contents
                       (expand-file-name ".nvmrc" nvmrc))
                      (string-trim
                       (buffer-substring-no-properties (point-min)
                                                       (point-max))))))

(defun npmjs-nvm-read-installed-node-versions ()
  "Read installed with nvm node versions.
Return a cons cell with version and absolute path."
  (let ((alist (npmjs-nvm--installed-versions))
        (default-version (string-trim (shell-command-to-string "node -v")))
        (choice))
    (setq choice (completing-read "Version: "
                                  (lambda (str pred action)
                                    (if (eq action 'metadata)
                                        `(metadata
                                          (annotation-function .
                                                               (lambda
                                                                 (it)
                                                                 (when
                                                                     (equal
                                                                      it
                                                                      ,default-version)
                                                                   " (default)"))))
                                      (complete-with-action action
                                                            alist
                                                            str pred)))
                                  nil t))
    (cons choice (cdr (assoc choice alist)))))

(defun npmjs-nvm-get-env-for-node (version)
  "Return alist of new environment for node VERSION."
  (when-let* ((version-path (cdr (npmjs-nvm-find-exact-version-for version))))
    (let* ((env-flags
            (mapcar
             (lambda (it)
               (cons
                (car it)
                (concat version-path "/" (cdr it))))
             '(("NVM_BIN" . "bin")
               ("NVM_PATH" . "lib/node")
               ("NVM_INC" . "include/node"))))
           (path-re (concat "^"
                            (concat (or (getenv "NVM_DIR")
                                        (expand-file-name "~/.nvm"))
                                    "/\\(?:versions/node/\\|versions/io.js/\\)?")
                            "v[0-9]+\.[0-9]+\.[0-9]+" "/bin/?$"))
           (new-bin-path (expand-file-name  "bin/" version-path))
           (paths
            (cons
             new-bin-path
             (seq-remove
              (lambda (path)
                (if path (string-match-p path-re path) t))
              (parse-colon-path (getenv "PATH")))))
           (new-path (cons "PATH" (string-join paths path-separator)))
           (flags (append env-flags (list new-path)))
           (regexp (mapconcat #'identity (mapcar #'car flags) "\\|")))
      (append (mapcar (lambda (it)
                        (concat (car it) "=" (cdr it)))
                      flags)
              (seq-remove (apply-partially #'string-match-p regexp)
                          process-environment)))))

(defun npmjs-get-node-env ()
  "Lookup and read .nvmrc file."
  (when-let ((version (npmjs-nvm-get-nvmrc-required-node-version)))
    (if-let ((env (npmjs-nvm-get-env-for-node version)))
        env
      (message "Mismatched nvm version")
      nil)))


(defvar npmjs-json-hash (make-hash-table :test 'equal))

(defun npmjs-get-package-json-path ()
  "Look up directory hierarchy for directory containing package.json.
Return absolute path to package.json or nil."
  (when-let ((project-root (npmjs-get-project-root)))
    (expand-file-name "package.json"
                      project-root)))

(defun npmjs-read-json (file &optional json-type)
  "Read the JSON object in FILE, return object converted to JSON-TYPE.
JSON-TYPE must be one of `alist', `plist', or `hash-table'."
  (require 'json)
  (condition-case nil
      (let* ((json-object-type (or json-type 'alist))
             (cache (gethash (format "%s:%s" file json-object-type)
                             npmjs-json-hash))
             (cache-tick (and cache (plist-get cache :tick)))
             (tick (file-attribute-modification-time (file-attributes
                                                      file
                                                      'string)))
             (content-json))
        (when (or (null cache)
                  (not (equal tick cache-tick)))
          (setq content-json
                (with-temp-buffer
                  (insert-file-contents file)
                  (when (fboundp 'json-read-from-string)
                    (json-read-from-string
                     (buffer-substring-no-properties (point-min)
                                                     (point-max))))))
          (setq cache (list
                       :tick tick
                       :json content-json))
          (puthash file cache npmjs-json-hash))
        (plist-get cache :json))
    (error (message "Could't read %s as json."
                    file))))



(defun npmjs-online-p ()
  "Check internet connection and return non-nil if so."
  (if (fboundp 'network-interface-list)
      (seq-some (lambda (iface)
                  (unless (equal "lo" (car iface))
                    (member 'up (car (last (network-interface-info
                                            (car iface)))))))
                (network-interface-list))
    t))

(defun npmjs-exec-in-dir (command &optional directory callback)
  "Execute COMMAND in PROJECT-DIR.
If DIRECTORY doesn't exists, create new.
Invoke CALLBACK without args."
  (let ((proc)
        (buffer (generate-new-buffer
                 (format "*%s*" (car (split-string command
                                                   nil t))))))
    (progn
      (switch-to-buffer buffer)
      (with-current-buffer buffer
        (when directory
          (setq directory (file-name-as-directory (expand-file-name
                                                   directory)))
          (if (file-exists-p directory)
              (setq default-directory directory)
            (mkdir directory)
            (setq default-directory directory)))
        (setq proc (start-process-shell-command
                    (nth 0
                         (split-string command))
                    buffer command))
        (shell-command-save-pos-or-erase)
        (require 'shell)
        (when (fboundp 'shell-mode)
          (shell-mode))
        (unless (get-buffer-window buffer)
          (pop-to-buffer buffer)))
      (set-process-sentinel
       proc
       (lambda (process _state)
         (let ((output (when-let ((buffer (process-buffer process)))
                         (with-current-buffer
                             (process-buffer process)
                           (buffer-string)))))
           ;; (kill-buffer (process-buffer process))
           (if (= (process-exit-status process) 0)
               (progn
                 (message "finished")
                 (when callback
                   (funcall callback output))
                 (when (bufferp (process-buffer process))
                   (kill-buffer (process-buffer process))))
             (user-error (format "%s\n%s" command output))))))
      (require 'comint)
      (when (fboundp 'comint-output-filter)
        (set-process-filter proc #'comint-output-filter)))))



(defun npmjs-common-create-unique-buffer-name (root npm-command)
  "Create buffer name unique to ROOT and NPM-COMMAND."
  (concat "*" npm-command " in " root "*"))

(defun npmjs-common--generate-buffer-name-function (root npm-command)
  "Generate function which return buffer name to pass `compilation-start'.
ROOT is project root directory.  NPM-COMMAND is npm command string.
ARGS is list of arguments passed to npm command.

This function uses `npmjs-common-buffer-name-function'."
  (if (stringp npmjs-common-buffer-name-function)
      npmjs-common-buffer-name-function
    (funcall npmjs-common-buffer-name-function
             root npm-command)))


(defun npmjs-get-project-root ()
  "Look up directory hierarchy for directory containing package.json.
Return full path of containing directory or nil."
  (locate-dominating-file
   default-directory
   "package.json"))

(defun npmjs-get-non-project-root ()
  "Look up directory hierarchy for first directory without package.json.
Return full path of containing directory or nil."
  (let ((dir (or (locate-dominating-file default-directory
                                         "package.json")
                 default-directory)))
    (while
        (locate-dominating-file dir
                                "package.json")
      (setq dir (file-name-parent-directory dir)))
    dir))

(defvar npmjs-history-dependencies nil)
(defun npmjs-current-default-node-version ()
  "Return current default node version."
  (let ((default-directory (expand-file-name "~/")))
    (npmjs-nvm-strip-prefix
     (string-trim
      (shell-command-to-string
       "node -v")))))

(defun npmjs-get-current-global-version ()
  "Return `npmjs--current-node-version' either from project or globally."
  (or
   npmjs--current-node-version
   (when-let ((buff (get-buffer (or
                                 (npmjs--get-project-buffer-name)
                                 (npmjs--get-global-buffer-name)))))
     (buffer-local-value 'npmjs--current-node-version buff))))

(defun npmjs-get-current-node-version ()
  "Return `npmjs--current-node-version' either from project or globally."
  (or
   npmjs--current-node-version
   (when-let ((buff (get-buffer (or
                                 (npmjs--get-project-buffer-name)
                                 (npmjs--get-global-buffer-name)))))
     (buffer-local-value 'npmjs--current-node-version buff))))

(defun npmjs-confirm-node-version ()
  "Confirm which node version to use."
  (let* ((default-global (npmjs-current-default-node-version))
         (current-global
          (when (get-buffer (npmjs--get-global-buffer-name))
            (buffer-local-value 'npmjs--current-node-version
                                (get-buffer
                                 (npmjs--get-global-buffer-name)))))
         (project-node
          (npmjs-nvm-get-nvmrc-required-node-version))
         (curr-node npmjs--current-node-version)
         (cands
          (seq-uniq
           (mapcar #'npmjs-nvm-strip-prefix
                   (delq nil
                         (list
                          default-global
                          current-global
                          project-node
                          curr-node)))))
         (annotf (lambda (it)
                   (cond ((equal it default-global)
                          " System Global")
                         ((equal it current-global)
                          " Current global")
                         ((equal it project-node)
                          " (nvmrc) ")
                         ((equal it curr-node)
                          " Buffer local node ")))))
    (if (<= (length cands) 1)
        (car cands)
      (completing-read "Which node to use?"
                       (lambda (str pred action)
                         (if (eq action 'metadata)
                             `(metadata
                               (annotation-function .
                                                    ,annotf))
                           (complete-with-action action
                                                 cands
                                                 str pred)))))))



(defun npmjs-compile-global (npm-command)
  "Generic compile command for NPM-COMMAND with ARGS functionality."
  (if-let ((nvm-version (npmjs-confirm-node-version)))
      (let ((env (npmjs-nvm-get-env-for-node
                  nvm-version))
            (nvm-path (npmjs-nvm-path)))
        (if (and (not env)
                 (yes-or-no-p
                  (format
                   "Install node %s?"
                   nvm-version)))
            (npmjs-exec-in-dir (string-join
                                (list "source" nvm-path "&&" "nvm"
                                      "install"
                                      (npmjs-nvm-strip-prefix nvm-version)
                                      "--reinstall-packages-from=current")
                                "\s")
                               default-directory
                               (lambda (&rest _)
                                 (npmjs-run-as-comint
                                  npm-command
                                  (npmjs-nvm-get-env-for-node
                                   nvm-version))))
          (npmjs-run-as-comint
           npm-command
           (or env process-environment))))
    (npmjs-run-as-comint npm-command process-environment)))


(defun npmjs-compile (npm-command &rest args)
  "Generic compile command for NPM-COMMAND with ARGS functionality."
  (when args
    (setq npm-command (concat npm-command
                              " "
                              (mapconcat (lambda (i)
                                           (unless (stringp i)
                                             (setq i (format "%s" i)))
                                           (string-trim i))
                                         (delq nil (flatten-list args))))))
  (if-let ((nvm-version (npmjs-nvm-get-nvmrc-required-node-version)))
      (let ((env (npmjs-nvm-get-env-for-node
                  nvm-version))
            (nvm-path (npmjs-nvm-path)))
        (if (and (not env)
                 (yes-or-no-p
                  (format
                   "This project requires node %s, which is not installed. Install?"
                   nvm-version)))
            (npmjs-exec-in-dir (string-join
                                (list "source" nvm-path "&&" "nvm"
                                      "install"
                                      (npmjs-nvm-strip-prefix nvm-version)
                                      "--reinstall-packages-from=current")
                                "\s")
                               default-directory
                               (lambda (&rest _)
                                 (npmjs-run-as-comint
                                  npm-command
                                  (npmjs-nvm-get-env-for-node
                                   nvm-version))))
          (npmjs-run-as-comint
           npm-command
           (or env process-environment))))
    (npmjs-run-as-comint npm-command process-environment)))



(defcustom npmjs-started-hook nil
  "Hooks to run after a npmjs process starts."
  :group 'npmjs
  :type 'hook)

(defcustom npmjs-finished-hook nil
  "Hooks to run after a npmjs process finishes."
  :group 'npmjs
  :type 'hook)

(defcustom npmjs-setup-hook nil
  "Hooks to run before a npmjs process starts."
  :group 'npmjs
  :type 'hook)

(defun npmjs-run-as-comint (command &optional env)
  "Run a npmjs comint session for COMMAND with ARGS and ENV."
  (let* ((buffer (npmjs--get-buffer))
         (process (get-buffer-process buffer)))
    (with-current-buffer buffer
      (when (comint-check-proc buffer)
        (unless (or compilation-always-kill
                    (yes-or-no-p "Kill running npmjs process?"))
          (user-error "Aborting; npmjs still running")))
      (when process
        (delete-process process))
      (erase-buffer)
      (setq-local process-environment (or env process-environment))
      (unless (eq major-mode 'npmjs-mode)
        (npmjs-mode))
      (compilation-forget-errors)
      (insert (format "cwd: %s\ncmd: %s\n\n" default-directory command))
      (setq npmjs--current-command command)
      (run-hooks 'npmjs-setup-hook)
      (let ((process-environment (or env process-environment)))
        (make-comint-in-buffer "npmjs" buffer "sh" nil "-c" command)
        (setq npmjs--current-node-version
              (npmjs-nvm-strip-prefix
               (string-trim
                (shell-command-to-string
                 "node -v"))))
        (run-hooks 'npmjs-started-hook)
        (setq process (get-buffer-process buffer))
        (set-process-sentinel process #'npmjs--process-sentinel)
        (display-buffer buffer)))))

(defun npmjs-run-compile (npm-command &optional env)
  "Run compile command for NPM-COMMAND in ENV."
  (let ((compenv (or env process-environment)))
    (let* ((command npm-command)
           (compilation-read-command nil)
           (compilation-environment compenv)
           (compile-command command)
           (compilation-buffer-name-function
            (lambda (_mode)
              (npmjs--get-buffer))))
      (compilation-start (concat "node -v && " command) t))))

(define-derived-mode npmjs-mode
  comint-mode "npmjs"
  "Major mode for npmjs sessions (derived from `comint-mode')."
  (setq-local comint-prompt-read-only nil)
  (compilation-setup t))

(defun npmjs-project-name ()
  "Return name from current project package.json."
  (alist-get 'name (npmjs-get-package-json-alist)))


(defun npmjs--get-global-buffer-name ()
  "Get a create a suitable compilation buffer."
  (format "npmjs<global>"))

(defun npmjs--get-project-buffer-name ()
  "Get a create a suitable compilation buffer."
  (when-let ((name (npmjs-project-name)))
    (format "npmjs<%s>" name)))

(defun npmjs--get-buffer ()
  "Get a create a suitable compilation buffer."
  (if (eq major-mode 'npmjs-mode)
      (current-buffer)
    (get-buffer-create
     (or (npmjs--get-project-buffer-name)
         (npmjs--get-global-buffer-name)))))

(defun npmjs--process-sentinel (proc _state)
  "Process sentinel helper to run hooks after PROC finishes."
  (with-current-buffer (process-buffer proc)
    (run-hooks 'npmjs-finished-hook)))

;;;###autoload
(defun npmjs-read-new-dependency (&optional initial-input)
  "Call the npm search shell command.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (require 'ivy nil t)
  (require 'counsel nil t)
  (if (and (fboundp 'ivy-more-chars)
           (fboundp 'counsel--async-command)
           (fboundp 'ivy-read)
           (fboundp 'counsel-delete-process))
      (let ((result)
            (dependencies))
        (setq result (ivy-read
                      "Repo:\s"
                      (lambda (str)
                        (or
                         (ivy-more-chars)
                         (progn
                           (counsel--async-command (concat
                                                    "npm search --parseable "
                                                    str))
                           '("" "working..."))))
                      :initial-input initial-input
                      :dynamic-collection t
                      :history 'npmjs-history-dependencies
                      :multi-action
                      (lambda (marked)
                        (setq dependencies
                              (append marked dependencies)))
                      :action (lambda (d) d)
                      :unwind #'counsel-delete-process
                      :caller 'npmjs-read-new-dependency))
        (if dependencies
            (string-join (mapcar (lambda (d)
                                   (car (split-string d)))
                                 dependencies)
                         "\s")
          (car (split-string result nil t))))
    (read-string "Dependency: ")))

(defvar npmjs-bmenu-last-search-regexp nil)
(defvar npmjs-bmenu-search-buff-name "*npm search*")
(defvar npmjs-bmenu-search-output nil)

(defun npmjs--alist-props (props alist)
  "Pick PROPS from ALIST."
  (mapcar (lambda (k)
            (alist-get k alist))
          props))

(defvar npmjs--marked-packages nil)
(defvar-local npmjs-search-highlight-ov nil
  "An overlay for `npmjs-search-mode'.")

(defun npmjs-search-highlight-current ()
  "Highlight current line."
  (let ((ov (seq-find (lambda (o)
                        (overlay-get o 'npmjs))
                      (overlays-at (point)))))
    (unless ov
      (if npmjs-search-highlight-ov
          (move-overlay npmjs-search-highlight-ov (line-beginning-position)
                        (line-end-position))
        (setq npmjs-search-highlight-ov (make-overlay (line-beginning-position)
                                                      (line-end-position)))
        (overlay-put npmjs-search-highlight-ov 'face 'success)
        (overlay-put npmjs-search-highlight-ov 'npmjs t)))))



(defun npmjs-search-parseable-to-rows (output)
  "Map OUTPUT to rows."
  (let ((rows (split-string output "[\n\r\f]" t)))
    (delq nil
          (mapcar
           (lambda (it)
             (let* ((parts (split-string it "[\t]"))
                    (name (car parts)))
               (list name
                     (apply #'vector
                            (list (or (car parts) name)
                                  (or (nth 1 parts) "")
                                  (or (nth 4 parts) ""))))))
           rows))))

(defun npmjs-table--revert (&rest _)
  "Revert table."
  (setq tabulated-list-entries (npmjs-search-parseable-to-rows
                                npmjs-bmenu-search-output))
  (tabulated-list-print t)
  (npmjs-search-highlight-current))

(declare-function json-read-from-string  "json")

(defun npmjs-search-package-by-regexp (regexp &optional args)
  "Perfoms npm search with REGEXP and ARGS."
  (require 'json)
  (unless (string-empty-p (string-trim regexp))
    (setq npmjs-bmenu-search-output
          (shell-command-to-string
           (concat
            "npm search "
            regexp
            (or (if (listp args)
                    (string-join args " ") args)
                "")
            " -p" (if (npmjs-online-p) "" " --offline"))))
    (let ((buff (get-buffer-create npmjs-bmenu-search-buff-name)))
      (with-current-buffer buff
        (if (eq major-mode 'npmjs-search-mode)
            (npmjs-table--revert)
          (npmjs-search-mode)
          (setq tabulated-list-entries (npmjs-search-parseable-to-rows
                                        npmjs-bmenu-search-output))
          (tabulated-list-print t)
          (npmjs-search-highlight-current))
        (unless (get-buffer-window buff)
          (select-window
           (let ((ignore-window-parameters t))
             (split-window
              (frame-root-window) -1 'below))
           'norecord)
          (pop-to-buffer-same-window buff))
        (fit-window-to-buffer nil 28 28 nil nil t)))))

(defun npmjs-search-package--forward-line-0 (&optional n)
  "Forward N lines in buffer `npmjs-bmenu-search-buff-name'."
  (let* ((id (tabulated-list-get-id))
         (new-id (progn (forward-line n)
                        (tabulated-list-get-id))))
    (cond ((not new-id)
           (if (and n (< n 0))
               (progn (goto-char (point-max))
                      (forward-line -1))
             (goto-char (point-min))))
          ((equal new-id id)
           (if (and n (< n 0))
               (progn (goto-char (point-max))
                      (forward-line -1))
             (goto-char (point-min)))))
    (npmjs-search-highlight-current)))

(defun npmjs-search-package--forward-line (&optional n)
  "Forward N lines in buffer `npmjs-bmenu-search-buff-name'."
  (if (eq npmjs-bmenu-search-buff-name (buffer-name (current-buffer)))
      (npmjs-search-package--forward-line-0 n)
    (when-let ((buff (get-buffer npmjs-bmenu-search-buff-name)))
      (with-current-buffer buff
        (npmjs-search-package--forward-line-0 n)))))


(defun npmjs-search-package--next-line ()
  "Forward line in buffer `npmjs-bmenu-search-buff-name'."
  (interactive)
  (npmjs-search-package--forward-line 1))


(defun npmjs-search-package--prev-line ()
  "Previous line in buffer `npmjs-bmenu-search-buff-name'."
  (interactive)
  (npmjs-search-package--forward-line -1))


(defun npmjs-search-package--beg-of-buffer ()
  "Previous line in buffer `npmjs-bmenu-search-buff-name'."
  (interactive)
  (when-let ((wind (get-buffer-window npmjs-bmenu-search-buff-name)))
    (with-selected-window wind
      (goto-char (point-min))
      (unless (tabulated-list-get-id)
        (forward-line 1)
        (npmjs-search-highlight-current)))))


(defun npmjs-search-package--end-of-buffer ()
  "Previous line in buffer `npmjs-bmenu-search-buff-name'."
  (interactive)
  (if (eq npmjs-bmenu-search-buff-name (buffer-name (current-buffer)))
      (progn
        (goto-char (point-max))
        (if (tabulated-list-get-id)
            (npmjs-search-highlight-current)
          (forward-line -1)
          (npmjs-search-highlight-current)))
    (when-let ((buff (get-buffer npmjs-bmenu-search-buff-name)))
      (with-current-buffer buff
        (progn
          (goto-char (point-max))
          (if (tabulated-list-get-id)
              (npmjs-search-highlight-current)
            (forward-line -1)
            (npmjs-search-highlight-current)))))))

;;;###autoload
(defun npmjs-search-package-mark-or-unmark ()
  "Previous line in buffer `npmjs-bmenu-search-buff-name'."
  (interactive)
  (if (eq npmjs-bmenu-search-buff-name (buffer-name (current-buffer)))
      (npmjs--mark-or-unmark)
    (when-let ((buff (get-buffer npmjs-bmenu-search-buff-name)))
      (with-current-buffer buff
        (npmjs--mark-or-unmark)))))

(defun npmjs--mark-or-unmark ()
  "Clear any mark on a gist and move to the next line."
  (when-let ((name (tabulated-list-get-id (point))))
    (setq npmjs--marked-packages
          (if (member name npmjs--marked-packages)
              (delete name npmjs--marked-packages)
            (append npmjs--marked-packages (list name))))
    (let ((marked npmjs--marked-packages))
      (when (active-minibuffer-window)
        (select-window (active-minibuffer-window))
        (delete-minibuffer-contents)
        (when marked
          (insert (string-join marked ",")
                  (if marked "," "")))))))

;;;###autoload
(defun npmjs-search-package (&optional args)
  "Incremental search of npm packages with ARGS."
  (interactive)
  (let ((timer nil))
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (when (minibufferp)
                (setq
                 timer
                 (run-with-idle-timer
                  0.2 'repeat
                  (lambda (buf)
                    (when-let
                        ((input
                          (with-current-buffer buf
                            (use-local-map
                             (let ((map (make-sparse-keymap)))
                               (define-key map [remap next-line]
                                           'npmjs-search-package--next-line)
                               (define-key map [remap
                                                previous-line]
                                           'npmjs-search-package--prev-line)
                               (define-key map [remap
                                                set-mark-command]
                                           'npmjs-search-package-mark-or-unmark)
                               (define-key map [remap
                                                scroll-down-command]
                                           'npmjs-search-package--beg-of-buffer)
                               (define-key map [remap
                                                scroll-up-command]
                                           'npmjs-search-package--end-of-buffer)
                               (set-keymap-parent map
                                                  (current-local-map))
                               map))
                            (ignore-errors (car
                                            (last
                                             (seq-difference
                                              (split-string
                                               (minibuffer-contents-no-properties)
                                               "," t)
                                              npmjs--marked-packages)))))))
                      (npmjs-search-package-by-regexp
                       input
                       args)
                      (select-window (active-minibuffer-window))))
                  (current-buffer)))))
          (read-string "Package: ")
          (when timer (cancel-timer timer)
                (setq timer nil))
          (when-let* ((buff (get-buffer npmjs-bmenu-search-buff-name))
                      (result (with-current-buffer buff
                                (let ((result (or npmjs--marked-packages
                                                  (tabulated-list-get-id))))
                                  (setq npmjs--marked-packages nil)
                                  result))))
            (print result)
            result))
      (when timer
        (cancel-timer timer))
      (setq npmjs--marked-packages nil))))

(define-derived-mode npmjs-search-mode tabulated-list-mode
  "Show report gathered about unused definitions."
  (setq-local tabulated-list-format
              [("Name" 40 nil)
               ("Description" 50 nil)
               ("Version" 5 nil)])
  (add-hook 'tabulated-list-revert-hook #'npmjs-table--revert nil t)
  (add-hook 'post-command-hook #'npmjs-search-highlight-current nil t)
  (setq cursor-type nil)
  (setq tabulated-list-padding 2)
  (setq revert-buffer-function 'npmjs-table--revert)
  (tabulated-list-init-header))


(defun npmjs--menu-mark-delete (&optional _num)
  "Mark a gist for deletion and move to the next line."
  (interactive "p")
  (tabulated-list-put-tag "D" t))


(defun npmjs--menu-mark-upgrade (&optional _num)
  "Mark a gist for clone and move to the next line."
  (interactive "p")
  (tabulated-list-put-tag "U" t))


(defun npmjs--menu-mark-unmark (&optional _num)
  "Clear any mark on a gist and move to the next line."
  (interactive "p")
  (tabulated-list-put-tag " " t))


(defvar npmjs-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "D" 'npmjs--menu-mark-delete)
    (define-key map "m" 'npmjs--menu-mark-upgrade)
    (define-key map "u" 'npmjs--menu-mark-unmark)
    map))


(define-derived-mode npmjs-list-mode tabulated-list-mode
  "Show report gathered about unused definitions."
  (setq-local tabulated-list-format
              [("Name" 40 nil)
               ("Version" 10 nil)
               ("Description" 33 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun npmjs-get-package-json-alist ()
  "Return list of globally installed packages."
  (when-let ((package-json-file (npmjs-get-package-json-path)))
    (ignore-errors (npmjs-read-json package-json-file
                                    'alist))))

(defun npmjs-global-packages ()
  "Return list of globally installed packages."
  (require 'json nil t)
  (if (fboundp 'json-read-from-string)
      (json-read-from-string (with-temp-buffer
                               (shell-command
                                "npm ls --global --json"
                                (current-buffer))
                               (let ((output (string-trim
                                              (buffer-string))))
                                 (unless (string-empty-p output)
                                   (string-trim (buffer-string))))))
    '()))

(defun npmjs-read-local-packages ()
  "Read globally installed js packages."
  (let* ((default-directory
          (expand-file-name (or
                             (npmjs-get-project-root)
                             (vc-root-dir)
                             default-directory)))
         (process-environment (or (npmjs-get-node-env) process-environment))
         (alist (npmjs-local-packages))
         (prompt (format "Locally intstalled packages (%s): "
                         (string-trim (shell-command-to-string
                                       "node -v"))))
         (annotf (lambda (it)
                   (let ((value (cdr-safe (assoc it alist))))
                     (concat "@" (if (listp value)
                                     (string-join value " ")
                                   (or value "")))))))
    (completing-read prompt
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (annotation-function . ,annotf))
                         (complete-with-action action alist
                                               str pred))))))

(defun npmjs-local-packages ()
  "Return list of globally installed packages."
  (require 'json nil t)
  (if-let* ((package-json-file (npmjs-get-package-json-path))
            (package-json (ignore-errors (npmjs-read-json package-json-file
                                                          'alist))))
      (npmjs-pluck-depenencies package-json)
    '()))

(defun npmjs-alist-to-entries (alist)
  "Transfrom ALIST to tabulated entries."
  (mapcar (lambda (a)
            (let
                ((row
                  (cond ((consp (car-safe a))
                         (mapcar (lambda (it)
                                   (or it ""))
                                 (npmjs--alist-props
                                  '(name version
                                         description)
                                  a)))
                        (t (or (pcase-let ((`(,name ,version ,type) a))
                                 (mapcar #'substring-no-properties
                                         (list name version (or type ""))))
                               (pcase-let ((`(,name ,version) a))
                                 (mapcar #'substring-no-properties (list name
                                                                         version
                                                                         ""))))))))
              (list (car row)
                    (apply #'vector row))))
          alist))

(defun npmjs-list-render (buff-name entries)
  "Render ENTRIES in buffer BUFF-NAME."
  (with-current-buffer (get-buffer-create (format buff-name
                                                  default-directory))
    (unless (eq major-mode 'npmjs-list-mode)
      (npmjs-list-mode))
    (setq tabulated-list-entries entries)
    (tabulated-list-print)
    (unless (get-buffer-window (current-buffer))
      (pop-to-buffer (current-buffer)))))

(defun npmjs-list-global-packages ()
  "Read globally installed js packages."
  (let* ((default-directory
          (expand-file-name (or
                             (npmjs-get-project-root)
                             (vc-root-dir)
                             default-directory)))
         (process-environment (or (npmjs-get-node-env) process-environment))
         (alist (npmjs-pluck-depenencies (npmjs-global-packages)))
         (buff-name (format "Globally intstalled packages (%s): "
                            (string-trim (shell-command-to-string
                                          "node -v"))))
         (entries (npmjs-alist-to-entries alist)))
    (npmjs-list-render buff-name entries)))

(defun npmjs-list-local-packages ()
  "Read globally installed js packages."
  (let* ((default-directory
          (expand-file-name (or
                             (npmjs-get-project-root)
                             (vc-root-dir)
                             default-directory)))
         (process-environment (or (npmjs-get-node-env) process-environment))
         (alist (npmjs-pluck-depenencies (npmjs-local-packages)))
         (buff-name (format "Locally intstalled packages (%s): "
                            (string-trim (shell-command-to-string
                                          "node -v"))))
         (entries (npmjs-alist-to-entries alist)))
    (npmjs-list-render buff-name entries)))



(defun npmjs-read-global-packages ()
  "Read globally installed js packages."
  (let* ((default-directory
          (expand-file-name (or
                             (npmjs-get-project-root)
                             (vc-root-dir)
                             default-directory)))
         (process-environment (or (npmjs-nvm-get-env-for-node
                                   (npmjs-confirm-node-version))
                                  process-environment))
         (alist (npmjs-pluck-depenencies (npmjs-global-packages)))
         (prompt (format "Globally intstalled packages (%s): "
                         (string-trim (shell-command-to-string
                                       "node -v"))))
         (annotf (lambda (it)
                   (let ((value (cdr-safe (assoc it alist))))
                     (concat "@" (if (listp value)
                                     (string-join value "")
                                   (or value "")))))))
    (completing-read prompt
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (annotation-function . ,annotf))
                         (complete-with-action action alist
                                               str pred))))))

(defun npmjs-s-strip-props (item)
  "If ITEM is string, return it without text properties.

 If ITEM is symbol, return it is `symbol-name.'
 Otherwise return nil."
  (cond ((stringp item)
         (let ((str (seq-copy item)))
           (set-text-properties 0 (length str) nil str)
           str))
        ((and item (symbolp item))
         (symbol-name item))
        (nil item)))

(defun npmjs-pluck-depenencies (package-json-alist)
  "Completion table with current PACKAGE-JSON-ALIST dependencies."
  (seq-reduce (lambda (acc it)
                (if-let ((deps (when-let ((found
                                           (alist-get it package-json-alist)))
                                 (mapcar (lambda (cell)
                                           (let* ((symb (car cell))
                                                  (value (cdr cell))
                                                  (version
                                                   (if (stringp value)
                                                       value
                                                     (cdr-safe (assoc 'version
                                                                      value))))
                                                  (type (pcase it
                                                          ('devDependencies
                                                           "dev")
                                                          ('peerDependencies
                                                           "peer")
                                                          ('optionalDependencies
                                                           "optional"))))
                                             (remove nil
                                                     (list (npmjs-s-strip-props
                                                            (if (stringp
                                                                 symb)
                                                                symb
                                                              (symbol-name
                                                               symb)))
                                                           version
                                                           type))))
                                         found))))
                    (setq acc (append acc deps)))
                acc)
              '(optionalDependencies
                peerDependencies
                devDependencies
                dependencies)
              '()))
(defun npmjs-get-package-info (package)
  "Exec \"npm info\" for PACKAGE and return list of available versions."
  (require 'json)
  (when (fboundp 'json-read-from-string)
    (json-read-from-string
     (string-trim
      (shell-command-to-string
       (string-join
        `("npm info"
          ,package
          "--json")
        " "))))))

(defun npmjs-get-package-versions (package)
  "Exec \"npm info\" for PACKAGE and return list of available versions."
  (require 'json)
  (let ((data (npmjs-get-package-info package)))
    (append
     (mapcar #'cdr (alist-get 'dist-tags data))
     (reverse
      (append
       (alist-get 'versions
                  data)
       nil)))))

(defun npmjs-confirm-package-version (package &optional prompt)
  "Confirm PACKAGE version with PROMPT."
  (let ((version))
    (unless (string-match-p "[a-z-]+@" package)
      (when-let ((versions (seq-reduce
                            (lambda (acc it)
                              (let ((prefixes '("^" ">="))
                                    (l `(,it)))
                                (dolist (prefix prefixes)
                                  (push (format "%s%s" prefix it) l))
                                (setq acc (append acc l))))
                            (npmjs-get-package-versions package)
                            '())))
        (setq version
              (completing-read
               (or prompt (format "%s " package))
               (push "latest" versions)))))
    (if version
        (format "%s@%s" package version)
      package)))

(defun npmjs-read-script ()
  "Read package json script."
  (let* ((alist
          (when-let* ((package-json-path
                       (npmjs-get-package-json-path))
                      (package-json
                       (npmjs-read-json
                        package-json-path
                        'alist)))
            (alist-get
             'scripts
             package-json)))
         (annotf
          (lambda (it)
            (concat ": "
                    (cdr
                     (assoc
                      (intern
                       it)
                      alist))))))
    (completing-read "Script: "
                     (lambda (str pred
                             action)
                       (if
                           (eq action
                               'metadata)
                           `(metadata
                             (annotation-function
                              . ,annotf))
                         (complete-with-action
                          action alist
                          str
                          pred))))))

(defun npmjs-default-format-args (args)
  "Format ARGS to string."
  (mapconcat (lambda (it)
               (let ((opt (string-trim it)))
                 (if (string-match "\s\t" opt)
                     (shell-quote-argument opt)
                   opt)))
             (remove nil (flatten-list
                          args))
             "\s"))

;; transient
(defun npmjs-format-transient-args ()
  "Return string from current transient arguments."
  (let ((args (transient-args
               transient-current-command)))
    (pcase transient-current-command
      ('npmjs-audit (npmjs-default-format-args
                     (if-let ((subcommand
                               (seq-find
                                (lambda (it)
                                  (member
                                   it
                                   args))
                                '("signatures"
                                  "fix"))))
                         (append (list subcommand)
                                 (remove subcommand args))
                       args)))
      (_ (npmjs-default-format-args
          args)))))


;;;###autoload
(defun npmjs-done ()
  "Execute npm transient command."
  (interactive)
  (when transient-current-command
    (let* ((global (transient-arg-value "--global"
                                        (transient-args
                                         transient-current-command)))
           (formatted-args (npmjs-format-transient-args))
           (cmd (pcase transient-current-command
                  ('npmjs-get "npm get")
                  ('npmjs-whoami "npm whoami")
                  ('npmjs-version "npm version")
                  ('npmjs-view "npm view")
                  ('npmjs-update
                   (let ((package
                          (if
                              global
                              (npmjs-read-global-packages)
                            (npmjs-read-local-packages))))
                     (concat "npm update " (or (npmjs-confirm-package-version
                                                package)
                                               package))))
                  ('npmjs-unpublish "npm unpublish")
                  ('npmjs-unstar "npm unstar")
                  ('npmjs-token "npm token")
                  ('npmjs-team "npm team")
                  ('npmjs-test "npm test")
                  ('npmjs-stop "npm stop")
                  ('npmjs-star "npm star")
                  ('npmjs-stars "npm stars")
                  ('npmjs-start "npm start")
                  ('npmjs-shrinkwrap "npm shrinkwrap")
                  ('npmjs-set "npm set")
                  ;; ('npmjs-search "npm search")
                  ('npmjs-root "npm root")
                  ('npmjs-run-script
                   (list "npm run-script"
                         (npmjs-read-script)))
                  ('npmjs-rebuild "npm rebuild")
                  ('npmjs-repo "npm repo")
                  ('npmjs-restart "npm restart")
                  ('npmjs-uninstall
                   (string-join (list "npm uninstall"
                                      (let ((package
                                             (if global
                                                 (npmjs-read-global-packages)
                                               (npmjs-read-local-packages))))
                                        package))
                                "\s"))
                  ('npmjs-pack "npm pack")
                  ('npmjs-ping "npm ping")
                  ('npmjs-pkg "npm pkg")
                  ('npmjs-publish "npm publish")
                  ('npmjs-prefix "npm prefix")
                  ('npmjs-profile "npm profile")
                  ('npmjs-prune "npm prune")
                  ('npmjs-org "npm org")
                  ('npmjs-outdated "npm outdated")
                  ('npmjs-owner "npm owner")
                  ('npmjs-link "npm link")
                  ('npmjs-ll "npm ll")
                  ('npmjs-ls "npm ls")
                  ('npmjs-login "npm login")
                  ('npmjs-logout "npm logout")
                  ('npmjs-install-test "npm install-test")
                  ('npmjs-install
                   (list
                    "npm install"
                    (funcall-interactively #'npmjs-search-package)))
                  ('npmjs-help "npm help")
                  ('npmjs-hook "npm hook")
                  ('npmjs-find-dupes "npm find-dupes")
                  ('npmjs-fund "npm fund")
                  ('npmjs-explain "npm explain")
                  ('npmjs-explore "npm explore")
                  ('npmjs-exec "npm exec")
                  ('npmjs-edit (list "npm edit"
                                     (if (or global (not
                                                     (npmjs-get-project-root)))
                                         (npmjs-read-global-packages)
                                       (npmjs-read-local-packages))))
                  ('npmjs-docs "npm docs")
                  ('npmjs-doctor "npm doctor")
                  ('npmjs-diff "npm diff")
                  ('npmjs-dist-tag "npm dist-tag")
                  ('npmjs-dedupe "npm dedupe")
                  ('npmjs-deprecate "npm deprecate")
                  ('npmjs-config "npm config")
                  ('npmjs-cache "npm cache")
                  ('npmjs-ci "npm ci")
                  ('npmjs-bin "npm bin")
                  ('npmjs-bugs "npm bugs")
                  ('npmjs-access "npm access")
                  ('npmjs-adduser "npm adduser")
                  ('npmjs-audit (list "npm audit"))))
           (full-cmd (string-trim
                      (string-join
                       (remove nil
                               (flatten-list
                                (list cmd
                                      formatted-args)))
                       "\s"))))
      (if global
          (npmjs-compile-global (read-string "Run: " full-cmd))
        (let ((default-directory (expand-file-name (or
                                                    (npmjs-get-project-root)
                                                    (vc-root-dir)
                                                    default-directory))))
          (npmjs-compile (read-string "Run: " full-cmd)))))))
;;;###autoload (autoload 'npmjs-show-args "npmjs.el" nil t)
(transient-define-suffix npmjs-show-args ()
  :transient t
  (interactive)
  (when-let ((args
              (npmjs-format-transient-args)))
    (message
     (propertize args 'face 'success))))

(transient-define-argument npmjs-audit--audit-level ()
  "Set argument --audit-level."
  :class 'transient-switches
  :description "Level"
  :argument-format "--audit-level=%s"
  :argument-regexp "info\\|low\\|moderate\\|high\\|critical"
  :choices '("info" "low" "moderate" "high" "critical"))

(transient-define-argument npmjs--omit-option ()
  "Set argument --omit."
  :class 'transient-option
  :argument "--omit "
  :description "Omit"
  :multi-value 'repeat
  :choices '("dev" "optional" "peer"))

(transient-define-argument npmjs--workspace-option ()
  "Set argument --workspace."
  :description "Set workspace "
  :argument "--workspace "
  :multi-value 'repeat
  :class 'transient-option)

(transient-define-argument npmjs--registry-option ()
  "Set argument --registry."
  :description "Registry "
  :argument "--registry "
  :class 'transient-option)

(transient-define-argument npmjs--otp-option ()
  "Set argument --otp."
  :description "One time password "
  :argument "--otp "
  :class 'transient-option)

(transient-define-argument npmjs--depth-option ()
  "Set argument --depth."
  :description "--depth "
  :argument "--depth "
  :class 'transient-option)

(transient-define-argument npmjs-audit-subcommand-option ()
  :class 'transient-switches
  :description "Command"
  :argument-format "%s"
  :argument-regexp
  "fix\\|signatures"
  :choices '("fix" "signatures"))

;;;###autoload (autoload 'npmjs-audit "npmjs.el" nil t)
(transient-define-prefix npmjs-audit ()
  "Run a security audit.
Usage:
npm audit [fix|signatures]

Options:
[--audit-level= <info|low|moderate|high|critical|none>] [--dry-run] [-f|--force]
[--json] [--package-lock-only]
[--omit <dev|optional|peer> [--omit <dev|optional|peer> ...]]
[--foreground-scripts] [--ignore-scripts]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root] [--install-links]

Run \"npm help audit\" for more info"
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("o" npmjs--omit-option)
   ("a" npmjs-audit--audit-level)
   ("c" npmjs-audit-subcommand-option)]
  ["Switches"
   ("i" "install-links" "--install-links")
   ("r" "include-workspace-root" "--include-workspace-root")
   ("s" "workspaces" "--workspaces")
   ("g" "ignore-scripts" "--ignore-scripts")
   ("f" "foreground-scripts" "--foreground-scripts")
   ("p" "package-lock-only" "--package-lock-only")
   ("j" "json" "--json")
   ("F" "force" "--force")
   ("d" "dry-run" "--dry-run")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-adduser--auth-type ()
  "Set argument --auth-type."
  :class 'transient-switches
  :description "Auth type "
  :argument-format "%s"
  :argument-regexp "legacy\\|web\\|sso\\|saml\\|oauth\\|webauthn"
  :choices '("legacy"
             "web"
             "sso"
             "saml"
             "oauth"
             "webauthn"))

(transient-define-argument npmjs-adduser--scope ()
  "Set argument --scope."
  :description "--scope "
  :argument "--scope "
  :class 'transient-option)


;;;###autoload (autoload 'npmjs-adduser "npmjs.el" nil t)
(transient-define-prefix npmjs-adduser ()
  "Add a registry user account.
Usage:
npm adduser

Options:
[--registry <registry>] [--scope <@scope>]
[--auth-type <legacy|web|sso|saml|oauth|webauthn>]

aliases: login, add-user

Run \"npm help adduser\" for more info"
  ["Arguments"
   ("a" npmjs-adduser--auth-type)
   ("s" npmjs-adduser--scope)
   ("r" npmjs--registry-option)]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-access-type-options ()
  :class 'transient-switches
  :description "Access"
  :argument-format "%s"
  :init-value (lambda (ob)
                (when (not (slot-value ob 'value))
                  (setf (slot-value ob 'value)
                        "restricted")))
  :argument-regexp
  "public\\|restricted\\|ls-collaborators\\|ls-packages\\|2fa-not-required\\|2fa-required\\|revoke\\|grant"
  :choices '("public" "restricted" "ls-collaborators"
             "ls-packages"
             "2fa-not-required"
             "2fa-required"
             "revoke"
             "grant"))


;;;###autoload (autoload 'npmjs-access "npmjs.el" nil t)
(transient-define-prefix npmjs-access ()
  "Set access level on published packages.
Usage:
npm access public [<package>]
npm access restricted [<package>]
npm access grant <read-only|read-write> <scope:team> [<package>]
npm access revoke <scope:team> [<package>]
npm access 2fa-required [<package>]
npm access 2fa-not-required [<package>]
npm access ls-packages [<user>|<scope>|<scope:team>]
npm access ls-collaborators [<package> [<user>]]
npm access edit [<package>]

Options:
[--registry <registry>] [--otp <otp>]

Run \"npm help access\" for more info"
  ["Arguments"
   ("o" npmjs--otp-option)
   ("a" npmjs-access-type-options)
   ("r" npmjs--registry-option)]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-bugs--browser ()
  "Set argument --browser."
  :description "--browser "
  :argument "--browser "
  :class 'transient-option
  :always-read t)


;;;###autoload (autoload 'npmjs-bugs "npmjs.el" nil t)
(transient-define-prefix npmjs-bugs ()
  "Report bugs for a package in a web browser.
Usage:
npm bugs [<pkgname> [<pkgname> ...]]

Options:
[--no-browser|--browser <browser>] [--registry <registry>]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root]

alias: issues

Run \"npm help bugs\" for more info"
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("r" npmjs--registry-option)
   ("b" npmjs-bugs--browser)]
  ["Switches"
   ("i" "include-workspace-root" "--include-workspace-root")
   ("s" "workspaces" "--workspaces")
   ("n" "no-browser" "--no-browser")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])



;;;###autoload (autoload 'npmjs-bin "npmjs.el" nil t)
(transient-define-prefix npmjs-bin ()
  "Display npm bin folder.
Usage:
npm bin

Options:
[-g|--global]

Run \"npm help bin\" for more info"
  :value (lambda ()
           (unless (npmjs-get-project-root)
             (list "--global")))
  ["Switches"
   ("g" "global" "--global")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-ci--script-shell ()
  "Set argument --script-shell."
  :description "--script-shell "
  :argument "--script-shell "
  :class 'transient-option)

;;;###autoload (autoload 'npmjs-ci "npmjs.el" nil t)
(transient-define-prefix npmjs-ci ()
  "Clean install a project.
Usage:
npm ci

Options:
[--no-audit] [--foreground-scripts] [--ignore-scripts]
[--script-shell <script-shell>]

aliases: clean-install, ic, install-clean, isntall-clean

Run \"npm help ci\" for more info"
  ["Arguments"
   ("s" npmjs-ci--script-shell)]
  ["Switches"
   ("i" "ignore-scripts" "--ignore-scripts")
   ("f" "foreground-scripts" "--foreground-scripts")
   ("n" "no-audit" "--no-audit")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-cache--cache ()
  "Set argument --cache."
  :description "Cache "
  :argument "--cache "
  :class 'transient-option
  :always-read t)

(transient-define-argument npmjs-cache-options ()
  :class 'transient-switches
  :argument-format "%s"
  :init-value (lambda (ob)
                (when (not (slot-value ob 'value))
                  (setf (slot-value ob 'value)
                        "ls")))
  :argument-regexp
  "clean\\|add\\|ls\\|verify"
  :choices '("clean"
             "add"
             "ls"
             "verify"))
;;;###autoload (autoload 'npmjs-cache "npmjs.el" nil t)
(transient-define-prefix npmjs-cache ()
  "Manipulates packages cache.
Usage:
npm cache add <package-spec>
npm cache clean [<key>]
npm cache ls [<name>@<version>]
npm cache verify

Options:
[--cache <cache>]

Run \"npm help cache\" for more info"
  ["Arguments"
   ("c" "Cache" "--cache " :class transient-option)
   ("s" "Commmand" npmjs-cache-options)]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-config--editor ()
  "Set argument --editor."
  :description "--editor "
  :argument "--editor "
  :init-value (lambda (ob)
                (when (not (slot-value ob 'value))
                  (setf (slot-value ob 'value)
                        "emacsclient")))
  :class 'transient-option)

(transient-define-argument npmjs-config-argument ()
  :class 'transient-switches
  :always-read t
  :argument-format "%s"
  :argument-regexp "list\\|list --json\\|set\\|edit\\|delete"
  :choices '("list" "list --json" "set" "edit" "delete"))

(transient-define-argument npmjs-config--location ()
  :class 'transient-switches
  :description "Location"
  :always-read t
  :argument-format "--location %s"
  :argument-regexp "global\\|user\\|project"
  :choices '("global" "user" "project"))
;;;###autoload (autoload 'npmjs-config "npmjs.el" nil t)
(transient-define-prefix npmjs-config ()
  "Manage the npm configuration files.
Usage:
npm config set <key>=<value> [<key>=<value> ...]
npm config get [<key> [<key> ...]]
npm config delete <key> [<key> ...]
npm config list [--json]
npm config edit

Options:
[--json] [-g|--global] [--editor <editor>] [-L|--location <global|user|project>]
[-l|--long]

alias: c

Run \"npm help config\" for more info"
  :value (lambda ()
           (unless (npmjs-get-project-root)
             (list "list" "--global")))
  ["Arguments"
   ("c" "action" npmjs-config-argument)
   ("L" npmjs-config--location)
   ("e" npmjs-config--editor)]
  ["Switches"
   ("j" "json" "--json")
   ("l" "long" "--long")
   ("g" "global" "--global")
   ("s" "json" "--json")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-completion "npmjs.el" nil t)
(transient-define-prefix npmjs-completion ()
  "Tab Completion for npm.
Usage:
npm completion

Run \"npm help completion\" for more info"
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-deprecate "npmjs.el" nil t)
(transient-define-prefix npmjs-deprecate ()
  "Deprecate a version of a package.
Usage:
npm deprecate <package-spec> <message>

Options:
[--registry <registry>] [--otp <otp>]

Run \"npm help deprecate\" for more info"
  ["Arguments"
   ("o" npmjs--otp-option)
   ("r" npmjs--registry-option)]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-dedupe "npmjs.el" nil t)
(transient-define-prefix npmjs-dedupe ()
  "Reduce duplication in the package tree.
Usage:
npm dedupe

Options:
[--global-style] [--legacy-bundling] [--strict-peer-deps] [--no-package-lock]
[--omit <dev|optional|peer> [--omit <dev|optional|peer> ...]] [--ignore-scripts]
[--no-audit] [--no-bin-links] [--no-fund] [--dry-run]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root] [--install-links]

alias: ddp

Run \"npm help dedupe\" for more info"
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("o" npmjs--omit-option)]
  ["Switches"
   ("i" "install-links" "--install-links")
   ("r" "include-workspace-root" "--include-workspace-root")
   ("s" "workspaces" "--workspaces")
   ("d" "dry-run" "--dry-run")
   ("n" "no-fund" "--no-fund")
   ("b" "no-bin-links" "--no-bin-links")
   ("a" "no-audit" "--no-audit")
   ("g" "ignore-scripts" "--ignore-scripts")
   ("p" "no-package-lock" "--no-package-lock")
   ("t" "strict-peer-deps" "--strict-peer-deps")
   ("l" "legacy-bundling" "--legacy-bundling")
   ("y" "global-style" "--global-style")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-dist-tag "npmjs.el" nil t)
(transient-define-prefix npmjs-dist-tag ()
  "Modify package distribution tags.
Usage:
npm dist-tag add <package-spec (with version)> [<tag>]
npm dist-tag rm <package-spec> <tag>
npm dist-tag ls [<package-spec>]

Options:
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root]

alias: dist-tags

Run \"npm help dist-tag\" for more info"
  ["Arguments"
   ("w" npmjs--workspace-option)]
  ["Switches"
   ("i" "include-workspace-root" "--include-workspace-root")
   ("s" "workspaces" "--workspaces")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-diff--tag ()
  "Set argument --tag."
  :description "--tag "
  :argument "--tag "
  :class 'transient-option
  :always-read t)

(transient-define-argument npmjs-diff--diff-dst-prefix ()
  "Set argument --diff-dst-prefix."
  :description "--diff-dst-prefix "
  :argument "--diff-dst-prefix "
  :class 'transient-option
  :always-read t)

(transient-define-argument npmjs-diff--diff-src-prefix ()
  "Set argument --diff-src-prefix."
  :description "--diff-src-prefix "
  :argument "--diff-src-prefix "
  :class 'transient-option
  :always-read t)

(transient-define-argument npmjs-diff--diff-unified ()
  "Set argument --diff-unified."
  :description "--diff-unified "
  :argument "--diff-unified "
  :class 'transient-option
  :always-read t)

(transient-define-argument npmjs-diff--diff ()
  "Set argument --diff."
  :description "--diff "
  :argument "--diff "
  :class 'transient-option
  :always-read t)


;;;###autoload (autoload 'npmjs-diff "npmjs.el" nil t)
(transient-define-prefix npmjs-diff ()
  "The registry diff command.
Usage:
npm diff [...<paths>]

Options:
[--diff <package-spec> [--diff <package-spec> ...]]
[--diff-name-only]
[--diff-unified <number>] [--diff-ignore-all-space] [--diff-no-prefix]
[--diff-src-prefix <path>] [--diff-dst-prefix <path>] [--diff-text]
[-g|--global]
[--tag <tag>]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root]

Run \"npm help diff\" for more info"
  :value (lambda ()
           (unless (npmjs-get-project-root)
             (list "--global")))
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("t" npmjs-diff--tag)
   ("d" npmjs-diff--diff-dst-prefix)
   ("s" npmjs-diff--diff-src-prefix)
   ("u" npmjs-diff--diff-unified)
   ("i" npmjs-diff--diff)]
  ["Switches"
   ("r" "include-workspace-root" "--include-workspace-root")
   ("W" "workspaces" "--workspaces")
   ("g" "global" "--global")
   ("e" "diff-text" "--diff-text")
   ("n" "diff-no-prefix" "--diff-no-prefix")
   ("a" "diff-ignore-all-space" "--diff-ignore-all-space")
   ("o" "diff-name-only" "--diff-name-only")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-doctor "npmjs.el" nil t)
(transient-define-prefix npmjs-doctor ()
  "Check your npm environment.
Usage:
npm doctor

Options:
[--registry <registry>]

Run \"npm help doctor\" for more info"
  ["Arguments"
   ("r" npmjs--registry-option)]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-docs--browser ()
  "Set argument --browser."
  :description "--browser "
  :argument "--browser "
  :class 'transient-option
  :always-read t)


;;;###autoload (autoload 'npmjs-docs "npmjs.el" nil t)
(transient-define-prefix npmjs-docs ()
  "Open documentation for a package in a web browser.
Usage:
npm docs [<pkgname> [<pkgname> ...]]

Options:
[--no-browser|--browser <browser>] [--registry <registry>]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root]

alias: home

Run \"npm help docs\" for more info"
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("r" npmjs--registry-option)
   ("b" npmjs-docs--browser)]
  ["Switches"
   ("i" "include-workspace-root" "--include-workspace-root")
   ("s" "workspaces" "--workspaces")
   ("n" "no-browser" "--no-browser")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-edit--editor ()
  "Set argument --editor."
  :description "--editor "
  :argument "--editor "
  :class 'transient-option
  :always-read t)


;;;###autoload (autoload 'npmjs-edit "npmjs.el" nil t)
(transient-define-prefix npmjs-edit ()
  "Edit an installed package.
Usage:
npm edit <pkg>[/<subpkg>...]

Options:
[--editor <editor>]

Run \"npm help edit\" for more info"
  ["Arguments"
   ("e" npmjs-edit--editor)]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-exec--call ()
  "Set argument --call."
  :description "--call "
  :argument "--call "
  :class 'transient-option
  :always-read t)

(transient-define-argument npmjs-exec--package ()
  "Set argument --package."
  :description "--package "
  :argument "--package "
  :class 'transient-option
  :always-read t)


;;;###autoload (autoload 'npmjs-exec "npmjs.el" nil t)
(transient-define-prefix npmjs-exec ()
  "Run a command from a local or remote npm package.
Usage:
npm exec -- <pkg>[@<version>] [args...]
npm exec --package=<pkg>[@<version>] -- <cmd> [args...]
npm exec -c '<cmd> [args...]'
npm exec --package=foo -c '<cmd> [args...]'

Options:
[--package <package-spec> [--package <package-spec> ...]] [-c|--call <call>]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root]

alias: x

Run \"npm help exec\" for more info"
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("c" npmjs-exec--call)
   ("p" npmjs-exec--package)
   ("a" npmjs-exec--package)]
  ["Switches"
   ("i" "include-workspace-root" "--include-workspace-root")
   ("s" "workspaces" "--workspaces")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-explore--shell ()
  "Set argument --shell."
  :description "--shell "
  :argument "--shell "
  :class 'transient-option
  :always-read t)


;;;###autoload (autoload 'npmjs-explore "npmjs.el" nil t)
(transient-define-prefix npmjs-explore ()
  "Browse an installed package.
Usage:
npm explore <pkg> [-- <command>]

Options:
[--shell <shell>]

Run \"npm help explore\" for more info"
  ["Arguments"
   ("s" npmjs-explore--shell)]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-explain "npmjs.el" nil t)
(transient-define-prefix npmjs-explain ()
  "Explain installed packages.
Usage:
npm explain <package-spec>

Options:
[--json] [-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]

alias: why

Run \"npm help explain\" for more info"
  ["Arguments"
   ("w" npmjs--workspace-option)]
  ["Switches"
   ("j" "json" "--json")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-fund--which ()
  "Set argument --which."
  :description "--which "
  :argument "--which "
  :class 'transient-option
  :always-read t)

(transient-define-argument npmjs-fund--browser ()
  "Set argument --browser."
  :description "--browser "
  :argument "--browser "
  :class 'transient-option
  :always-read t)


;;;###autoload (autoload 'npmjs-fund "npmjs.el" nil t)
(transient-define-prefix npmjs-fund ()
  "Retrieve funding information.
Usage:
npm fund [<package-spec>]

Options:
[--json] [--no-browser|--browser <browser>] [--no-unicode]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[--which <fundingSourceNumber>]

Run \"npm help fund\" for more info"
  ["Arguments"
   ("w" npmjs-fund--which)
   ("-" npmjs--workspace-option)
   ("b" npmjs-fund--browser)]
  ["Switches"
   ("n" "no-unicode" "--no-unicode")
   ("o" "no-browser" "--no-browser")
   ("j" "json" "--json")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-find-dupes "npmjs.el" nil t)
(transient-define-prefix npmjs-find-dupes ()
  "Find duplication in the package tree.
Usage:
npm find-dupes

Options:
[--global-style] [--legacy-bundling] [--strict-peer-deps] [--no-package-lock]
[--omit <dev|optional|peer> [--omit <dev|optional|peer> ...]] [--ignore-scripts]
[--no-audit] [--no-bin-links] [--no-fund]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root] [--install-links]

Run \"npm help find-dupes\" for more info"
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("o" npmjs--omit-option)]
  ["Switches"
   ("i" "install-links" "--install-links")
   ("r" "include-workspace-root" "--include-workspace-root")
   ("s" "workspaces" "--workspaces")
   ("n" "no-fund" "--no-fund")
   ("b" "no-bin-links" "--no-bin-links")
   ("a" "no-audit" "--no-audit")
   ("g" "ignore-scripts" "--ignore-scripts")
   ("p" "no-package-lock" "--no-package-lock")
   ("d" "strict-peer-deps" "--strict-peer-deps")
   ("l" "legacy-bundling" "--legacy-bundling")
   ("t" "global-style" "--global-style")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-hook "npmjs.el" nil t)
(transient-define-prefix npmjs-hook ()
  "Manage registry hooks.
Usage:
npm hook add <pkg> <url> <secret> [--type=<type>]
npm hook ls [pkg]
npm hook rm <id>
npm hook update <id> <url> <secret>

Options:
[--registry <registry>] [--otp <otp>]

Run \"npm help hook\" for more info"
  ["Arguments"
   ("o" npmjs--otp-option)
   ("r" npmjs--registry-option)]
  ["Switches"
   ("t" "type" "--type")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-help--viewer ()
  "Set argument --viewer."
  :description "--viewer "
  :argument "--viewer "
  :class 'transient-option
  :always-read t)


;;;###autoload (autoload 'npmjs-help "npmjs.el" nil t)
(transient-define-prefix npmjs-help ()
  "Get help on npm.
Usage:
npm help <term> [<terms..>]

Options:
[--viewer <viewer>]

alias: hlep

Run \"npm help help\" for more info"
  ["Arguments"
   ("v" npmjs-help--viewer)]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-init--scope ()
  "Set argument --scope."
  :description "--scope "
  :argument "--scope "
  :class 'transient-option
  :always-read t)


(defun npmjs-init-do ()
  "Initialize new project."
  (interactive)
  (let ((args (transient-args
               transient-current-command)))
    (let ((dir
           (file-name-as-directory
            (read-directory-name "Directory for project "
                                 (npmjs-get-non-project-root)))))
      (if (file-exists-p dir)
          (setq default-directory dir)
        (when (yes-or-no-p (format
                            "Create directory %s?" dir))
          (mkdir dir)
          (setq default-directory dir)))
      (let ((default-directory (file-name-as-directory dir)))
        (npmjs-compile
         (string-trim
          (concat "npm init "
                  (npmjs-default-format-args
                   args))))))))


;;;###autoload (autoload 'npmjs-init "npmjs.el" nil t)
(transient-define-prefix npmjs-init ()
  "Create a package.json file.
Usage:
npm init <package-spec> (same as `npx <package-spec>)
npm init <@scope> (same as `npx <@scope>/create`)

Options:
[-y|--yes] [-f|--force] [--scope <@scope>]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--no-workspaces-update] [--include-workspace-root]

aliases: create, innit

Run \"npm help init\" for more info"
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("-" npmjs--workspace-option)
   ("s" npmjs-init--scope)]
  ["Switches"
   ("i" "include-workspace-root" "--include-workspace-root")
   ("n" "no-workspaces-update" "--no-workspaces-update")
   ("W" "workspaces" "--workspaces")
   ("f" "force" "--force")
   ("y" "yes" "--yes")]
  ["Actions"
   ("RET" "Run" npmjs-init-do)
   ("<return>" "Run" npmjs-init-do)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

;;;###autoload (autoload 'npmjs-install-test "npmjs.el" nil t)
(transient-define-prefix npmjs-install-test ()
  "Install package(s) and run tests.

Usage:
npm install-test [<package-spec> ...]

Options:
[-S|--save|--no-save|--save-prod|--save-dev|--save-optional|
--save-peer|--save-bundle]
[-E|--save-exact] [-g|--global] [--global-style] [--legacy-bundling]
[--omit <dev|optional|peer> [--omit <dev|optional|peer> ...]]
[--strict-peer-deps] [--no-package-lock] [--foreground-scripts]
[--ignore-scripts] [--no-audit] [--no-bin-links] [--no-fund] [--dry-run]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root] [--install-links]

alias: it

Run \"npm help install-test\" for more info"
  :value (lambda ()
           (unless (npmjs-get-project-root)
             (list "--global")))
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("o" npmjs--omit-option)]
  ["Switches"
   ("i" "install-links" "--install-links")
   ("r" "include-workspace-root" "--include-workspace-root")
   ("s" "workspaces" "--workspaces")
   ("d" "dry-run" "--dry-run")
   ("n" "no-fund" "--no-fund")
   ("b" "no-bin-links" "--no-bin-links")
   ("a" "no-audit" "--no-audit")
   ("g" "ignore-scripts" "--ignore-scripts")
   ("f" "foreground-scripts" "--foreground-scripts")
   ("p" "no-package-lock" "--no-package-lock")
   ("t" "strict-peer-deps" "--strict-peer-deps")
   ("l" "legacy-bundling" "--legacy-bundling")
   ("y" "global-style" "--global-style")
   ("G" "global" "--global")
   ("E" "save-exact" "--save-exact")
   ("v" "save-bundle" "--save-bundle")
   ("e" "save-peer" "--save-peer")
   ("S" "save-optional" "--save-optional")
   ("A" "save-dev" "--save-dev")
   ("V" "save-prod" "--save-prod")
   ("N" "no-save" "--no-save")
   ("c" "save" "--save")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-install--script-shell ()
  "Set argument --script-shell."
  :description "--script-shell "
  :argument "--script-shell "
  :class 'transient-option
  :always-read t)



;;;###autoload (autoload 'npmjs-install "npmjs.el" nil t)
(transient-define-prefix npmjs-install ()
  "Install a package.
Usage:
npm install [<package-spec> ...]

Options:
[-S|--save|--no-save|--save-prod|--save-dev|--save-optional|
--save-peer|--save-bundle]
[-E|--save-exact] [-g|--global] [--global-style] [--legacy-bundling]
[--omit <dev|optional|peer> [--omit <dev|optional|peer> ...]]
[--strict-peer-deps] [--no-package-lock] [--foreground-scripts]
[--ignore-scripts] [--no-audit] [--no-bin-links] [--no-fund] [--dry-run]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root] [--install-links]

aliases: add, i, in, ins, inst, insta, instal, isnt, isnta, isntal, isntall

Run \"npm help install\" for more info

install-ci-test Install a project with a clean slate and run tests

Usage:
npm install-ci-test

Options:
[--no-audit] [--foreground-scripts] [--ignore-scripts]
[--script-shell <script-shell>]

alias: cit

Run \"npm help install-ci-test\" for more info"
  :value (lambda ()
           (unless (npmjs-get-project-root)
             (list "--global")))
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("o" npmjs--omit-option)
   ("s" npmjs-install--script-shell)]
  ["Switches"
   ("i" "install-links" "--install-links")
   ("r" "include-workspace-root" "--include-workspace-root")
   ("W" "workspaces" "--workspaces")
   ("d" "dry-run" "--dry-run")
   ("n" "no-fund" "--no-fund")
   ("b" "no-bin-links" "--no-bin-links")
   ("a" "no-audit" "--no-audit")
   ("g" "ignore-scripts" "--ignore-scripts")
   ("f" "foreground-scripts" "--foreground-scripts")
   ("p" "no-package-lock" "--no-package-lock")
   ("t" "strict-peer-deps" "--strict-peer-deps")
   ("l" "legacy-bundling" "--legacy-bundling")
   ("y" "global-style" "--global-style")
   ("G" "global" "--global")
   ("E" "save-exact" "--save-exact")
   ("v" "save-bundle" "--save-bundle")
   ("e" "save-peer" "--save-peer")
   ("S" "save-optional" "--save-optional")
   ("A" "save-dev" "--save-dev")
   ("V" "save-prod" "--save-prod")
   ("N" "no-save" "--no-save")
   ("c" "save" "--save")
   ("I" "ignore-scripts" "--ignore-scripts")
   ("u" "foreground-scripts" "--foreground-scripts")
   ("O" "no-audit" "--no-audit")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-logout--scope ()
  "Set argument --scope."
  :description "--scope "
  :argument "--scope "
  :class 'transient-option
  :always-read t)


;;;###autoload (autoload 'npmjs-logout "npmjs.el" nil t)
(transient-define-prefix npmjs-logout ()
  "Log out of the registry.
Usage:
npm logout

Options:
[--registry <registry>] [--scope <@scope>]

Run \"npm help logout\" for more info"
  ["Arguments"
   ("s" npmjs-logout--scope)
   ("r" npmjs--registry-option)]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-login--auth-type ()
  "Set argument --auth-type."
  :description "Auth type"
  :argument "--auth-type "
  :argument-format "--auth-type %s"
  :argument-regexp "legacy\\|web\\|sso\\|saml\\|oauth\\|webauthn"
  :choices '("legacy" "web" "sso" "saml" "oauth" "webauthn")
  :class 'transient-switches
  :always-read t)

(transient-define-argument npmjs-login--scope ()
  "Set argument --scope."
  :description "--scope "
  :argument "--scope "
  :class 'transient-option
  :always-read t)


;;;###autoload (autoload 'npmjs-login "npmjs.el" nil t)
(transient-define-prefix npmjs-login ()
  "Add a registry user account.
Usage:
npm adduser

Options:
[--registry <registry>] [--scope <@scope>]
[--auth-type <legacy|web|sso|saml|oauth|webauthn>]

aliases: login, add-user

Run \"npm help adduser\" for more info"
  ["Arguments"
   ("a" npmjs-login--auth-type)
   ("s" npmjs-login--scope)
   ("r" npmjs--registry-option)]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-ls--depth ()
  "Set argument --depth."
  :description "--depth "
  :argument "--depth "
  :reader 'transient-read-number-N0
  :class 'transient-option)


;;;###autoload (autoload 'npmjs-ls "npmjs.el" nil t)
(transient-define-prefix npmjs-ls ()
  "List installed packages.
Usage:
npm ls <package-spec>

Options:
[-a|--all] [--json] [-l|--long] [-p|--parseable] [-g|--global] [--depth <depth>]
[--omit <dev|optional|peer> [--omit <dev|optional|peer> ...]] [--link]
[--package-lock-only] [--no-unicode]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root] [--install-links]

alias: list

Run \"npm help ls\" for more info"
  :value (lambda ()
           (unless (npmjs-get-project-root)
             (list "--global")))
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("o" npmjs--omit-option)
   ("d" npmjs-ls--depth)]
  ["Switches"
   ("i" "install-links" "--install-links")
   ("r" "include-workspace-root" "--include-workspace-root")
   ("s" "workspaces" "--workspaces")
   ("n" "no-unicode" "--no-unicode")
   ("p" "package-lock-only" "--package-lock-only")
   ("l" "link" "--link")
   ("g" "global" "--global")
   ("P" "parseable" "--parseable")
   ("L" "long" "--long")
   ("j" "json" "--json")
   ("a" "all" "--all")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-ll "npmjs.el" nil t)
(transient-define-prefix npmjs-ll ()
  "List installed packages.
Usage:
npm ll [[<@scope>/]<pkg> ...]

Options:
[-a|--all] [--json] [-l|--long] [-p|--parseable] [-g|--global] [--depth <depth>]
[--omit <dev|optional|peer> [--omit <dev|optional|peer> ...]] [--link]
[--package-lock-only] [--no-unicode]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root] [--install-links]

alias: la

Run \"npm help ll\" for more info"
  :value (lambda ()
           (unless (npmjs-get-project-root)
             (list "--global")))
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("-" npmjs--workspace-option)
   ("o" npmjs--omit-option)
   ("m" npmjs--omit-option)
   ("d" npmjs--depth-option)]
  ["Switches"
   ("i" "install-links" "--install-links")
   ("r" "include-workspace-root" "--include-workspace-root")
   ("s" "workspaces" "--workspaces")
   ("n" "no-unicode" "--no-unicode")
   ("p" "package-lock-only" "--package-lock-only")
   ("l" "link" "--link")
   ("g" "global" "--global")
   ("P" "parseable" "--parseable")
   ("L" "long" "--long")
   ("j" "json" "--json")
   ("a" "all" "--all")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-link "npmjs.el" nil t)
(transient-define-prefix npmjs-link ()
  "Symlink a package folder.
Usage:
npm link [<package-spec>]

Options:
[-S|--save|--no-save|--save-prod|--save-dev|--save-optional|--save-peer|
--save-bundle]
[-E|--save-exact] [-g|--global] [--global-style] [--legacy-bundling]
[--strict-peer-deps] [--no-package-lock]
[--omit <dev|optional|peer> [--omit <dev|optional|peer> ...]] [--ignore-scripts]
[--no-audit] [--no-bin-links] [--no-fund] [--dry-run]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root] [--install-links]

alias: ln

Run \"npm help link\" for more info"
  :value (lambda ()
           (unless (npmjs-get-project-root)
             (list "--global")))
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("-" npmjs--workspace-option)
   ("o" npmjs--omit-option)
   ("m" npmjs--omit-option)]
  ["Switches"
   ("i" "install-links" "--install-links")
   ("r" "include-workspace-root" "--include-workspace-root")
   ("s" "workspaces" "--workspaces")
   ("d" "dry-run" "--dry-run")
   ("n" "no-fund" "--no-fund")
   ("b" "no-bin-links" "--no-bin-links")
   ("a" "no-audit" "--no-audit")
   ("g" "ignore-scripts" "--ignore-scripts")
   ("p" "no-package-lock" "--no-package-lock")
   ("t" "strict-peer-deps" "--strict-peer-deps")
   ("l" "legacy-bundling" "--legacy-bundling")
   ("y" "global-style" "--global-style")
   ("G" "global" "--global")
   ("E" "save-exact" "--save-exact")
   ("v" "save-bundle" "--save-bundle")
   ("e" "save-peer" "--save-peer")
   ("S" "save-optional" "--save-optional")
   ("A" "save-dev" "--save-dev")
   ("V" "save-prod" "--save-prod")
   ("N" "no-save" "--no-save")
   ("c" "save" "--save")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-owner "npmjs.el" nil t)
(transient-define-prefix npmjs-owner ()
  "Manage package owners.
Usage:
npm owner add <user> <package-spec>
npm owner rm <user> <package-spec>
npm owner ls <package-spec>

Options:
[--registry <registry>] [--otp <otp>]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces]

alias: author

Run \"npm help owner\" for more info"
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("-" npmjs--workspace-option)
   ("o" npmjs--otp-option)
   ("r" npmjs--registry-option)]
  ["Switches"
   ("s" "workspaces" "--workspaces")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-outdated "npmjs.el" nil t)
(transient-define-prefix npmjs-outdated ()
  "Check for outdated packages.
Usage:
npm outdated [<package-spec> ...]

Options:
[-a|--all] [--json] [-l|--long] [-p|--parseable] [-g|--global]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]

Run \"npm help outdated\" for more info"
  :value (lambda ()
           (unless (npmjs-get-project-root)
             (list "--global")))
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("-" npmjs--workspace-option)]
  ["Switches"
   ("g" "global" "--global")
   ("p" "parseable" "--parseable")
   ("l" "long" "--long")
   ("j" "json" "--json")
   ("a" "all" "--all")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-org "npmjs.el" nil t)
(transient-define-prefix npmjs-org ()
  "Manage orgs.
Usage:
npm org set orgname username [developer | admin | owner]
npm org rm orgname username
npm org ls orgname [<username>]

Options:
[--registry <registry>] [--otp <otp>] [--json] [-p|--parseable]

alias: ogr

Run \"npm help org\" for more info"
  ["Arguments"
   ("o" npmjs--otp-option)
   ("r" npmjs--registry-option)]
  ["Switches"
   ("p" "parseable" "--parseable")
   ("j" "json" "--json")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-prune "npmjs.el" nil t)
(transient-define-prefix npmjs-prune ()
  "Remove extraneous packages.
Usage:
npm prune [[<@scope>/]<pkg>...]

Options:
[--omit <dev|optional|peer> [--omit <dev|optional|peer> ...]] [--dry-run]
[--json] [--foreground-scripts] [--ignore-scripts]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root] [--install-links]

Run \"npm help prune\" for more info"
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("m" npmjs--omit-option)]
  ["Switches"
   ("i" "install-links" "--install-links")
   ("r" "include-workspace-root" "--include-workspace-root")
   ("s" "workspaces" "--workspaces")
   ("g" "ignore-scripts" "--ignore-scripts")
   ("f" "foreground-scripts" "--foreground-scripts")
   ("j" "json" "--json")
   ("d" "dry-run" "--dry-run")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-profile "npmjs.el" nil t)
(transient-define-prefix npmjs-profile ()
  "Change settings on your registry profile.
Usage:
npm profile enable-2fa [auth-only|auth-and-writes]
npm profile disable-2fa
npm profile get [<key>]
npm profile set <key> <value>

Options:
[--registry <registry>] [--json] [-p|--parseable] [--otp <otp>]

Run \"npm help profile\" for more info"
  ["Arguments"
   ("o" npmjs--otp-option)
   ("r" npmjs--registry-option)]
  ["Switches"
   ("p" "parseable" "--parseable")
   ("j" "json" "--json")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-prefix "npmjs.el" nil t)
(transient-define-prefix npmjs-prefix ()
  "Display prefix.
Usage:
npm prefix [-g]

Options:
[-g|--global]

Run \"npm help prefix\" for more info"
  :value (lambda ()
           (unless (npmjs-get-project-root)
             (list "--global")))
  ["Switches"
   ("g" "global" "--global")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-publish--access ()
  "Set argument --access."
  :description "--access "
  :argument "--access "
  :class 'transient-option
  :always-read t)

(transient-define-argument npmjs-publish--tag ()
  "Set argument --tag."
  :description "--tag "
  :argument "--tag "
  :class 'transient-option
  :always-read t)


;;;###autoload (autoload 'npmjs-publish "npmjs.el" nil t)
(transient-define-prefix npmjs-publish ()
  "Publish a package.
Usage:
npm publish <package-spec>

Options:
[--tag <tag>] [--access <restricted|public>] [--dry-run] [--otp <otp>]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root]

Run \"npm help publish\" for more info"
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("-" npmjs--workspace-option)
   ("o" npmjs--otp-option)
   ("a" npmjs-publish--access)
   ("t" npmjs-publish--tag)]
  ["Switches"
   ("i" "include-workspace-root" "--include-workspace-root")
   ("s" "workspaces" "--workspaces")
   ("d" "dry-run" "--dry-run")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-pkg "npmjs.el" nil t)
(transient-define-prefix npmjs-pkg ()
  "Manages your package.json.
Usage:
npm pkg set <key>=<value> [<key>=<value> ...]
npm pkg get [<key> [<key> ...]]
npm pkg delete <key> [<key> ...]
npm pkg set [<array>[<index>].<key>=<value> ...]
npm pkg set [<array>[].<key>=<value> ...]

Options:
[-f|--force] [--json]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces]

Run \"npm help pkg\" for more info"
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("-" npmjs--workspace-option)]
  ["Switches"
   ("s" "workspaces" "--workspaces")
   ("j" "json" "--json")
   ("f" "force" "--force")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-ping "npmjs.el" nil t)
(transient-define-prefix npmjs-ping ()
  "Ping npm registry.
Usage:
npm ping

Options:
[--registry <registry>]

Run \"npm help ping\" for more info"
  ["Arguments"
   ("r" npmjs--registry-option)]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-pack--pack-destination ()
  "Set argument --pack-destination."
  :description "--pack-destination "
  :argument "--pack-destination "
  :class 'transient-option
  :always-read t)


;;;###autoload (autoload 'npmjs-pack "npmjs.el" nil t)
(transient-define-prefix npmjs-pack ()
  "Create a tarball from a package.
Usage:
npm pack <package-spec>

Options:
[--dry-run] [--json] [--pack-destination <pack-destination>]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root]

Run \"npm help pack\" for more info"
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("-" npmjs--workspace-option)
   ("p" npmjs-pack--pack-destination)]
  ["Switches"
   ("i" "include-workspace-root" "--include-workspace-root")
   ("s" "workspaces" "--workspaces")
   ("j" "json" "--json")
   ("d" "dry-run" "--dry-run")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-restart--script-shell ()
  "Set argument --script-shell."
  :description "--script-shell "
  :argument "--script-shell "
  :class 'transient-option
  :always-read t)


;;;###autoload (autoload 'npmjs-restart "npmjs.el" nil t)
(transient-define-prefix npmjs-restart ()
  "Restart a package.
Usage:
npm restart [-- <args>]

Options:
[--ignore-scripts] [--script-shell <script-shell>]

Run \"npm help restart\" for more info"
  ["Arguments"
   ("s" npmjs-restart--script-shell)]
  ["Switches"
   ("i" "ignore-scripts" "--ignore-scripts")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-repo--browser ()
  "Set argument --browser."
  :description "--browser "
  :argument "--browser "
  :class 'transient-option
  :always-read t)


;;;###autoload (autoload 'npmjs-repo "npmjs.el" nil t)
(transient-define-prefix npmjs-repo ()
  "Open package repository page in the browser.
Usage:
npm repo [<pkgname> [<pkgname> ...]]

Options:
[--no-browser|--browser <browser>] [--registry <registry>]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root]

Run \"npm help repo\" for more info"
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("-" npmjs--workspace-option)
   ("r" npmjs--registry-option)
   ("b" npmjs-repo--browser)]
  ["Switches"
   ("i" "include-workspace-root" "--include-workspace-root")
   ("s" "workspaces" "--workspaces")
   ("n" "no-browser" "--no-browser")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-rebuild "npmjs.el" nil t)
(transient-define-prefix npmjs-rebuild ()
  "Rebuild a package.
Usage:
npm rebuild [<package-spec>] ...]

Options:
[-g|--global] [--no-bin-links] [--foreground-scripts] [--ignore-scripts]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root] [--install-links]

alias: rb

Run \"npm help rebuild\" for more info"
  :value (lambda ()
           (unless (npmjs-get-project-root)
             (list "--global")))
  ["Arguments"
   ("w" npmjs--workspace-option)]
  ["Switches"
   ("i" "install-links" "--install-links")
   ("r" "include-workspace-root" "--include-workspace-root")
   ("s" "workspaces" "--workspaces")
   ("g" "ignore-scripts" "--ignore-scripts")
   ("f" "foreground-scripts" "--foreground-scripts")
   ("n" "no-bin-links" "--no-bin-links")
   ("G" "global" "--global")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-run-script--script-shell ()
  "Set argument --script-shell."
  :description "--script-shell "
  :argument "--script-shell "
  :class 'transient-option
  :always-read t)

(transient-define-argument npmjs-run-script--workspace ()
  "Set argument --workspace."
  :description "--workspace "
  :argument "--workspace "
  :class 'transient-option
  :always-read t)


;;;###autoload (autoload 'npmjs-run-script "npmjs.el" nil t)
(transient-define-prefix npmjs-run-script ()
  "Run arbitrary package scripts.
Usage:
npm run-script <command> [-- <args>]

Options:
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root] [--if-present] [--ignore-scripts]
[--foreground-scripts] [--script-shell <script-shell>]

aliases: run, rum, urn

Run \"npm help run-script\" for more info"
  ["Arguments"
   ("s" npmjs-run-script--script-shell)
   ("w" npmjs-run-script--workspace)]
  ["Switches"
   ("f" "foreground-scripts" "--foreground-scripts")
   ("i" "ignore-scripts" "--ignore-scripts")
   ("p" "if-present" "--if-present")
   ("r" "include-workspace-root" "--include-workspace-root")
   ("W" "workspaces" "--workspaces")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-root "npmjs.el" nil t)
(transient-define-prefix npmjs-root ()
  "Display npm root.
Usage:
npm root

Options:
[-g|--global]

Run \"npm help root\" for more info"
  ["Switches"
   ("g" "global" "--global")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-search--searchexclude ()
  "Set argument --searchexclude."
  :description "--searchexclude "
  :argument "--searchexclude "
  :class 'transient-option)

(transient-define-argument npmjs-search--searchopts ()
  "Set argument --searchopts."
  :description "--searchopts "
  :argument "--searchopts "
  :class 'transient-option)


;;;###autoload (autoload 'npmjs-search "npmjs.el" nil t)
(transient-define-prefix npmjs-search ()
  "Search for packages.
Usage:
npm search [search terms ...]

Options:
[-l|--long] [--json] [--color|--no-color|--color always] [-p|--parseable]
[--no-description] [--searchopts <searchopts>] [--searchexclude <searchexclude>]
[--registry <registry>] [--prefer-online] [--prefer-offline] [--offline]

aliases: find, s, se

Run \"npm help search\" for more info"
  :value (lambda ()
           (append
            (unless (npmjs-online-p)
              (list "--offline"))
            (list "--parseable"
                  "--no-color"
                  "--long")))
  :incompatible '(("--color" "--no-color"))
  ["Arguments"
   ("r" npmjs--registry-option)
   ("s" npmjs-search--searchexclude)
   ("e" npmjs-search--searchopts)]
  ["Switches"
   ("o" "offline" "--offline")
   ("p" "prefer-offline" "--prefer-offline")
   ("f" "prefer-online" "--prefer-online")
   ("n" "no-description" "--no-description")
   ("-" "parseable" "--parseable")
   ("c" "color" "--color")
   ("l" "no-color" "--no-color")
   ("C" "color" "--color")
   ("j" "json" "--json")
   ("L" "long" "--long")]
  ["Actions"
   ("RET" "Run" npmjs-search-package)
   ("<return>" "Run" npmjs-search-package)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-set-script--workspace ()
  "Set argument --workspace."
  :description "--workspace "
  :argument "--workspace "
  :class 'transient-option
  :always-read t)



;;;###autoload (autoload 'npmjs-set-script "npmjs.el" nil t)
(transient-define-prefix npmjs-set-script ()
  "Set tasks in the scripts section of package.json, deprecated.
Usage:
npm set-script [<script>] [<command>]

Options:
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root]

Run \"npm help set-script\" for more info"
  ["Arguments"
   ("w" npmjs-set-script--workspace)
   ("-" npmjs-set-script--workspace)]
  ["Switches"
   ("i" "include-workspace-root" "--include-workspace-root")
   ("s" "workspaces" "--workspaces")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-set "npmjs.el" nil t)
(transient-define-prefix npmjs-set ()
  "Set a value in the npm configuration.
Usage:
npm set <key>=<value> [<key>=<value> ...] (See `npm config`)

Run \"npm help set\" for more info"
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-shrinkwrap "npmjs.el" nil t)
(transient-define-prefix npmjs-shrinkwrap ()
  "Lock down dependency versions for publication.
Usage:
npm shrinkwrap

Run \"npm help shrinkwrap\" for more info"
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-start--script-shell ()
  "Set argument --script-shell."
  :description "--script-shell "
  :argument "--script-shell "
  :class 'transient-option
  :always-read t)


;;;###autoload (autoload 'npmjs-start "npmjs.el" nil t)
(transient-define-prefix npmjs-start ()
  "Start a package.
Usage:
npm start [-- <args>]

Options:
[--ignore-scripts] [--script-shell <script-shell>]

Run \"npm help start\" for more info"
  ["Arguments"
   ("s" npmjs-start--script-shell)]
  ["Switches"
   ("i" "ignore-scripts" "--ignore-scripts")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-stars "npmjs.el" nil t)
(transient-define-prefix npmjs-stars ()
  "View packages marked as favorites.
Usage:
npm stars [<user>]

Options:
[--registry <registry>]

Run \"npm help stars\" for more info"
  ["Arguments"
   ("r" npmjs--registry-option)]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-star "npmjs.el" nil t)
(transient-define-prefix npmjs-star ()
  "Mark your favorite packages.
Usage:
npm star [<package-spec>...]

Options:
[--registry <registry>] [--no-unicode] [--otp <otp>]

Run \"npm help star\" for more info"
  ["Arguments"
   ("o" npmjs--otp-option)
   ("r" npmjs--registry-option)]
  ["Switches"
   ("n" "no-unicode" "--no-unicode")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-stop--script-shell ()
  "Set argument --script-shell."
  :description "--script-shell "
  :argument "--script-shell "
  :class 'transient-option
  :always-read t)


;;;###autoload (autoload 'npmjs-stop "npmjs.el" nil t)
(transient-define-prefix npmjs-stop ()
  "Stop a package.
Usage:
npm stop [-- <args>]

Options:
[--ignore-scripts] [--script-shell <script-shell>]

Run \"npm help stop\" for more info"
  ["Arguments"
   ("s" npmjs-stop--script-shell)]
  ["Switches"
   ("i" "ignore-scripts" "--ignore-scripts")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-test--script-shell ()
  "Set argument --script-shell."
  :description "--script-shell "
  :argument "--script-shell "
  :class 'transient-option
  :always-read t)


;;;###autoload (autoload 'npmjs-test "npmjs.el" nil t)
(transient-define-prefix npmjs-test ()
  "Test a package.
Usage:
npm test [-- <args>]

Options:
[--ignore-scripts] [--script-shell <script-shell>]

aliases: tst, t

Run \"npm help test\" for more info"
  ["Arguments"
   ("s" npmjs-test--script-shell)]
  ["Switches"
   ("i" "ignore-scripts" "--ignore-scripts")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-team "npmjs.el" nil t)
(transient-define-prefix npmjs-team ()
  "Manage organization teams and team memberships.
Usage:
npm team create <scope:team> [--otp <otpcode>]
npm team destroy <scope:team> [--otp <otpcode>]
npm team add <scope:team> <user> [--otp <otpcode>]
npm team rm <scope:team> <user> [--otp <otpcode>]
npm team ls <scope>|<scope:team>

Options:
[--registry <registry>] [--otp <otp>] [-p|--parseable] [--json]

Run \"npm help team\" for more info"
  ["Arguments"
   ("o" npmjs--otp-option)
   ("t" npmjs--otp-option)
   ("p" npmjs--otp-option)
   ("O" npmjs--otp-option)
   ("T" npmjs--otp-option)
   ("r" npmjs--registry-option)]
  ["Switches"
   ("j" "json" "--json")
   ("-" "parseable" "--parseable")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])

(transient-define-argument npmjs-token--cidr ()
  "Set argument --cidr."
  :description "--cidr "
  :argument "--cidr "
  :class 'transient-option
  :always-read t)


;;;###autoload (autoload 'npmjs-token "npmjs.el" nil t)
(transient-define-prefix npmjs-token ()
  "Manage your authentication tokens.
Usage:
npm token list
npm token revoke <id|token>
npm token create [--read-only] [--cidr=list]

Options:
[--read-only] [--cidr <cidr> [--cidr <cidr> ...]] [--registry <registry>]
[--otp <otp>]

Run \"npm help token\" for more info"
  ["Arguments"
   ("o" npmjs--otp-option)
   ("r" npmjs--registry-option)
   ("c" npmjs-token--cidr)]
  ["Switches"
   ("d" "cidr" "--cidr")
   ("e" "read-only" "--read-only")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-unstar "npmjs.el" nil t)
(transient-define-prefix npmjs-unstar ()
  "Remove an item from your favorite packages.
Usage:
npm unstar [<package-spec>...]

Options:
[--registry <registry>] [--no-unicode] [--otp <otp>]

Run \"npm help unstar\" for more info"
  ["Arguments"
   ("o" npmjs--otp-option)
   ("r" npmjs--registry-option)]
  ["Switches"
   ("n" "no-unicode" "--no-unicode")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-unpublish "npmjs.el" nil t)
(transient-define-prefix npmjs-unpublish ()
  "Remove a package from the registry.
Usage:
npm unpublish [<package-spec>]

Options:
[--dry-run] [-f|--force]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces]

Run \"npm help unpublish\" for more info"
  ["Arguments"
   ("w" npmjs--workspace-option)]
  ["Switches"
   ("s" "workspaces" "--workspaces")
   ("f" "force" "--force")
   ("d" "dry-run" "--dry-run")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-uninstall "npmjs.el" nil t)
(transient-define-prefix npmjs-uninstall ()
  "Remove a package.
Usage:
npm uninstall [<@scope>/]<pkg>...

Options:
[-S|--save|--no-save|--save-prod|--save-dev|--save-optional|--save-peer|
--save-bundle]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root] [--install-links]

aliases: unlink, remove, rm, r, un

Run \"npm help uninstall\" for more info"
  :value (lambda ()
           (unless (npmjs-get-project-root)
             (list "--global")))
  ["Arguments"
   ("w" npmjs--workspace-option)]
  ["Switches"
   ("i" "install-links" "--install-links")
   ("r" "include-workspace-root" "--include-workspace-root")
   ("s" "workspaces" "--workspaces")
   ("b" "save-bundle" "--save-bundle")
   ("g" "global" "--global")
   ("p" "save-peer" "--save-peer")
   ("o" "save-optional" "--save-optional")
   ("d" "save-dev" "--save-dev")
   ("P" "save-prod" "--save-prod")
   ("n" "no-save" "--no-save")
   ("S" "save" "--save")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-update "npmjs.el" nil t)
(transient-define-prefix npmjs-update ()
  "Update packages.
Usage:
npm update [<pkg>...]

Options:
[-S|--save|--no-save|--save-prod|--save-dev|--save-optional|--save-peer|
--save-bundle]
[-g|--global] [--global-style] [--legacy-bundling]
[--omit <dev|optional|peer> [--omit <dev|optional|peer> ...]]
[--strict-peer-deps] [--no-package-lock] [--foreground-scripts]
[--ignore-scripts] [--no-audit] [--no-bin-links] [--no-fund] [--dry-run]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root] [--install-links]

aliases: up, upgrade, udpate

Run \"npm help update\" for more info"
  :value (lambda ()
           (unless (npmjs-get-project-root)
             (list "--global")))
  ["Arguments"
   ("w" npmjs--workspace-option)
   ("o" npmjs--omit-option)]
  ["Switches"
   ("c" "save" "--save")
   ("N" "no-save" "--no-save")
   ("V" "save-prod" "--save-prod")
   ("A" "save-dev" "--save-dev")
   ("S" "save-optional" "--save-optional")
   ("e" "save-peer" "--save-peer")
   ("v" "save-bundle" "--save-bundle")
   ("G" "global" "--global")
   ("y" "global-style" "--global-style")
   ("l" "legacy-bundling" "--legacy-bundling")
   ("t" "strict-peer-deps" "--strict-peer-deps")
   ("p" "no-package-lock" "--no-package-lock")
   ("f" "foreground-scripts" "--foreground-scripts")
   ("g" "ignore-scripts" "--ignore-scripts")
   ("a" "no-audit" "--no-audit")
   ("b" "no-bin-links" "--no-bin-links")
   ("n" "no-fund" "--no-fund")
   ("d" "dry-run" "--dry-run")
   ("s" "workspaces" "--workspaces")
   ("r" "include-workspace-root" "--include-workspace-root")
   ("i" "install-links" "--install-links")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-view "npmjs.el" nil t)
(transient-define-prefix npmjs-view ()
  "View registry info.
Usage:
npm view [<package-spec>] [<field>[.subfield]...]

Options:
[--json] [-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--include-workspace-root]

aliases: info, show, v

Run \"npm help view\" for more info"
  ["npm view"
   ("w" npmjs--workspace-option)]
  ["Switches"
   ("i" "include-workspace-root" "--include-workspace-root")
   ("s" "workspaces" "--workspaces")
   ("j" "json" "--json")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])



(transient-define-argument npmjs-version-option ()
  :class 'transient-switches
  :description "Version type"
  :argument-format "%s"
  :init-value (lambda (ob)
                (when (not (slot-value ob 'value))
                  (setf (slot-value ob 'value)
                        "patch")))
  :argument-regexp
  "prerelease\\|prepatch\\|preminor\\|premajor\\|patch\\|minor\\|major\\|from-git"
  :choices '("prerelease"
             "prepatch"
             "preminor"
             "premajor"
             "patch"
             "minor"
             "major"
             "from-git"))

;;;###autoload (autoload 'npmjs-version "npmjs.el" nil t)
(transient-define-prefix npmjs-version ()
  "Bump a package version.
Usage:
npm version
[<newversion> | major | minor | patch | premajor | preminor | prepatch
| prerelease | from-git]

Options:
[--allow-same-version] [--no-commit-hooks] [--no-git-tag-version] [--json]
[--preid prerelease-id] [--sign-git-tag]
[-w|--workspace <workspace-name> [-w|--workspace <workspace-name> ...]]
[-ws|--workspaces] [--no-workspaces-update] [--include-workspace-root]

alias: verison

Run \"npm help version\" for more info"
  ["Arguments"
   ("w" npmjs-version-option)]
  ["Switches"
   ("i" "include-workspace-root" "--include-workspace-root")
   ("n" "no-workspaces-update" "--no-workspaces-update")
   ("s" "workspaces" "--workspaces")
   ("g" "sign-git-tag" "--sign-git-tag")
   ("p" "preid" "--preid")
   ("j" "json" "--json")
   ("t" "no-git-tag-version" "--no-git-tag-version")
   ("c" "no-commit-hooks" "--no-commit-hooks")
   ("a" "allow-same-version" "--allow-same-version")]
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-whoami "npmjs.el" nil t)
(transient-define-prefix npmjs-whoami ()
  "Display npm username."
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])


;;;###autoload (autoload 'npmjs-get "npmjs.el" nil t)
(transient-define-prefix npmjs-get ()
  "Get a value from the npm configuration.
Usage:
npm get [<key> ...] (See `npm config`)

Run \"npm help get\" for more info"
  ["Actions"
   ("RET" "Run" npmjs-done)
   ("<return>" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)])



;;;###autoload (autoload 'npmjs "npmjs.el" nil t)
(transient-define-prefix npmjs ()
  "Npm <command>.
Usage:

npm install        install all the dependencies in your project
npm install <foo>  add the <foo> dependency to your project
npm test           run this project's tests
npm run <foo>      run the script named <foo>
npm <command> -h   quick help on <command>
npm -l             display usage info for all commands
npm help <term>    search for help on <term>
npm help npm       more involved overview

All commands:"
	[:description
   (lambda ()
     (concat "NPM commands: "
             (let* ((default-global (npmjs-nvm-strip-prefix
                                     (npmjs-current-default-node-version)))
                    (current-global
                     (when (get-buffer (npmjs--get-global-buffer-name))
                       (ignore-errors
                         (npmjs-nvm-strip-prefix
                          (buffer-local-value
                           'npmjs--current-node-version
                           (get-buffer
                            (npmjs--get-global-buffer-name)))))))
                    (project-node
                     (ignore-errors
                       (npmjs-nvm-strip-prefix
                        (npmjs-nvm-get-nvmrc-required-node-version))))
                    (curr-node (npmjs-nvm-strip-prefix
                                npmjs--current-node-version))
                    (cands
                     (delq nil (seq-uniq
                                (list
                                 default-global
                                 current-global
                                 project-node
                                 curr-node))))
                    (annotf (lambda (it)
                              (format
															 (cond ((equal it default-global)
                                      "(System global %s)")
                                     ((equal it current-global)
                                      "(Current global %s)")
                                     ((equal it project-node)
                                      "(nvmrc %s) ")
                                     ((equal it curr-node)
                                      "(Buffer local node %s) "))
                               it))))
               (mapconcat annotf cands "\s"))))
	 [("ac" "access" npmjs-access)
		("ad" "adduser" npmjs-adduser)
		("au" "audit" npmjs-audit)
		("bu" "bugs" npmjs-bugs)
		("ca" "cache" npmjs-cache)
		("ci" "ci" npmjs-ci :inapt-if-not npmjs-get-project-root)
		("co" "completion" npmjs-completion)
		("cc" "config" npmjs-config)
		("de" "dedupe" npmjs-dedupe :inapt-if-not npmjs-get-project-root)
		("dd" "deprecate" npmjs-deprecate)
		("di" "diff" npmjs-diff)
		("dt" "dist-tag" npmjs-dist-tag)
		("do" "docs" npmjs-docs)
		("Do" "doctor" npmjs-doctor)
		("ed" "edit" npmjs-edit)
		("ex" "exec" npmjs-exec)
		("ee" "explain" npmjs-explain)
		("Ex" "explore" npmjs-explore)
		("fi" "find-dupes" npmjs-find-dupes)
		("fu" "fund" npmjs-fund)
		("ge" "get" npmjs-get)]
	 [("he" "help" npmjs-help)
		("ho" "hook" npmjs-hook)
		("in" "init" npmjs-init)
		("ii" "install" npmjs-install)
		("it" "install-test" npmjs-install-test)
		("li" "link" npmjs-link)
		("ll" "ll" npmjs-ll)
		("lo" "login" npmjs-login)
		("Lo" "logout" npmjs-logout)
		("ls" "ls" npmjs-ls)
		("or" "org" npmjs-org)
		("ou" "outdated" npmjs-outdated)
		("ow" "owner" npmjs-owner)
		("pa" "pack" npmjs-pack)
		("pi" "ping" npmjs-ping)
		("pk" "pkg" npmjs-pkg)
		("pr" "prefix" npmjs-prefix)
		("pp" "profile" npmjs-profile)
		("Pr" "prune" npmjs-prune)
		("pu" "publish" npmjs-publish :inapt-if-not npmjs-get-project-root)
		("re" "rebuild" npmjs-rebuild)]
	 [("rr" "repo" npmjs-repo)
		("Re" "restart" npmjs-restart)
		("ro" "root" npmjs-root)
		("ru" "run-script" npmjs-run-script :inapt-if-not npmjs-get-project-root)
		("se" "search" npmjs-search)
		("ss" "set" npmjs-set)
		("sh" "shrinkwrap" npmjs-shrinkwrap)
		("st" "star" npmjs-star)
		("St" "stars" npmjs-stars)
		("sT" "start" npmjs-start :inapt-if-not
		 (lambda nil
			 (or
				(alist-get 'start
									 (alist-get 'scripts
															(npmjs-get-package-json-alist)))
				(when-let ((proj
										(npmjs-get-project-root)))
					(file-exists-p
					 (expand-file-name "server.js" proj))))))
		("42" "stop" npmjs-stop :inapt-if-not npmjs-get-project-root)
		("te" "team" npmjs-team)
		("tt" "test" npmjs-test)
		("to" "token" npmjs-token)
		("un" "uninstall" npmjs-uninstall)
		("uu" "unpublish" npmjs-unpublish)
		("Un" "unstar" npmjs-unstar)
		("up" "update" npmjs-update)
		("ve" "version" npmjs-version)
		("vi" "view" npmjs-view)
		("wh" "whoami" npmjs-whoami)]
   [:if npmjs-nvm-path
        "NVM"
        ("ni" "Install other node version" npmjs-nvm-install-node-version)
        ("nl" "List installed node" npmjs-nvm-jump-to-installed-node)]])

(provide 'npmjs)
;;; npmjs.el ends here