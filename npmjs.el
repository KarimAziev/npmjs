;;; npmjs.el --- Command dispatcher for NPM -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>\

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/npmjs
;; Version: 0.1.0-git
;; Keywords: tools
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later

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
(require 'tray-builder)

(defvar json-object-type)
(defvar json-array-type)
(defvar json-null)
(defvar json-false)


(defcustom npmjs-common-buffer-name-function	'npmjs-common-create-unique-buffer-name
  "Buffer name for `npm' command, or function which return buffer name.
The function takes three arguments, ROOT, NPM-COMMAND, ARGS.
ROOT is project root directory.  NPM-COMMAND is npm command string.
ARGS is list of arguments passed to npm command.

You can use `npmjs-common-create-unique-buffer-name' to use unique buffer name
among all sesstions."
  :group 'npmjs
  :type '(choice
          (string :tag "Use same buffer through all sessions")
          (const :tag "Use unique buffer name among all sessions"
                 npmjs-common-create-unique-buffer-name)
          function))

;; nvm
(defcustom npmjs-nvm-dir (or (getenv "NVM_DIR")
                             (expand-file-name "~/.nvm"))
  "Full path to Nvm installation directory."
  :group 'npmjs
  :type 'directory)

(defcustom npmjs-inhibit-prefix-cache nil
  "Whether to always reevalulate prefixes. It is mostly for debug purposes."
  :group 'npmjs
  :type 'boolean)

(defcustom npmjs-omit-commmands '("stars"
																	"star"
																	"completion"
																	"bugs"
																	"help"
																	"help-search"
																	"fund"
																	"search"
																	"explore"
																	"unstar"
																	"pkg")
  "List of commands to omit in transient menu."
	:group 'npmjs
  :type '(repeat string))

(defcustom npmjs-compile-command 'npmjs
  "Command to run when compile and friends are called."
  :group 'npmjs
  :type 'function)

(defcustom npmjs-repeat-compile-command 'npmjs-repeat
  "Command to run when recompile and friends are called."
  :group 'npmjs
  :type 'function)

(defcustom npmjs-started-hook nil
  "Hooks to run after a npmjs process starts."
  :group 'npmjs
  :type 'hook)

(defvar npmjs--history nil
  "History for npmjs invocations.")
(defcustom npmjs-finished-hook nil
  "Hooks to run after a npmjs process finishes."
  :group 'npmjs
  :type 'hook)

(defcustom npmjs-setup-hook nil
  "Hooks to run before a npmjs process starts."
  :group 'npmjs
  :type 'hook)

(defvar-local npmjs--current-command nil)
(defvar-local npmjs--current-node-version nil)

(defun npmjs--plist-remove-nils (plist)
	"Return the keys in PLIST."
	(let* ((result (list 'head))
         (last result))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (new (and val (list key val))))
        (when new
          (setcdr last new)
          (setq last (cdr new)))))
    (cdr result)))

(defun npmjs-unquote (exp)
	"Return EXP unquoted."
	(declare (pure t)
           (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

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

(defmacro npmjs-nvm-with-env-vars (version &rest body)
	"Set nvm variables for node VERSION in the environment and execute BODY.
VARIABLES is a list of variable settings of the form (VAR VALUE),
where VAR is the name of the variable (a string) and VALUE
is its value (also a string).

The previous values will be restored upon exit."
	(declare
	 (indent defun))
	`(let ((process-environment (copy-sequence process-environment))
				 (vars (npmjs-nvm-get-env-vars ,version)))
		 (dolist (elem vars)
			 (setenv (car elem)
							 (cadr elem)))
		 ,@body))

(defmacro npmjs-nvm-with-current-node-version (&rest body)
	"Set nvm variables for node VERSION in the environment and execute BODY.
VARIABLES is a list of variable settings of the form (VAR VALUE),
where VAR is the name of the variable (a string) and VALUE
is its value (also a string).

The previous values will be restored upon exit."
	`(npmjs-nvm-with-env-vars
		(setq npmjs--current-node-version (or npmjs--current-node-version
																					(npmjs-confirm-node-version)))
		,@body))

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
      (message "npmjs: Installing nvm-sh: %s"
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
  "v[0-9]+\\.[0-9]+\\.[0-9]+"
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
                              "(Buffer local node) ")
														 ((member item installed)
															"Installed")))
                      (meta (cdr (assoc item node-versions))))
                  (concat " "
                          (string-join (delete nil (list status
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
    (if (member version (mapcar (lambda (it)
																	(npmjs-nvm-strip-prefix (car it)))
																(npmjs-nvm--installed-versions)))
        (setq npmjs--current-node-version version)
      (npmjs-exec-in-dir (read-string "Run?" (string-join
                                              (list "source" nvm-path "&&"
                                                    "nvm"
                                                    "install"
                                                    version
                                                    "--reinstall-packages-from=current")
                                              "\s"))
                         default-directory
                         (lambda (&rest _)
													 (setq npmjs--current-node-version version)
                           (npmjs-run-as-comint "node -v"))))))

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
             (string-match-p "v?[0-9]+\\(\\.[0-9]+\\(\\.[0-9]+\\)?\\)?$" short))
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


(defun npmjs-nvm-get-env-vars (version)
	"Return alist of new environment for node VERSION."
	(when-let ((version-path (cdr (npmjs-nvm-find-exact-version-for version))))
    (let* ((env-flags
            (mapcar
             (lambda (it)
               (list
                (car it)
                (concat version-path "/" (cadr it))))
             '(("NVM_BIN" "bin")
               ("NVM_PATH" "lib/node")
               ("NVM_INC" "include/node"))))
           (path-re (concat "^"
                            (concat (or (getenv "NVM_DIR")
                                        (expand-file-name "~/.nvm"))
                                    "/\\(?:versions/node/\\|versions/io.js/\\)?")
                            "v[0-9]+\\.[0-9]+\\.[0-9]+" "/bin/?$"))
           (new-bin-path (expand-file-name  "bin/" version-path))
           (paths
            (cons
             new-bin-path
             (seq-remove
              (lambda (path)
                (if path (string-match-p path-re path) t))
              (parse-colon-path (getenv "PATH")))))
           (new-path (list "PATH" (string-join paths path-separator)))
           (flags (append env-flags (list new-path))))
			flags)))



(defun npmjs-nvm-get-env-for-node (version)
	"Return alist of new environment for node VERSION."
	(when-let* ((flags (npmjs-nvm-get-env-vars version)))
    (let ((regexp (mapconcat #'identity (mapcar #'car flags)
														 "\\|")))
      (append (mapcar (lambda (it)
                        (concat (car it) "=" (cadr it)))
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

(defun npmjs-get-node-version-description ()
	"Return description of current node versions."
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
    (mapconcat annotf cands "\s")))


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
	(condition-case nil
      (let* ((json-object-type (or json-type 'alist))
						 (json-array-type 'list)
						 (cache (gethash (format "%s:%s" file json-object-type)
                             npmjs-json-hash))
             (cache-tick (and cache (plist-get cache :tick)))
             (tick (file-attribute-modification-time
										(file-attributes
                     file
                     'string)))
             (content-json))
        (when (or (null cache)
                  (not (equal tick cache-tick)))
          (setq content-json
                (with-temp-buffer
                  (insert-file-contents file)
                  (npmjs-json-parse
                   (buffer-substring-no-properties (point-min)
                                                   (point-max)))))
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
         (let ((output
								(when-let ((buffer (process-buffer process)))
                  (with-current-buffer
                      (process-buffer process)
                    (buffer-string)))))
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

(defun npmjs-get-workspaces ()
	"Look up directory hierarchy for directories containing package.json.
Return list of project roots."
	(when-let* ((proj-root (car (npmjs-get-project-roots)))
							(package-json-root (npmjs-read-json (expand-file-name
																									 "package.json" proj-root)
																									'alist))
							(workspaces (alist-get 'workspaces package-json-root)))
		(let ((result))
			(dolist (pattern workspaces)
				(cond ((and (file-exists-p pattern)
										(file-name-absolute-p pattern))
							 (push (if (file-directory-p pattern)
												 pattern
											 (file-name-directory pattern))
										 result))
							(t
							 (let* ((dirs (file-expand-wildcards
														 (concat proj-root pattern))))
								 (setq result (append result dirs))))))
			result)))

(defun npmjs-workspace-reader (&optional prompt initial-input history)
	"Read workspace with PROMPT, INITIAL-INPUT and HISTORY."
	(let ((dirs (npmjs-get-workspaces)))
		(completing-read-multiple (or prompt "Workspace: ") dirs nil nil
															initial-input history)))

(defun npmjs-get-project-roots ()
	"Look up directory hierarchy for directories containing package.json.
Return list of project roots."
	(let ((project-root (npmjs-get-project-root))
				(roots))
		(while project-root
			(push project-root roots)
			(setq project-root
						(let ((default-directory (expand-file-name "../" project-root)))
							(npmjs-get-project-root))))
		roots))



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


(defun npmjs-confirm-node-version (&optional all)
	"Confirm which node version to use.
If ALL, read all installed versions."
	(let* ((installed
					(when all
						(mapcar (lambda (it)
											(npmjs-nvm-strip-prefix (car it)))
										(npmjs-nvm--installed-versions))))
				 (default-global (npmjs-current-default-node-version))
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
                   (append (delq nil
																 (list
																	default-global
																	current-global
																	project-node
																	curr-node))
													 installed))))
         (annotf (lambda (it)
                   (cond ((equal it default-global)
                          " System Global")
                         ((equal it current-global)
                          " Current global")
                         ((equal it project-node)
                          "required by (nvmrc) ")
                         ((equal it curr-node)
                          " Buffer local node ")))))
		(setq npmjs--current-node-version
					(if (<= (length cands) 1)
							(car cands)
						(completing-read "Which node to use?"
														 (lambda (str pred
																					action)
															 (if (eq action
																			 'metadata)
																	 `(metadata
																		 (annotation-function
																			.
																			,annotf))
																 (complete-with-action
																	action
																	cands
																	str
																	pred))))))))

;;;###autoload
(defun npmjs-other-node-version ()
	"Use other node version."
	(interactive)
	(npmjs-confirm-node-version t)
	(when transient-current-command
		(npmjs)))

;;;###autoload
(defun npmjs-use-switch-system-node-version (version)
	"Change internal environment to use VERSION of node."
	(interactive (list (npmjs-confirm-node-version t)))
	(let ((vars (npmjs-nvm-get-env-vars version)))
		(dolist (elem vars)
			(setenv
			 (car elem)
			 (cadr elem)))
		(npmjs)))



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
                                  npm-command)))
          (npmjs-run-as-comint
           npm-command)))
    (npmjs-run-as-comint npm-command)))


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
	(if npmjs--current-node-version
			(let ((env (npmjs-nvm-get-env-for-node
                  npmjs--current-node-version))
            (nvm-path (npmjs-nvm-path)))
        (if (and (not env)
                 (yes-or-no-p
                  (format
                   "This project requires node %s, which is not installed. Install?"
                   npmjs--current-node-version)))
            (npmjs-exec-in-dir (string-join
                                (list "source" nvm-path "&&" "nvm"
                                      "install"
                                      (npmjs-nvm-strip-prefix
																			 npmjs--current-node-version)
                                      "--reinstall-packages-from=current")
                                "\s")
                               default-directory
                               (lambda (&rest _)
                                 (npmjs-run-as-comint
                                  npm-command
																	npmjs--current-node-version)))
          (npmjs-run-as-comint
           npm-command)))
		(npmjs-run-as-comint npm-command)))


(defvar npmjs-last-commands (make-hash-table :test 'equal)
  "Last executed command lines, per project.")

;;;###autoload
(defun npmjs-repeat ()
	"Run npmjs with the same argument as the most recent invocation.

With a prefix ARG, allow editing."
	(interactive)
  (let ((command (gethash
                  (npmjs-get-project-root)
                  npmjs-last-commands)))
    (when npmjs--current-command
      ;; existing jest-mode buffer; reuse command
      (setq command npmjs--current-command))
    (unless command
      (user-error "No previous npmjs run for this project"))
    (npmjs-run-as-comint
     command)))

(defun npmjs-run-as-comint (command &optional version)
	"Run a npmjs comint session for COMMAND with ARGS and VERSION."
	(let* ((version  (or version
											 npmjs--current-node-version
											 (npmjs-nvm-strip-prefix
												(string-trim (shell-command-to-string "node -v")))))
				 (buffer (npmjs--get-buffer))
         (process (get-buffer-process buffer)))
		(with-current-buffer buffer
			(npmjs-nvm-with-env-vars
			 (setq npmjs--current-node-version version)
			 (when (comint-check-proc buffer)
				 (unless (or compilation-always-kill
										 (yes-or-no-p
											"Kill running npmjs process?"))
					 (user-error "Aborting; npmjs still running")))
			 (when process
				 (delete-process process))
			 (erase-buffer)
			 (setq npmjs--current-node-version version)
			 (unless (eq major-mode 'npmjs-mode)
				 (npmjs-mode))
			 (compilation-forget-errors)
			 (insert (format "cwd: %s\ncmd: %s\n\n"
											 default-directory command))
			 (setq npmjs--current-command command)
			 (run-hooks 'npmjs-setup-hook)
			 (make-comint-in-buffer "npmjs" buffer "sh" nil
															"-c" command)
			 (setq npmjs--current-node-version
						 (npmjs-nvm-strip-prefix
							(string-trim
							 (shell-command-to-string
								"node -v"))))
			 (run-hooks 'npmjs-started-hook)
			 (setq process (get-buffer-process buffer))
			 (set-process-sentinel process
														 #'npmjs--process-sentinel)
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

;;;###autoload
(define-minor-mode npmjs-minor-mode
  "Minor mode to run `npmjs-mode' commands for compile and friends."
  :keymap
	(let ((map (make-sparse-keymap)))
    (define-key map [remap compile]
								npmjs-compile-command)
    (define-key map [remap recompile]
								npmjs-repeat-compile-command)
    (define-key map [remap projectile-compile-project]
								npmjs-compile-command)
		(define-key map [remap project-compile]
								npmjs-compile-command)
    map))
;;;###autoload
(define-derived-mode npmjs-mode
  comint-mode "npmjs"
  "Major mode for npmjs sessions (derived from `comint-mode')."
  (setq-local comint-prompt-read-only nil)
	(setq-local compile-command npmjs--current-command)
  (compilation-setup t))

(defun npmjs-project-name ()
  "Return name from current project package.json."
  (alist-get 'name (npmjs-get-package-json-alist)))


(defun npmjs--get-global-buffer-name ()
	"Get a create a suitable compilation buffer."
	(format "npmjs<global><%s>" npmjs--current-node-version))

(defun npmjs--get-project-buffer-name ()
	"Get a create a suitable compilation buffer."
	(when-let ((name (or (npmjs-project-name)
											 (npmjs-get-project-root))))
    (format "npmjs<%s><%s>" name npmjs--current-node-version)))

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
		(let ((status (process-exit-status proc)))
			(message "npm: %s" status))
    (run-hooks 'npmjs-finished-hook)))

;;;###autoload
(defun npmjs-read-npm-dependency (&optional prompt initial-input history)
	"Call the npm search shell command with PROMPT, INITIAL-INPUT, and HISTORY.
INITIAL-INPUT can be given as the initial minibuffer input."
	(interactive)
	(if (progn
				(require 'ivy nil t)
				(require 'counsel nil t)
				(and (fboundp 'ivy-more-chars)
						 (fboundp 'counsel--async-command)
						 (fboundp 'ivy-read)
						 (fboundp 'counsel-delete-process)))
			(let* ((dependencies)
						 (choice (ivy-read
											(or prompt
													"Repo:\s")
											(lambda (str)
												(or
												 (ivy-more-chars)
												 (progn
													 (counsel--async-command
														(concat
														 "npm search --prefer-offline --no-color --parseable "
														 str))
													 '("" "working..."))))
											:initial-input
											(when initial-input
												(string-join (split-string
																			initial-input "[\s\t,]+"
																			t)
																		 ","))
											:dynamic-collection t
											:history (or history
																	 'npmjs-history-dependencies)
											:multi-action
											(lambda (marked)
												(setq dependencies
															(append marked dependencies)))
											:action (lambda (d) d)
											:unwind #'counsel-delete-process
											:caller 'npmjs-read-npm-dependency))
						 (single-cand (if (stringp choice)
															(car (split-string choice nil t))
														choice))
						 (result
							(cond (dependencies
										 (mapcar (lambda (d)
															 (if (stringp d)
																	 (car (split-string d nil))
																 d))
														 dependencies))
										(t (list single-cand)))))
				(string-join result " "))
		(let ((pkg (npmjs-search-package)))
			(if (stringp pkg)
					(list pkg)
				pkg))))

(transient-define-argument npmjs-package-argument ()
	"Read package name from npm registry."
	:argument " "
	:prompt "Package"
	:always-read t
  :description "Packages "
  :reader 'npmjs-read-npm-dependency
  :class 'transient-files)

(transient-define-argument npmjs-installed-dependencies ()
	"Read package name from npm registry."
	:argument " "
	:prompt "Package"
	:always-read t
  :description "Installed dependency"
  :reader 'npmjs-read-npm-dependency
  :class 'transient-files)

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



(defun npmjs-goto-start-of-entry ()
  "Return button data props from first column."
  (when-let ((id (tabulated-list-get-id)))
    (let ((prev-id))
      (while (and
              (equal (setq prev-id (tabulated-list-get-id))
                     id)
              (= (forward-line -1) 0))
        prev-id)
      (unless (equal prev-id id)
        (forward-line 1)))))

(defun npmjs--mark-by-pred (pred)
	"Mark entries, that satisfies function PRED.
PRED should accepts one argument - id."
	(require 'text-property-search)
  (save-excursion
    (goto-char (point-min))
    (while (text-property-search-forward 'tabulated-list-id)
      (when-let ((id (tabulated-list-get-id)))
        (when (funcall pred id)
          (tabulated-list-put-tag "*"))))))


(defun npmjs--tabulated-marked (&optional entries)
	"Return marked ids or if is non nil ENTRIES, entries.
If optional CHAR is non-nil, then only return items
marked with that character."
	(let (c list
					(fn (if entries
									'tabulated-list-get-entry
								'tabulated-list-get-id)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq c (char-after))
        (unless (eq c ?\s)
					(when-let ((val (funcall fn)))
						(push val list)))
        (forward-line)))
    list))



(defun npmjs--mark ()
	"Mark a tabulated entry at point and move to the next one."
	(interactive)
  (require 'text-property-search)
  (save-excursion
    (npmjs-goto-start-of-entry)
    (tabulated-list-put-tag "*" t))
  (text-property-search-forward 'tabulated-list-id))




(defun npmjs--unmark-all (&optional char)
	"Unmark all marked entries.
If optional CHAR is non-nil, then only return items
marked with that character."
	(interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (pcase (char-after)
        (?\s nil)
        ((pred (apply-partially #'eq char))
         (tabulated-list-put-tag " "))
        (_ (tabulated-list-put-tag " ")))
      (forward-line))))




(defun npmjs--unmark ()
	"Unmark a tabulated entry at point and move to the previous one."
	(interactive)
  (require 'text-property-search)
  (save-excursion
    (npmjs-goto-start-of-entry)
    (tabulated-list-put-tag " " t))
  (text-property-search-backward 'tabulated-list-id))

(defvar npmjs-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "u" 'npmjs--mark)
    (define-key map "m" 'npmjs--unmark)
		(define-key map "U" 'npmjs--unmark-all)
    map))

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
	(setq tabulated-list-entries (or (npmjs-search-parseable-to-rows
																		npmjs-bmenu-search-output)
																	 tabulated-list-entries))
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
    (npmjs-search-highlight-current)
		(npmjs--update-current (or (tabulated-list-get-id) ""))))

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
      (unless (tabulated-list-get-id) (forward-line 1) (npmjs-search-highlight-current)))))


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

(defun npmjs--update-current (cand)
	"Update selected CAND in minibuffer."
	(if (minibufferp)
			(progn (delete-minibuffer-contents)
						 (insert cand))
		(when-let ((wind (active-minibuffer-window)))
			(with-selected-window wind
				(delete-minibuffer-contents)
				(insert cand)))))

(defun npmjs--mark-or-unmark ()
	"Clear any mark on a gist and move to the next line."
	(when-let ((name (tabulated-list-get-id (point))))
    (setq npmjs--marked-packages
          (if (member name npmjs--marked-packages)
              (delete name npmjs--marked-packages)
            (append npmjs--marked-packages (list name))))
    (let ((marked npmjs--marked-packages))
			(npmjs--update-current (string-join marked ",")))))

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
											(when (string-empty-p input)
												(setq npmjs--marked-packages nil))
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
            result))
      (when timer
        (cancel-timer timer))
      (setq npmjs--marked-packages nil)
			(when (get-buffer npmjs-bmenu-search-buff-name)
				(kill-buffer (get-buffer npmjs-bmenu-search-buff-name))))))

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



(defun npmjs-json-parse-string (str &optional object-type array-type null-object
																		false-object)
	"Parse STR with natively compiled function or with json library.

The argument OBJECT-TYPE specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `alist'.

The argument ARRAY-TYPE specifies which Lisp type is used
to represent arrays; `array'/`vector' and `list'.

The argument NULL-OBJECT specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The argument FALSE-OBJECT specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
	(if (and (fboundp 'json-parse-string)
					 (fboundp 'json-available-p)
					 (json-available-p))
			(json-parse-string str
												 :object-type (or object-type 'alist)
												 :array-type
												 (pcase array-type
													 ('list 'list)
													 ('vector 'array)
													 (_ 'array))
												 :null-object (or null-object :null)
												 :false-object (or false-object :false))
		(when (progn
						(require 'json nil t)
						(fboundp 'json-read-from-string))
			(let ((json-object-type (or object-type 'alist))
						(json-array-type
						 (pcase array-type
							 ('list 'list)
							 ('array 'vector)
							 (_ 'vector)))
						(json-null (or null-object :null))
						(json-false (or false-object :false)))
				(json-read-from-string str)))) )

(defun npmjs-json-parse (str)
	"Parse STR with natively compiled function or with json library.
If Emacs has libjansson support, parse it with natively compiled function,
otherwise use the parsing routines from the json library."
	(require 'json nil t)
	(if (and (fboundp 'json-parse-string)
					 (fboundp 'json-available-p)
					 (json-available-p))
			(json-parse-string str
												 :object-type (or json-object-type 'alist)
												 :array-type (if (eq json-array-type 'vector)
																				 'array
																			 'list))
		(when (fboundp 'json-read-from-string)
			(json-read-from-string str))))

(defun npmjs-global-packages ()
	"Return list of globally installed packages."
	(npmjs-nvm-with-current-node-version
	 (with-temp-buffer
		 (shell-command
			"npm ls --global --json"
			(current-buffer))
		 (let ((output (string-trim
										(buffer-string))))
			 (unless (string-empty-p output)
				 (let ((json-object-type 'alist)
							 (json-array-type 'list))
					 (npmjs-json-parse (buffer-string))))))))

(defun npmjs-read-local-packages ()
	"Read globally installed js packages."
	(let* ((alist (npmjs-local-packages))
         (prompt (format "Locally intstalled packages (%s): "
												 (npmjs-get-package-json-path)))
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
    (unless (string-match-p "[a-z0-9-]+@" package)
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
								 (when (string-match "<\\([a-z-0-9@-/]+\\)>" opt)
									 (setq opt
												 (with-temp-buffer
													 (insert opt)
													 (when (re-search-backward ">" nil t 1)
														 (forward-char 1)
														 (if (not (looking-at " "))
																 (read-string "Value for opt %s" opt)
															 (skip-chars-forward "\s\t")
															 (string-trim
																(buffer-substring-no-properties (point)
																																(point-max))))))))
								 (cond ((string-match "\s\t" opt)
												(shell-quote-argument opt))
											 (t opt))))
             (remove nil
										 (flatten-list
                      args))
             "\s"))

(defvar npmjs-man-paths nil)

(defun npmjs-get-man-paths (&optional force)
	"Return MANPATH with node and npm man pages.
IF FORCE is non nil, purge cache."
	(or (and (not force)
					 npmjs-man-paths)
			(setq npmjs-man-paths
						(let* ((files (seq-filter
													 #'file-directory-p
													 (mapcar
														#'directory-file-name
														(directory-files-recursively
														 (cdr
															(assoc
															 (string-trim
																(shell-command-to-string
																 "node -v"))
															 (npmjs-nvm--installed-versions)))
														 "\\_<\\(man\\)\\_>"
														 t))))
									 (mpath (string-join (append (if (getenv "MANPATH")
																									 (list (getenv "MANPATH"))
																								 (process-lines "manpath"))
																							 (list (string-join
																											files
																											path-separator)))
																			 path-separator)))
							mpath))))

(defvar manual-program)
(defvar Man-man-k-use-anchor)

(defun npmjs-man-read-man-entry ()
	"Read manual entry with extended MANPATH."
	(let ((prefix "^"))
		(with-temp-buffer
			(let ((default-directory "/")
						(entries))
				(with-environment-variables (("MANPATH" (or npmjs-man-paths
																										(npmjs-get-man-paths)))
																		 ("COLUMNS" "999"))
					(when (eq 0 (ignore-errors
												(call-process
												 "mandb" nil '(t nil) nil
												 "-c")))
						(delete-region (point-min)
													 (point-max))
						(ignore-errors (call-process
														manual-program nil '(t nil) nil
														"-k" (concat
																	(when (or Man-man-k-use-anchor
																						(string-equal prefix ""))
																		"^")
																	prefix)))
						(setq entries (split-string (buffer-string) "\n")))
					(let* ((entry (completing-read "Candidates: "
																				 entries))
								 (result (with-temp-buffer (insert entry)
																					 (when (fboundp 'Man-parse-man-k)
																						 (car (Man-parse-man-k))))))
						(man result)))))))

;;;###autoload
(defun npmjs-man-advice (&optional fn &rest args)
	"Call FN either with ARGS or with manual entry.
By default FN is man."
	(interactive)
	(if args
			(apply (or fn #'man) args)
		(let ((prefix "^"))
			(npmjs-nvm-with-current-node-version
			 (with-temp-buffer
				 (let ((default-directory "/")
							 (entries))
					 (with-environment-variables (("MANPATH" (npmjs-get-man-paths t))
																				("COLUMNS" "999"))
						 (when (eq 0 (ignore-errors
													 (call-process
														"mandb" nil '(t nil) nil
														"-c")))
							 (delete-region (point-min)
															(point-max))
							 (ignore-errors (call-process
															 manual-program nil '(t nil) nil
															 "-k" (concat
																		 (when (or
																						(string-equal prefix ""))
																			 "^")
																		 prefix)))
							 (setq entries (split-string (buffer-string) "\n")))
						 (let* ((entry (completing-read "Candidates: "
																						entries))
										(result (with-temp-buffer (insert entry)
																							(when (fboundp 'Man-parse-man-k)
																								(car (Man-parse-man-k))))))
							 (apply (or fn 'man)
											(list result))))))))))

;; transient
(defun npmjs-format-transient-args ()
	"Return string from current transient arguments."
	(let ((args (transient-args
               transient-current-command)))
		(print args)
    (pcase transient-current-command
			;; ('npmjs-audit (npmjs-default-format-args
      ;;                (if-let ((subcommand
      ;;                          (seq-find
      ;;                           (lambda (it)
      ;;                             (member
      ;;                              it
      ;;                              args))
      ;;                           '("signatures"
      ;;                             "fix"))))
      ;;                    (append (list subcommand)
      ;;                            (remove subcommand args))
      ;;                  args)))
      (_ (npmjs-default-format-args
          args)))))

(defun npmjs-make-command-name (&rest args)
	"Make name from ARGS."
	(let ((name (mapconcat (apply-partially #'format "%s")
												 (remove nil (mapcar 'npmjs-unquote
																						 (flatten-list args)))
												 " ")))
		name))


(defun npmjs-make-symbol (&rest args)
	"Make name from ARGS."
	(make-symbol
	 (replace-regexp-in-string "[\s\t]+" "-"
														 (npmjs-make-command-name args))))


(defun npmjs-confirm-command (cmd &rest args)
	"Prompt CMD with ARGS in minibuffer."
	(read-string "Run: "
							 (npmjs-make-command-name
								cmd args)))

(defun npmjs-confirm-and-run (cmd &rest args)
	"Run CMD with ARGS after prompting."
	(if (seq-find
			 (apply-partially #'string-match-p
												(regexp-opt
												 '("--global" "-g")
												 'symbols))
			 args)
			(npmjs-compile-global (npmjs-confirm-command cmd args))
		(npmjs-compile (npmjs-confirm-command cmd args))))


(defun npmjs-group-vectors (arguments &optional height win-width)
	"Group ARGUMENTS into vector.
Default value for HEIGHT is `max-mini-window-height',
and for WIN-WIDTH - window width."
	(let* ((descriptions
					(sort
					 (mapcar #'(lambda
											 (&rest args)
											 (length
												(apply
												 #'(lambda
														 (&rest args)
														 (apply #'concat
																		(list
																		 (apply
																			#'tray-builder-take-key
																			args)
																		 (apply
																			#'tray-builder-take-description
																			args))))
												 args)))
                   arguments)
           #'>))
         (longest (+ 10 (car descriptions)))
         (win-width (or win-width (min (window-width) 140)))
         (max-cols (/ win-width longest))
         (count (length arguments))
         (height (or height
										 (floor (* max-mini-window-height 100))))
         (cols (if (>= height count)
									 1
								 (/ win-width longest)))
         (final-cols (min max-cols cols))
         (final (/ count final-cols)))
    (mapcar
     (lambda (it)
       (apply #'vector it))
     (seq-split arguments final))))

;;;###autoload
(defun npmjs-done ()
	"Execute npm transient command."
	(interactive)
  (when transient-current-command
		(let ((name (get transient-current-command 'npm-command))
					(global (transient-arg-value "--global"
																			 (transient-args
																				transient-current-command)))
          (args (npmjs-format-transient-args)))
			(let* ((default-directory (if global
																		default-directory
																	(or
																	 (npmjs-get-project-root)
																	 (vc-root-dir)
																	 default-directory)))
						 (cmd
							(append
							 (npmjs-make-command-name "npm" name
																				(pcase name
																					((or "edit" "uninstall")
																					 (if
																							 (or global
																									 (not
																										(npmjs-get-project-root)))
																							 (npmjs-read-global-packages)
																						 (npmjs-read-local-packages))))))))
				(npmjs-confirm-and-run cmd args)))))

;;;###autoload (autoload 'npmjs-show-args "npmjs.el" nil t)
(transient-define-suffix npmjs-show-args ()
	:transient t
  (interactive)
  (let ((name (get transient-current-command 'npm-command))
				(args
         (npmjs-format-transient-args)))
    (message
     (propertize (npmjs-make-command-name name args)
								 'face 'success))))

(defvar npmjs-known-hints '("<workspace-name>" "<dev|optional|peer>"
														"<hoisted|nested|shallow|linked>" "<otp>"
														"<registry>" "<cidr>" "<id|token>" "<script-shell>"
														"<scope>|<scope:team>" "<scope:team>"
														"<searchexclude>" "<searchopts>" "<browser>"
														"<restricted|public>" "<tag>" "<key>"
														"<key>=<value>" "<pack-destination>"
														"<package-spec>" "<user>" "<depth>" "<@scope>"
														"<legacy|web>" "<id>" "<pkg>" "<viewer>"
														"<fundingSourceNumber>" "<shell>" "<call>"
														"<editor>" "<package-spec" "<path>" "<number>"
														"<global|user|project>" "<cache>"
														"<info|low|moderate|high|critical|none>"
														"<read-only|read-write>"))

(transient-define-argument npmjs-workspace-argument ()
	"Multi value as repeat."
	:argument "--workspace "
  :shortarg "-w"
  :description "Files repeat"
  :multi-value 'repeat
  :class 'transient-option
  :reader 'npmjs-workspace-reader)

(defun npmjs-parse-map-vector (vect)
	"Parse VECT with CMD.
CMD is used for evaluated infix.
If INHIBIT-EVAL is non nil, don't eval infixes."
	(cond ((vectorp vect)
				 (npmjs-parse-map-vector (append vect nil)))
				((not vect)
				 nil)
				((proper-list-p vect)
				 (mapcar #'npmjs-parse-map-vector vect))
				((consp vect)
				 (cons (npmjs-parse-map-vector (car vect))
							 (npmjs-parse-map-vector (cdr vect))))
				((and vect
							(stringp vect))
				 (substring-no-properties vect))
				((and vect)
				 vect)))

(defun npmjs-parse-apply-push (key item)
	"Push KEY to ITEM."
	(push key item))

(defun npmjs-parse-hint (str)
	"Parse string STR e.g. <info|low|moderate|high|critical|none>.
Return string or list of strings."
	(pcase str
		("<workspace-name>"
		 (list
			:multi-value 'repeat
			:class 'transient-option
			:reader 'npmjs-workspace-reader))
		((guard (string-match-p "|" str))
		 (list
			:choices (split-string (if (and (string-prefix-p "<" str)
																			(string-suffix-p ">" str))
																 (substring-no-properties str 1 (1- (length
																																		 str)))
															 str)
														 "[|]" t)))
		(_
		 (if (and (string-prefix-p "<" str)
							(string-suffix-p ">" str))
				 (substring-no-properties str 1 (1- (length
																						 str)))
			 str))))

(defun npmjs-remove-angle-brackets (str)
	"Strip enclosing brackets from STR."
	(if
			(and
			 (string-prefix-p "<" str)
			 (string-suffix-p ">"
												str))
			(substring-no-properties
			 str 1
			 (1-
				(length
				 str)))
		str))

(defun npmjs-split-argument (jsregex)
	"Split JSREGEX to list of choices.
For example:
--force|--package-lock-only|--dry-run|--production|--only=(dev|prod)."
	(cond ((string-match-p ">|" jsregex)
				 (split-string jsregex "|" t))
				((string-match-p " | " jsregex)
				 (split-string jsregex " | " t))
				((string-match-p "|--\\([a-z0-9-]+\\)=[(]\\([^(]+\\)[)]"
												 jsregex)
				 (let* ((str (npmjs-remove-angle-brackets jsregex))
								(substart (string-match-p
													 "|--\\([a-z0-9-]+\\)=[(]\\([^(]+\\)[)]"
													 jsregex)))
					 (if-let* ((left (split-string (substring-no-properties str 0
																																	substart)
																				 "|" t))
										 (right (split-string
														 (substring-no-properties str
																											(1+
																											 substart))
														 "=" t))
										 (arg (concat (pop right) "="))
										 (arg-choices (mapcar (lambda (it)
																						(concat arg it))
																					(split-string
																					 (replace-regexp-in-string
																						"^(\\|)$"
																						""
																						(string-join
																						 right
																						 "="))
																					 "|"))))
							 (append left arg-choices))))
				(t
				 (split-string (npmjs-remove-angle-brackets jsregex)
											 "|" t))))

(defun npmjs-regexp-argument-format (jsregex)
	"Format JSREGEX to plist with :choices and :argument-regexp arg-format.
For example:
--force|--package-lock-only|--dry-run|--production|--only=(dev|prod)."
	(let* ((choices
					(if-let* ((substart (string-match-p
															 "|--\\([a-z0-9-]+\\)=[(]\\([^(]+\\)[)]"
															 jsregex))
										(left (split-string (substring-no-properties jsregex 0
																																 substart)
																				"|" t))
										(right (split-string
														(substring-no-properties jsregex
																										 (1+
																											substart))
														"=" t))
										(arg (concat (pop right) "="))
										(arg-choices (mapcar (lambda (it)
																					 (concat arg it))
																				 (split-string
																					(replace-regexp-in-string
																					 "^(\\|)$"
																					 ""
																					 (string-join
																						right
																						"="))
																					"|"))))
							(append left arg-choices)
						(split-string jsregex "|" t)))
				 (arg-regexp
					(regexp-opt choices)))
		(list
		 :choices choices
		 :argument-regexp arg-regexp)))

(defun npmjs-get-class (args)
	"Extract :class from ARGS."
	(when-let ((class (memq :class args)))
		(cadr class)))

(defun npmjs-get-keyword-value (keyword args)
	"Extract next element after KEYWORD in list ARGS."
	(when (listp args)
		(when-let ((class (memq keyword args)))
			(cadr class))))


(defun npmjs-eval-infix (name &optional args inhibit-eval)
	"Define NAME as a transient infix command with ARGS.
Return list with description and NAME.
If INHIBIT-EVAL no eval."
	(let ((descr (car (seq-filter
										 (lambda (it)
											 (and (stringp it)
														(not	(string-empty-p it))))
										 (list
											(when (stringp (car args))
												(car args))
											(replace-regexp-in-string "npmjs-" ""
																								(format "%s" name))))))
				(pl (if (stringp (car args))
								(cdr args)
							args)))
		(when (memq :argument-format pl)
			(plist-put pl :class ''transient-switches))
		(if inhibit-eval
				(list descr name `(transient-define-infix
														,name
														()
														,@pl))
			(eval `(transient-define-infix
							 ,name
							 ()
							 ,@pl)
						t)
			(list descr name))))



(defun npmjs-parse-vector-choices (items)
	"Return arguments for infix from ITEMS.
ITEMS can be a vector or list of elements divided with char |."
	(when-let* ((groupped
							 (when (seq-find (apply-partially #'equal "|") items)
								 (npmjs-group-with
									(lambda (it)
										(if (stringp it) :choices :args))
									items)))
							(choices (remove "|" (alist-get :choices groupped))))
		`(:choices ',choices
							 :class 'transient-switches
							 :argument-format "%s"
							 :argument-regexp ,(regexp-opt choices))))

(defun npmjs-plist-merge (plist-a plist-b)
	"Add props from PLIST-B to PLIST-A."
	(dotimes (idx (length plist-b))
    (when (eq (logand idx 1) 0)
      (let ((prop-name (nth idx plist-b)))
        (let ((val (plist-get plist-b prop-name)))
          (plist-put plist-a prop-name val)))))
  plist-a)

(defun npmjs-nth (n list-or-vector)
	"Return the N element from list or vector LIST-OR-VECTOR."
	(when (> (length list-or-vector) n)
		(pcase list-or-vector
			((pred vectorp)
			 (aref list-or-vector n))
			((pred proper-list-p)
			 (nth n list-or-vector)))))

(defun npmjs-parse-no-argument-p (argument)
	"Check whether ARGUMENT is line --no-[arg]|arg."
	(string-match-p "--no-" argument))

(defun npmjs-parse-argument (vect)
	"Parse argument VECT."
	(when (vectorp vect)
		(setq vect (append vect nil)))
	(when (stringp vect)
		(setq vect (list vect)))
	(let ((curr)
				(extra-props (seq-filter 'symbolp vect))
				(result))
		(setq vect (seq-remove 'symbolp vect))
		(while (setq curr (pop vect))
			(let* ((value (pop vect))
						 (longarg-parts
							(when (stringp curr)
								(npmjs-get-long-short-option curr)))
						 (longarg
							(when-let ((arg (car longarg-parts)))
								(if (or value extra-props)
										(npmjs-ensure-option-ending arg)
									arg)))
						 (shortarg (nth 1 longarg-parts))
						 (argument
							(or longarg
									(when-let ((plain-arg
															(when (and
																		 (string-prefix-p "--" curr)
																		 (not (string-match-p "|" curr)))
																curr)))
										(if (or value extra-props)
												(npmjs-ensure-option-ending plain-arg)
											plain-arg))))
						 (res))
				(setq res
							(pcase curr
								("--"
								 (setq vect nil)
								 (list "--" (format "Arguments %s" (or value "")) "--"
											 :class 'transient-option
											 :multi-value 'rest
											 :prompt "Arguments (separated by commas): "))
								((guard (and curr (member curr vect)))
								 (setq value
											 (concat (or value "")
															 (string-join
																(seq-filter 'stringp
																						(seq-take vect
																											(seq-position
																											 vect
																											 curr)))
																"")))
								 (setq vect nil)
								 (append
									(list
									 (or longarg curr)
									 (if shortarg
											 (list shortarg longarg)
										 (or argument curr)))
									(npmjs-plist-merge
									 (list
										:class 'transient-option
										:prompt value)
									 extra-props)))
								((guard
									(and
									 (not value)
									 (string-match-p "^[a-z-0-9]+=\\([a-z-0-9]+|[a-z-0-9|]+\\)$"
																	 curr)))
								 (let* ((parts (split-string curr "=" t))
												(arg (pop parts))
												(choices (split-string (car parts) "|" t)))
									 (if extra-props
											 (append `(,arg
																 :choices ',choices
																 :argument ,(format "%s=" curr)
																 :class 'transient-option)
															 (mapcar  (lambda (it)
																					(if (and it
																									 (not (keywordp it))
																									 (symbolp it))
																							`(quote ,it)
																						it))
																				extra-props))
										 `(,arg
											 :choices ',choices
											 :class 'transient-switches
											 :argument-format ,(concat arg "=" "%s")
											 :argument-regexp ,(regexp-opt choices)))))
								((guard
									(and argument
											 value
											 (string-match-p "|" value)
											 extra-props))
								 (append `(,argument
													 :choices ',(npmjs-split-argument value)
													 :argument ,argument
													 :class 'transient-option)
												 (mapcar  (lambda (it)
																		(if (and it
																						 (not (keywordp it))
																						 (symbolp it))
																				`(quote ,it)
																			it))
																	extra-props)))
								((guard
									(and argument
											 value
											 (string-match-p "|" value)
											 (not extra-props)))
								 (let ((choices (npmjs-split-argument value)))
									 `(,argument
										 :choices ',(npmjs-split-argument value)
										 :class 'transient-switches
										 :argument-format ,(concat argument "%s")
										 :argument-regexp ,(regexp-opt choices))))
								((guard
									(and value
											 (stringp value)
											 (stringp curr)
											 (or
												longarg
												(not (string-match-p "|" curr)))
											 (not (string-match-p "|" value))
											 (not vect)))
								 (let* ((long (or argument (npmjs-ensure-option-ending curr)))
												(description (replace-regexp-in-string "^[-]+" "" long))
												(props (append
																(list
																 description
																 (if shortarg
																		 (list shortarg long)
																	 long))
																(npmjs-plist-merge
																 (list
																	:class 'transient-option
																	:prompt value)
																 extra-props))))
									 props))
								((guard (and (stringp curr)
														 (not value)
														 (not (npmjs-short-long-option-p curr))
														 (string-match-p "|" curr)))
								 (let ((choices (npmjs-split-argument curr)))
									 `(:choices ',choices
															:class 'transient-switches
															:argument-format "%s"
															:argument-regexp ,(regexp-opt choices))))
								((guard (and argument
														 (not value)
														 (not extra-props)))
								 (list argument
											 (if shortarg
													 (list shortarg argument)
												 argument)))
								((guard (and (stringp curr)
														 (not value)
														 (not (string-prefix-p "-" value))))
								 (let ((props (npmjs-plist-merge (list :class 'transient-option)
																								 extra-props)))
									 (append (list
														curr
														(npmjs-ensure-option-ending
														 curr))
													 props)))
								((guard (and (stringp curr)
														 (not value)))
								 (list (replace-regexp-in-string "^[-]+" "" curr) curr))
								((guard (and (stringp curr)
														 (vectorp value)
														 (equal curr (npmjs-nth 0 value))))
								 (let ((item (append value nil)))
									 item))
								((guard (and (stringp curr)
														 (npmjs-short-long-option-p curr)
														 (stringp value)
														 extra-props))
								 (let* ((parts (npmjs-get-long-short-option curr))
												(argument (pop parts))
												(shortarg (pop parts)))
									 (append (list (substring-no-properties argument 2)
																 (list shortarg (npmjs-ensure-option-ending
																								 argument)))
													 (reverse extra-props))))
								((guard (npmjs-parse-no-argument-p curr))
								 (let* ((choices (split-string curr "|" t))
												(filtered (seq-remove (apply-partially
																							 'string-prefix-p
																							 "--no-")
																							choices))
												(nodubs (seq-uniq filtered)))
									 (cond ((and value
															 (> (length filtered)
																	(length nodubs))
															 (not (string-match-p value "<")))
													(let ((final-choices (append
																								(seq-filter (apply-partially
																														 'string-prefix-p
																														 "--no-")
																														choices)
																								nodubs
																								(mapcar (lambda (it)
																													(concat it " " value))
																												nodubs))))
														`(,(car nodubs)
															:choices ',final-choices
															:class 'transient-switches
															:argument-format "%s"
															:argument-regexp ,(regexp-opt
																								 final-choices))))
												 (t
													`(,(car filtered)
														:choices ',choices
														:class 'transient-switches
														:argument-format "%s"
														:argument-regexp ,(regexp-opt
																							 choices))))))
								(_
								 nil)))
				(when res
					(push res result))))
		(npmjs-get-list-item-if-one result)))

(defvar npmjs-all-args nil)
(defvar npmjs-all-args-alist nil)

(defun npmjs-show-all-args ()
	"Print all known args."
	(interactive)
	(npmjs-pp (mapcar (lambda (it)
											(cons it
														(list (npmjs-parse-help--vector it "" t))))
										(seq-copy npmjs-all-args))))

(defun npmjs-parse-help--vector (vect &optional cmd inhibit-eval)
	"Parse VECT with CMD.
CMD is used for evaluated infix.
If INHIBIT-EVAL is non nil, don't eval infixes."
	(add-to-list 'npmjs-all-args vect)
	(add-to-list 'npmjs-all-args-alist (cons cmd vect))
	(or
	 (let ((args (or (npmjs-parse-vector-choices vect)
									 (npmjs-parse-argument vect)))
				 (result))
		 (setq result
					 (cond ((or (npmjs-get-keyword-value :argument-format args)
											(and (npmjs-get-keyword-value :choices args)
													 (npmjs-get-keyword-value :multi-value args)))
									(let* ((choices (npmjs-get-keyword-value :choices args))
												 (basename (car (npmjs-unquote choices))))
										(npmjs-eval-infix (npmjs-make-symbol
																			 "npmjs" cmd
																			 basename)
																			args
																			inhibit-eval)))
								 (t args)))
		 result)))

(defun npmjs-tokenize (&optional stop-char)
	"Tokenize current buffer.
STOP-CHAR is used for recoursive purposes."
	(let ((node)
        (nodes)
        (stop nil))
    (while (setq node
								 (unless stop (ignore-errors
																(prog1 (char-to-string (char-after (point)))
																	(forward-char)))))
      (setq stop (if (and stop-char (listp stop-char))
                     (member node stop-char)
                   (and stop-char (equal stop-char node))))
      (unless stop
        (pcase node
					((guard (and (or (and (string= node " ")
																(looking-at "\\.\\.\\."))
													 (and (string= node ".")
																(looking-at "\\.\\.")))))
					 (skip-chars-forward " .")
					 (push :multi-value nodes)
           (setq node
								 'repeat))
					((or "}" "]")
           (setq node nil))
          ("{"
           (setq node (npmjs-tokenize "}")))
					("<"
					 (when-let* ((start (point))
											 (end (progn
															(skip-chars-forward "^>")
															(unless (eobp)
																(when (looking-at ">")
																	(forward-char 1)
																	(point))))))
						 (setq node
									 (concat "<" (buffer-substring-no-properties start end)))
						 node))
					("'"
					 (when-let* ((start (point))
											 (end (progn
															(skip-chars-forward "'")
															(unless (eobp)
																(when (looking-at "'")
																	(forward-char 1)
																	(point))))))
						 (setq node
									 (concat "'" (buffer-substring-no-properties start end)))
						 node))
          ("["
           (setq node
								 (apply 'vector
												(npmjs-tokenize "]"))))
          ((guard (stringp (car-safe nodes)))
					 (let* ((prev (reverse (split-string (pop nodes) " ")))
									(last-node (pop prev)))
						 (setq prev (reverse prev))
						 (when prev
							 (setq prev (string-join prev " "))
							 (push prev nodes))
						 (setq node (concat last-node node))))))
      (unless stop
				(when node
					(push node nodes))))
		(seq-remove (lambda (it)
									(and (stringp it)
											 (string-empty-p it)))
								(mapcar (lambda (it)
													(if (stringp it)
															(string-trim it)
														it))
												(when nodes
													(nreverse nodes))))))


(defvar npmjs-all-columns-alist nil)
(defvar npmjs-all-descriptions-alist nil)

(defun npmjs-write-all-installed-npm-outputs ()
	"\\(--\\([a-z][a-z0-9-]+\\)\\)."
	(let ((versions (npmjs-nvm--installed-versions)))
		(dolist (cell versions)
			(npmjs-nvm-with-env-vars
			 (car cell)
			 (let ((npm-version (string-trim (shell-command-to-string "npm -v")))
						 (npm-output (shell-command-to-string "npm -l")))
				 (unless (file-exists-p (expand-file-name "fixtures" default-directory))
					 (make-directory (expand-file-name "fixtures" default-directory)
													 t))
				 (write-region
					npm-output
					nil
					(expand-file-name (concat "fixtures/" npm-version)
														default-directory)
					nil
					nil
					nil
					nil))))))

(defun npmjs-write-get-all-descriptions-alist ()
	"\\(--\\([a-z][a-z0-9-]+\\)\\)."
	(let ((versions (npmjs-nvm--installed-versions))
				(results))
		(setq npmjs-all-columns-alist nil)
		(dolist (cell versions)
			(let ((v (car cell)))
				(npmjs-nvm-with-env-vars v
					(let ((npm-version (string-trim (shell-command-to-string
																					 "npm -v")))
								(output))
						(unless (assoc-string npm-version results)
							(setq output
										(with-temp-buffer
											(shell-command
											 "npm -l"
											 (current-buffer))
											(goto-char
											 (point-min))
											(npmjs-parse-columns-to-alist)))
							(push (cons npm-version output)
										npmjs-all-columns-alist)
							(push (cons npm-version
													(npmjs-map-descriptions-lines
													 output
													 t))
										results))))))
		(setq npmjs-all-columns-alist
					(seq-sort-by 'car 'version<
											 (nreverse
												npmjs-all-columns-alist)))
		(setq npmjs-all-descriptions-alist (nreverse results))))

(defun npmjs-print-all-descriptions (&optional force)
	"Generate descriptions. If FORCE, recalc."
	(interactive "P")
	(unless (and npmjs-all-descriptions-alist
							 (not force))
		(npmjs-write-get-all-descriptions-alist))
	(npmjs-pp npmjs-all-descriptions-alist))

(defun npmjs-print-current-descriptions (&optional force)
	"Generate descriptions. If FORCE, recalc."
	(interactive "P")
	(unless (and npmjs-all-descriptions-alist
							 (not force))
		(npmjs-write-get-all-descriptions-alist))
	(npmjs-pp npmjs-all-descriptions-alist))

(defun npmjs-upcased-p (str)
	"Return non nil is string STR begins with upcased letters."
	(let ((case-fold-search nil))
		(string-match-p "^[A-Z]" str)))

(defun npmjs-map-descriptions-lines (alist &optional inhibit-eval)
	"Map ALIST of commands and arguments.
If INHIBIT-EVAL is non nil, don't eval infixes."
	(mapcar (lambda (it)
						(npmjs-map-command-cell-lines it inhibit-eval))
					alist))


(defun npmjs-get-list-item-if-one (item)
	"If ITEM is a list of one element, return those element."
	(cond ((and (proper-list-p item)
							(= 1 (length item)))
				 (car item))
				((and (vectorp item)
							(= 1 (length item)))
				 (npmjs-nth
					0 item))
				(t item)))

(defun npmjs-ensure-option-ending (long)
	"Maybe add space to LONG option."
	(if (or (string-suffix-p "=" long)
					(string= "--" long)
					(string-suffix-p " " long))
			long
		(concat long " ")))

(defun npmjs-short-long-option-p (str)
	"Return non nil if STR has short and long option."
	(string-match-p "^\\(-\\([a-z]+\\)\\)[|]\\(--\\([a-z-0-9-]+\\)\\)$"
									str))

(defun npmjs-get-long-short-option (str)
	"Split STR to list of long and short option."
	(if (npmjs-short-long-option-p str)
			(reverse (split-string str "|" t))
		nil))



(defun npmjs-flatten-vectors (item)
	"Flattten vector ITEM."
	(cond ((vectorp item)
				 (seq-reduce (lambda (acc it)
											 (let* ((last-idx
															 (when (> (length acc) 0)
																 (1- (length acc))))
															(last-value
															 (when last-idx
																 (npmjs-nth
																	last-idx
																	acc))))
												 (cond ((and last-idx
																		 (vectorp it)
																		 (= 1 (length it))
																		 (stringp last-value)
																		 (stringp (npmjs-nth 0 it)))
																(let* ((curr-value
																				(npmjs-nth 0 it))
																			 (new-val
																				(concat
																				 last-value
																				 curr-value)))
																	(aset acc last-idx new-val)
																	(setq acc acc)))
															 ((and (vectorp it)
																		 (setq acc
																					 (vconcat acc
																										(npmjs-flatten-vectors
																										 it)))))
															 (t (setq acc (vconcat acc (vector it)))))))
										 item
										 []))))

(defun npmjs-multi-extract-vector (vect)
	"Transform vector VECT."
	(or
	 (when-let ((subvect (and (vectorp vect)
														(seq-find #'vectorp vect))))
		 (when (equal (npmjs-nth 0 subvect)
									(npmjs-nth 0 vect))
			 subvect))
	 vect))


(defun npmjs-normalize-vectors (vect)
	"Normalize vector VECT."
	(let ((res (npmjs-flatten-vectors
							(npmjs-multi-extract-vector vect))))
		(let* ((l (append res nil))
					 (left
						(unless (member "|" l)
							(seq-take-while (lambda (it)
																(and (stringp it)
																		 (not (string-prefix-p "-" it))))
															l)))
					 (right (seq-drop l (length left))))
			(if left
					(setq left (apply 'vector
														(append (list (string-join left ""))
																		right)))
				res))))
(defun npmjs-make-command-doc (cmd lines)
	"Make command CMD description from help LINES."
	(string-join
	 (seq-filter  (lambda (it)
									(or
									 (when cmd
										 (string-prefix-p
											(concat "npm " cmd)
											it))
									 (and (npmjs-upcased-p it)
												(not (string-prefix-p
															"Options:" it))
												(not (string-prefix-p
															"Usage:" it))
												(not
												 (string-prefix-p
													"Run"
													it)))))
								lines)
	 "\n"))


(defun npmjs-join-strings (items)
	"Recoursively concat list or vector of ITEMS."
	(seq-reduce (lambda (acc it)
								(when-let* ((str (if (stringp it)
																		 it
																	 (or (npmjs-join-strings it)
																			 "")))
														(suffix (if (and (string-match-p "^[a-z-]" str)
																						 (not (string-empty-p acc)))
																				" "
																			"")))
									(setq acc (concat acc
																		suffix
																		str))))
							(when (not (symbolp items))
								items)
							""))

(defun npmjs-premap-options (rawoptions)
	"Concat RAWOPTIONS."
	(let ((options)
				(item))
		(while (setq item (pop rawoptions))
			(cond ((and (vectorp item)
									(stringp (npmjs-nth 0 item))
									(not (string-prefix-p "-" (npmjs-nth 0 item))))
						 (let* ((items (seq-take-while
														(lambda (it)
															(or (stringp it)
																	(symbolp it)
																	(when (and
																				 (vectorp it)
																				 (stringp
																					(npmjs-nth
																					 0 it)))
																		(not (string-prefix-p
																					"-"
																					(npmjs-nth
																					 0 it))))))
														rawoptions))
										(next-options (seq-drop rawoptions (length items))))
							 (setq rawoptions next-options)
							 (let* ((syms (append
														 (seq-filter 'symbolp (append item nil))
														 (seq-filter 'symbolp
																				 items)))
											(res (delq nil (append (list
																							(npmjs-join-strings
																							 (append
																								(seq-remove 'symbolp
																														(append item
																																		nil))
																								(seq-remove
																								 'symbolp
																								 items))))
																						 syms))))
								 (push (apply 'vector res)
											 options))))
						((and (stringp item))
						 (let* ((items (seq-take-while 'stringp rawoptions))
										(next-options (seq-drop rawoptions (length items))))
							 (setq rawoptions next-options)
							 (push (vector (concat item (string-join items ""))) options)))
						((vectorp item)
						 (push item options))))
		(reverse options)))

(defun npmjs-map-raw-options (inline-options)
	"Normalize INLINE-OPTIONS."
	(mapcar
	 #'npmjs-normalize-vectors
	 (seq-filter
		#'vectorp
		(mapcar (lambda (it)
							(pcase it
								((guard (and (stringp it)
														 (string-match-p it "^<")
														 it))
								 (vector it))
								(_ it)))
						(npmjs-premap-options inline-options)))))

(defun npmjs-map-command-cell-lines (cell &optional inhibit-eval)
	"Parse CELL with command and description lines to options.
If INHIBIT-EVAL is nil, don't eval infixes."
	(let ((cmd (car cell))
				(lines (cdr cell))
				(subcommands)
				(common-options)
				(aliases)
				(descriptions)
				(curr))
		(while (setq curr (pop lines))
			(setq curr (replace-regexp-in-string "[(]same as[ ][^)]+)" "" curr))
			(setq curr (replace-regexp-in-string "(in package dir)" "" curr))
			(setq curr (replace-regexp-in-string "[(]with no args,[^)]+)" "" curr))
			(setq curr (replace-regexp-in-string "[(]See [^)]+)" "" curr))
			(setq curr (replace-regexp-in-string "[\s]?|[\s]?" "|" curr))
			(let ((parsed (npmjs-read-line curr)))
				(pcase-let* ((words
											(seq-take-while (lambda (it)
																				(and (stringp it)
																						 (not (string-prefix-p  "<" it))
																						 (not (string-prefix-p "(" it))
																						 (not (string-prefix-p "-" it))
																						 (not (string-match-p "=" it))))
																			parsed))
										 (key (string-join words "\s"))
										 (rawoptions (seq-drop parsed (length words)))
										 (`(,first-word ,second-word ,third-word . ,other)
											(split-string key " " t)))
					(pcase first-word
						("common"
						 (setq common-options
									 (append common-options rawoptions)))
						("Options:"
						 (when-let ((parsed-options (seq-take-while (lambda (it)
																													(string-prefix-p "["
																																					 it))
																												lines)))
							 (setq lines (seq-drop lines (length parsed-options)))
							 (setq common-options
										 (seq-reduce (lambda (acc it)
																	 (append acc (npmjs-read-line it)))
																 parsed-options
																 '()))))
						("Usage:")
						((guard (and (stringp first-word)
												 (npmjs-upcased-p first-word)))
						 (push curr descriptions))
						((guard (listp key)))
						((guard (and (not first-word)
												 (vectorp key)))
						 (push key common-options))
						((or "aliases:"
								 "alias:")
						 (let ((strs (mapcar (apply-partially 'replace-regexp-in-string
																									"," "")
																 (delq nil
																			 (flatten-list (list second-word
																													 third-word other))))))
							 (setq aliases (if aliases (nconc aliases strs)
															 strs))))
						((guard (and (stringp first-word)
												 (string= first-word "npm")
												 second-word
												 (string= cmd second-word)))
						 (setq key (replace-regexp-in-string "^npm[ ]" "" key))
						 (setq rawoptions (npmjs-map-raw-options
															 (npmjs-multi-extract-vector
																rawoptions)))
						 (setq rawoptions (mapcar (lambda (it)
																				(npmjs-parse-help--vector
																				 it
																				 key
																				 inhibit-eval))
																			rawoptions))
						 (let ((subcommand-cell (assoc-string key subcommands)))
							 (if subcommand-cell
									 (setcdr subcommand-cell
													 (append (cdr subcommand-cell)
																	 rawoptions))
								 (push (cons key rawoptions) subcommands))))
						(_)))))
		(setq common-options (mapcar (lambda (it)
																	 (npmjs-parse-help--vector
																		(npmjs-normalize-vectors
																		 it)
																		cmd
																		inhibit-eval))
																 common-options))
		(setq subcommands (mapcar
											 (lambda
												 (it)
												 (npmjs--plist-remove-nils
													(list
													 :description (npmjs-make-command-doc
																				 (car it)
																				 (cdr cell))
													 :cmd (car it)
													 :key
													 (car (last (split-string (car it) " " t)))
													 :options
													 (delq nil
																 (append
																	(cdr it)
																	common-options)))))
											 subcommands))
		(if (not (= (length subcommands) 1))
				(npmjs--plist-remove-nils (list
																	 :cmd cmd
																	 :description (npmjs-make-command-doc
																								 nil
																								 (cdr cell))
																	 :subcommands subcommands
																	 :options common-options))
			(let ((spec (car subcommands))
						(short-descr  (npmjs-make-command-doc
													 (car cell)
													 (cdr cell))))
				(plist-put spec :description short-descr)
				spec))))

(defmacro npmjs-parse-help-with-output (output &rest body)
	"Expand BODY in temp buffer with OUTPUT."
	(declare (indent 1)
					 (debug t))
  `(let ((version npmjs--current-node-version))
		 (with-temp-buffer
			 (erase-buffer)
			 (npmjs-nvm-with-env-vars
				 version
				 (save-excursion
					 (insert ,output)))
			 ,@body)))

(defun npmjs-get-command-description-raw (command)
	"Return COMMAND description from help output."
	(npmjs-parse-help-with-output
			(shell-command-to-string
			 "npm -l")
		(let ((command-re
					 (concat "^[\s\t]?+" (regexp-quote command) "[:]?[\s][\s]+"))
					(regexp "^\\([\s\t]*\\)\\([a-z0-9]+[a-z0-9_-]+\\)[:]?[\s][\s]+"))
			(when (re-search-forward
						 command-re nil t 1)
				(let ((descr-start (point))
							(descr-end))
					(setq descr-end (or (save-excursion
																(when-let ((found (re-search-forward
																									 regexp nil t 1)))
																	(forward-line -1)
																	(point)))
															(save-excursion
																(let ((col (current-column))
																			(found))
																	(when (> col 0)
																		(while
																				(when (= 0 (forward-line))
																					(let* ((beg (point))
																								 (str (buffer-substring-no-properties
																											 beg
																											 (+ beg
																													col))))
																						(string-empty-p (string-trim str))))
																			(setq found (point))))
																	found))
															(line-end-position)))
					(buffer-substring-no-properties
					 descr-start
					 descr-end))))))

(defun npmjs-get-description-spec (command)
	"Pase COMMAND description from help output."
	(npmjs-nvm-with-current-node-version
	 (seq-remove #'string-empty-p
							 (split-string (or (npmjs-get-command-description-raw command) "")
														 "[\n]" t))))

(defun npmjs-get-command-spec (command &optional inhibit-eval)
	"Pase COMMAND description from help output.
If INHIBIT-EVAL is non nil, don't eval infixes."
	(npmjs-map-command-cell-lines (cons
																 command
																 (npmjs-get-description-spec command))
																inhibit-eval))

(defun npmjs-read-line (line)
	"Tokenize single description LINE."
	(if (stringp line)
			(with-temp-buffer
				(insert line)
				(goto-char (point-min))
				(npmjs-tokenize))
		line))

(defun npmjs-group-with (fn items &optional transform-fn)
	"Group ITEMS by calling FN with every item.
FN should return key.
TRANSFORM-FN should return transformed item."
	(nreverse
	 (seq-reduce (lambda (acc it)
								 (let* ((key (funcall fn it))
												(val (if transform-fn (funcall transform-fn it)
															 it))
												(cell (assoc key acc))
												(group (if cell
																	 (append (cdr cell)
																					 (list val))
																 (list val))))
									 (if cell
											 (setcdr cell group)
										 (push (cons key group) acc))
									 acc))
							 (seq-copy items) '())))

(defun npmjs-parse-columns-to-alist ()
	"Search and parse COMMAND flags using REGEXP and NUM."
	(let ((rows)
				(regexp "^\\([\s\t]*\\)\\([a-z0-9]+[a-z0-9_-]+\\)[:]?[\s][\s]+"))
    (while (re-search-forward
						regexp
						nil t 1)
      (let ((cmd (match-string-no-properties 2))
            (descr-start (point))
            (description)
            (dend))
				(setq dend (or (save-excursion
												 (when-let ((found (re-search-forward
																						regexp nil t 1)))
													 (forward-line -1)
													 (point)))
											 (let ((col (current-column))
														 (found))
												 (while
														 (when (= 0 (forward-line))
															 (let* ((beg (point))
																			(str (buffer-substring-no-properties
																						beg
																						(+ beg
																							 col))))
																 (string-empty-p (string-trim str))))
													 (setq found (point)))
												 found)))
        (setq description (buffer-substring-no-properties
                           descr-start
													 (max (or dend (point))
																(point))))
				(push (cons cmd (seq-remove 'string-empty-p
																		(mapcar 'string-trim
																						(split-string
																						 description
																						 "[\r\f\n]+"
																						 t))))
							rows)))
    (nreverse rows)))

(defun npmjs-eval-symb (cmd options &optional description no-eval)
	"Eval prefix with OPTIONS, CMD and SUBCOMMANDS.
NO-EVAL is used for debug purposes."
	(let ((name (npmjs-make-symbol "npm" cmd)))
		(if no-eval
				(message "Evaluating %s as %s" cmd name)
			(npmjs-eval-prefix name (if description
																	`([:description ,description ,@options])
																`([,@options]))))
		(put name 'npm-command cmd)
		name))

;;;###autoload
(defun npmjs-eval-prefix (name body)
	"Eval and call transient prefix with NAME and BODY."
	(interactive)
  (eval `(progn (transient-define-prefix ,name ()
									:value (lambda ()
													 (unless (npmjs-get-project-root)
														 (list "--global")))
                  ,@body)
                ',name)
        t))

(defun npmjs-get-package-json-script (script)
	"Search for SCRIPT in package.json."
	(alist-get script (alist-get 'scripts (npmjs-get-package-json-alist))))

(defvar npmjs-commands-props
	'(("publish" :inapt-if-not npmjs-get-project-root)
		("run-script" :inapt-if-not npmjs-get-project-root)
		("start" :inapt-if-not
     (lambda ()
			 (or
				(npmjs-get-package-json-script 'start)
				(when-let ((proj (npmjs-get-project-root)))
					(file-exists-p (expand-file-name "server.js" proj))))))
		("test" :inapt-if-not
     (lambda ()
			 (npmjs-get-package-json-script 'test)))
		("stop" :inapt-if-not (lambda ()
														(npmjs-get-package-json-script 'stop)))
		("ci" :inapt-if-not npmjs-get-project-root)
		("dedupe" :inapt-if-not npmjs-get-project-root)))


(defvar npmjs-map-replace-commands '(("run-script" . npmjs-run-script))
	"Alist of commands and predefined commands to use instead.")

(defvar npmjs-extra-arguments
	'(("install" ("dependency" npmjs-package-argument))
		("uninstall" ("global" ("-g" "--global")))))

(defvar npmjs-options-suffixes
	'(("RET" "Run" npmjs-done)
		("C-c C-a" "Show arguments" npmjs-show-args)))

(defun npmjs-map-commands (commands)
	"Recoursively map and eval COMMANDS.
COMMANDS is a list of plists with such props:
:cmd - a string, the command name e.g. \"install\", \"access list\".
:key - a string, which will be used for generating key.
:options - alist of descriptions (string) and its arguments.
:subcommands - same as COMMANDS."
	(cond ((and (listp (car commands))
							(keywordp (car-safe (car commands))))
				 (tray-builder-generate-shortcuts
					(seq-remove
					 (lambda (it)
						 (member (plist-get it :cmd)
										 npmjs-omit-commmands))
					 commands)
					(lambda (it)
						(or (plist-get it :key)
								(plist-get it :cmd)))
					(lambda (key value)
						(npmjs-map-commands (plist-put value :key key)))))
				((and (keywordp (car-safe commands))
							(plist-get commands :subcommands))
				 (setq commands
							 (plist-put commands :subcommands
													(tray-builder-generate-shortcuts
													 (plist-get commands :subcommands)
													 (lambda (it)
														 (let ((key (or (plist-get it :key)
																						(plist-get it :cmd))))
															 key))
													 (lambda (key value)
														 (setq value (plist-put value :key key))
														 (npmjs-map-commands
															value)))))
				 (let ((name (plist-get commands :cmd))
							 (key (plist-get commands :key))
							 (sym))
					 (setq sym
								 (or (cdr (assoc-string name npmjs-map-replace-commands))
										 (npmjs-eval-symb
											name
											(plist-get commands :subcommands)
											(plist-get commands :description))))
					 (if-let ((props (cdr (assoc-string name npmjs-commands-props))))
							 (append (list key name sym) props)
						 (list key name sym))))
				((and (keywordp (car-safe commands))
							(plist-get commands :options))
				 (let ((name (plist-get commands :cmd))
							 (sym))
					 (setq commands
								 (plist-put commands :options
														(append
														 (npmjs-add-options-shortcuts
															(if-let ((extra-options
																				(cdr
																				 (assoc-string
																					name
																					npmjs-extra-arguments))))
																	(append extra-options (plist-get
																												 commands
																												 :options))
																(plist-get commands :options)))
														 npmjs-options-suffixes)))
					 (setq sym
								 (or (cdr (assoc-string name npmjs-map-replace-commands))
										 (npmjs-eval-symb name
																			(plist-get commands :options)
																			(plist-get commands :description))))
					 (if-let ((props (cdr (assoc-string name npmjs-commands-props))))
							 (append (list (plist-get commands :key) name sym) props)
						 (list (plist-get commands :key) name sym))))
				((and (keywordp (car-safe commands))
							(assoc-string (plist-get commands :cmd)
														npmjs-map-replace-commands))
				 (let* ((cmd (plist-get commands :cmd))
								(name (cdr (assoc-string cmd npmjs-map-replace-commands)))
								(key (plist-get commands :key)))
					 (list key cmd name)))
				((and (keywordp (car-safe commands))
							(plist-get commands :cmd)
							(not (cdr (assoc-string (plist-get commands :cmd)
																			npmjs-map-replace-commands))))
				 (let* ((cmd (plist-get commands :cmd))
								(name
								 (npmjs-make-symbol cmd))
								(key (plist-get commands :key)))
					 (put name 'npm-command cmd)
					 (fset name
								 `(lambda ()
										(interactive)
										(npmjs-confirm-and-run
										 "npm"
										 ,cmd)))
					 (if-let ((props (cdr (assoc-string name npmjs-commands-props))))
							 (append (list key cmd name) props)
						 (list key cmd name))))
				(t (message "unmatch %s" commands)
					 commands)))

(defun npmjs-get-option-shortcut-word (option)
	"Return OPTION description to use in shortcut generation."
	(let ((description (car option)))
		(pcase description
			((pred stringp)
			 description)
			((pred symbolp)
			 (replace-regexp-in-string "^npmjs-" "" (symbol-name (car option)))))))

(defun npmjs-add-options-shortcuts (options)
	"Add shortcuts to OPTIONS."
	(tray-builder-generate-shortcuts
	 options
	 #'npmjs-get-option-shortcut-word
	 (lambda (k v)
		 (pcase (car v)
			 ("--" v)
			 (_ (push k v))))))


;;;###autoload (autoload 'npmjs-run-script "npmjs.el" nil t)
(transient-define-prefix npmjs-run-script ()
	"Run arbitrary package scripts."
	[:description
	 (lambda ()
		 (when-let* ((package-json-path
									(npmjs-get-package-json-path))
								 (package-json
									(npmjs-read-json
									 package-json-path
									 'alist)))
			 (or (alist-get 'name package-json) "Run script ")))
	 :setup-children
	 (lambda (&rest _args)
		 (when-let* ((package-json-path
									(npmjs-get-package-json-path))
								 (package-json
									(npmjs-read-json
									 package-json-path
									 'alist))
								 (scripts (mapcar (lambda (it)
																		(cons (symbol-name (car it))
																					(cdr it)))
																	(alist-get
																	 'scripts
																	 package-json))))
			 (let* ((children
							 (tray-builder-generate-shortcuts
								scripts
								#'car
								(lambda (key value)
									(let ((descr (cdr value))
												(script (car value))
												(name (npmjs-make-symbol (car value))))
										(fset name
													`(lambda ()
														 (interactive)
														 (npmjs-confirm-and-run
															"npm"
															"run-script"
															,script
															(npmjs-format-transient-args))))
										(list key
													name
													:description `(lambda ()
																					(concat
																					 ,script
																					 " "
																					 "("
																					 (propertize
																						,descr
																						'face
																						'transient-value)
																					 ")"))))))))
				 (mapcar
					(apply-partially #'transient-parse-suffix
													 transient--prefix)
					children))))]
	["Options"
	 :setup-children
	 (lambda (&rest _args)
		 (when-let* ((package-json-path
									(npmjs-get-package-json-path))
								 (package-json
									(npmjs-read-json
									 package-json-path
									 'alist))
								 (scripts (mapcar (lambda (it)
																		(cons (symbol-name (car it))
																					(cdr it)))
																	(alist-get
																	 'scripts
																	 package-json))))
			 (let* ((props (npmjs-get-command-spec "run-script"))
							(options (npmjs-add-options-shortcuts
												(plist-get props :options)))
							(children (append options
																npmjs-options-suffixes)))
				 (mapcar
					(apply-partially #'transient-parse-suffix
													 transient--prefix)
					children))))])

(put 'npmjs-run-script 'npm-command "run-script")

(defun npmjs-get-prefix-spec ()
	"Return cons with description and parsed npm help output."
	(let* ((output (shell-command-to-string "npm -l"))
				 (description
					(car
					 (last (split-string output "\n" t))))
				 (spec (npmjs-parse-help-with-output
									 output
								 (npmjs-parse-columns-to-alist))))
		(cons description spec)))

(defun npmjs-get-prefix-spec-current-node ()
	"Return cons with description and parsed npm help output."
	(npmjs-nvm-with-env-vars
	 (or npmjs--current-node-version
			 (setq npmjs--current-node-version (or npmjs--current-node-version
																						 (npmjs-confirm-node-version))))
	 (npmjs-get-prefix-spec)))

(defvar npmjs-evaluated-commands (make-hash-table :test 'equal)
  "Last executed command lines, per project.")

(defun npmjs-pp (obj)
	"Print OBJ."
	(when (and
				 (require 'momentary-popup nil t)
				 (fboundp 'momentary-popup-inspect))
		(momentary-popup-inspect obj)))

(defun npmjs-debug ()
	"Eval prefix."
	(interactive)
	(npmjs-nvm-with-env-vars
		(or npmjs--current-node-version
				(setq npmjs--current-node-version (or npmjs--current-node-version
																							(npmjs-confirm-node-version))))
		(let* ((spec (npmjs-get-prefix-spec))
					 (description (car spec))
					 (new-spec
						(npmjs-map-descriptions-lines (cdr spec)
																					t)))
			(message "%s" description)
			(npmjs-pp (list new-spec
											(npmjs-map-commands new-spec))))))

;;;###autoload
(defun npmjs ()
	"Eval prefix."
	(interactive)
	(npmjs-nvm-with-env-vars
		(or npmjs--current-node-version
				(setq npmjs--current-node-version (or npmjs--current-node-version
																							(npmjs-confirm-node-version))))
		(let ((npm-version (string-trim (shell-command-to-string
																		 "npm -v"))))
			(if-let ((prefix (and (not npmjs-inhibit-prefix-cache)
														(gethash npm-version
																		 npmjs-evaluated-commands))))
					(call-interactively prefix)
				(let* ((output (shell-command-to-string "npm -l"))
							 (title (car (last (split-string output "[\n]" t))))
							 (description (concat "NPM: "
																		npm-version
																		" "
																		title))
							 (spec
								(npmjs-map-descriptions-lines
								 (npmjs-parse-help-with-output
										 output
									 (npmjs-parse-columns-to-alist))))
							 (mapped (npmjs-map-commands spec))
							 (groupped (npmjs-group-vectors mapped))
							 (prefix-symb
								(let ((sym (npmjs-make-symbol "npmjs-" npm-version)))
									(npmjs-eval-prefix
									 sym
									 `([:description
											,description
											,@(append
												 groupped
												 (list
													(apply #'vector
																 '(("-o"
																		"nvm"
																		npmjs-nvm
																		:inapt-if-not
																		npmjs-nvm-path)))))]))
									(put sym 'npm-command "npm")
									sym)))
					(call-interactively prefix-symb)
					(puthash npm-version prefix-symb npmjs-evaluated-commands))))))

(defvar npmjs-nvm-suffix `["NVM"
													 ("I" "Install new node version"
														npmjs-nvm-install-node-version)
													 ("u" "Use other node version"
														npmjs-other-node-version)
													 ("U" "Use other node version (globally)"
														npmjs-use-switch-system-node-version)
													 ("f" "Jump to installed node"
														npmjs-nvm-jump-to-installed-node)])

;;;###autoload (autoload 'npmjs-nvm "npmjs.el" nil t)
(transient-define-prefix npmjs-nvm ()
	"Menu for NVM (node version manager) commands."
	:man-page "npm"
	["nvm dummy menu"])

(transient-insert-suffix 'npmjs-nvm (list 0)
  npmjs-nvm-suffix)

(provide 'npmjs)
;;; npmjs.el ends here