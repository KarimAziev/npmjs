;;; npmjs.el --- Command dispatcher for npm package manager -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>\

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/npmjs
;; Version: 0.1.0-git
;; Keywords: tools
;; Package-Requires: ((emacs "28.1") (transient "0.4.1"))
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

;; Command dispatcher for npm package manager

;;; Code:

(require 'transient)
(require 'compile)

(eval-when-compile
  (require 'subr-x))

(defvar json-object-type)
(defvar json-array-type)
(defvar json-null)
(defvar json-false)

(declare-function json-read-from-string  "json")

(defcustom npmjs-nvm-dir (or (getenv "NVM_DIR")
                             (expand-file-name "~/.nvm"))
  "Full path to NVM installation directory.
See https://github.com/nvm-sh/nvm."
  :group 'npmjs
  :type 'directory)



(defcustom npmjs-inhibit-prefix-cache nil
  "Whether to always re-evaluate prefixes. It is mostly for debug purposes."
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

(defcustom npmjs-finished-hook nil
  "Hooks to run after a npmjs process finishes."
  :group 'npmjs
  :type 'hook)

(defcustom npmjs-setup-hook nil
  "Hooks to run before a npmjs process starts."
  :group 'npmjs
  :type 'hook)

(defcustom npmjs-message-function 'message
  "Function to show messages or nil.
If a function, it will be called with one argument - a formatted string."
  :type '(choice
          (const :tag "None" nil)
          (function :tag "Function"))
  :group 'npmjs)

(defvar-local npmjs--current-command nil)
(defvar-local npmjs--current-node-version nil)
(defvar-local npmjs-current-descriptions-alist nil)
(defvar npmjs-descriptions-alist nil)

;; common utilities
(defun npmjs-message (string &rest arguments)
  "Apply `npmjs-message-function' with ARGUMENTS.
The first argument STRING is a format control string.
The other arguments are substituted into it to make the result, a string."
  (when npmjs-message-function
    (funcall npmjs-message-function
             (concat "npmjs: " (apply #'format string arguments)))))

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

(eval-and-compile
  (defun npmjs--expand (init-fn)
    "If INIT-FN is a non-quoted symbol, add a sharp quote.
Otherwise, return it as is."
    (setq init-fn (macroexpand init-fn))
    (if (symbolp init-fn)
        `(#',init-fn)
      `(,init-fn))))

(defmacro npmjs--compose (&rest functions)
  "Return right-to-left composition from FUNCTIONS."
  (declare (debug t)
           (pure t)
           (indent defun)
           (side-effect-free t))
  (setq functions (reverse functions))
  (let ((args-var (make-symbol "arguments")))
    `(lambda (&rest ,args-var)
       ,@(let ((init-fn (pop functions)))
           (list
            (seq-reduce
             (lambda (acc fn)
               `(funcall ,@(npmjs--expand fn) ,acc))
             functions
             `(apply ,@(npmjs--expand init-fn) ,args-var)))))))

(defmacro npmjs--cond (&rest pairs)
  "Return a function that expands a list of PAIRS to `cond' clauses.
Every pair should be either:
- a vector of [predicate transformer],
- a list of (predicate transformer).

The predicate can also be t.

All of the arguments to function are applied to each of the predicates in turn
until one returns a \"truthy\" value, at which point the function
returns the result of applying its arguments to the corresponding transformer."
  (declare (pure t)
           (indent defun)
           (side-effect-free error-free))
  (setq pairs (mapcar (lambda (it)
                        (if (listp it)
                            (apply #'vector it)
                          it))
                      pairs))
  (let ((args (make-symbol "arguments")))
    `(lambda (&rest ,args)
       (cond ,@(mapcar (lambda (v)
                         (list (if (eq (aref v 0) t) t
                                 `(apply ,@(npmjs--expand (aref v 0)) ,args))
                               `(apply ,@(npmjs--expand (aref v 1)) ,args)))
                       pairs)))))

(defmacro npmjs--or (&rest functions)
  "Return an unary function which invoke FUNCTIONS until first non-nil result."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  (let ((it (make-symbol "it")))
    `(lambda (,it)
       (or
        ,@(mapcar (lambda (v)
                    (if (symbolp v)
                        `(,v ,it)
                      `(funcall ,v ,it)))
                  functions)))))

(defmacro npmjs--and (&rest functions)
  "Return an unary function which invoke FUNCTIONS until first nil result."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  `(lambda (it)
     (and
      ,@(mapcar (lambda (v)
                  (if (symbolp v)
                      `(,v it)
                    `(funcall ,v it)))
                functions))))

(defmacro npmjs--rpartial (fn &rest args)
  "Return a partial application of FN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list (if (symbolp fn)
                     `(apply #',fn (append pre-args (list ,@args)))
                   `(apply ,fn (append pre-args (list ,@args))))))))

(defmacro npmjs--converge (combine-fn &rest functions)
  "Make a function to apply COMBINE-FN with the results of branching FUNCTIONS.
If the first element of FUNCTIONS is a vector, it will be used instead.

Example:

\(funcall (npmjs-converge concat [upcase downcase]) \"John\").
\(funcall (npmjs-converge concat upcase downcase) \"John\")

Result: \"JOHNjohn\"."
  `(lambda (&rest args)
     (apply
      ,@(npmjs--expand combine-fn)
      (list
       ,@(mapcar (lambda (v)
                   `(apply ,@(npmjs--expand v) args))
                 (if (vectorp (car functions))
                     (append (car functions) nil)
                   functions))))))

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

;; nvm utilities
(defconst npmjs-nvm-version-re
  "v[0-9]+\\.[0-9]+\\.[0-9]+"
  "Regex matching a Node version.")

(defmacro npmjs-nvm-with-env-vars (version &rest body)
  "Set nvm variables for node VERSION in the environment and execute BODY.
VARIABLES is a list of variable settings of the form (VAR VALUE),
where VAR is the name of the variable (a string) and VALUE
is its value (also a string).

The previous values will be restored upon exit."
  (declare
   (indent defun))
  `(let ((process-environment (copy-sequence process-environment))
         (vars
          (when (and (npmjs-nvm-path)
                     ,version)
            (npmjs-nvm-get-env-vars ,version))))
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
     (setq npmjs--current-node-version
           (or npmjs--current-node-version
               (if (npmjs-nvm-path)
                   (npmjs-confirm-node-version)
                 (replace-regexp-in-string "^v" ""
                                           (car
                                            (process-lines
                                             "node"
                                             "-v"))))))
     ,@body))

(defun npmjs-nvm-path ()
  "Return path to NVM_DIR if exists."
  (when-let* ((nvm-dir (or (getenv "NVM_DIR")
                           (when (file-exists-p "~/.nvm/")
                             "~/.nvm/")))
              (file (expand-file-name "nvm.sh" nvm-dir)))
    (when (file-exists-p file)
      file)))

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
  "Strip prefix from VERSION, e.g. v1.0.0 to 1.0.0."
  (if (and version (string-match-p npmjs-nvm-version-re version))
      (substring-no-properties version 1)
    version))

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
       (when-let ((name
                   (when (file-directory-p it)
                     (file-name-nondirectory
                      (directory-file-name
                       it)))))
         (if (string-match-p (concat npmjs-nvm-version-re "$")
                             name)
             (list it)
           (directory-files it t npmjs-nvm-version-re))))
     files)))

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
         (when possible-versions
           (car (sort possible-versions
                      (lambda (a b)
                        (not (npmjs-nvm-version-compare
                              (npmjs-nvm--version-from-string
                               (car a))
                              (npmjs-nvm--version-from-string
                               (car b)))))))))))))

(defun npmjs-nvm-get-nvmrc-required-node-version ()
  "Lookup and read `.nvmrc' file."
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
    (setq choice (completing-read
                  "Version: "
                  (lambda (str pred action)
                    (if
                        (eq action 'metadata)
                        `(metadata
                          (annotation-function .
                                               (lambda
                                                 (it)
                                                 (when (equal
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
                            (concat
                             (or (getenv "NVM_DIR")
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

(defun npmjs-current-default-node-version ()
  "Return current default node version."
  (let ((default-directory (expand-file-name "~/")))
    (npmjs-nvm-strip-prefix
     (string-trim
      (shell-command-to-string
       "node -v")))))

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

(defun npmjs--install-nvm ()
  "Install nvm."
  (npmjs-message
   "Loading https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh")
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
    (npmjs-message "Installing nvm-sh: %s"
                   (with-temp-buffer
                     (insert script)
                     (shell-command-on-region (point-min)
                                              (point-max)
                                              "sh")))
    (npmjs-message "Installing node")
    (npmjs-exec-in-dir (string-join
                        (list "source"
                              (shell-quote-argument (npmjs-nvm-path))
                              "&&" "nvm"
                              "install"
                              "node")
                        "\s"))))

;;;###autoload
(defun npmjs-install-nvm (&optional force)
  "Download and install nvm and node with prompt unless FORCE is non nil."
  (interactive "P")
  (when (or force
            (yes-or-no-p "Download and install nvm?"))
    (npmjs--install-nvm)))

;;;###autoload
(defun npmjs-ensure-nvm-install ()
  "Ask user whether to install nvm and node if nvm directory doesn't exists."
  (interactive)
  (unless (npmjs-nvm-path)
    (npmjs-install-nvm)))

;;;###autoload
(defun npmjs-nvm-use-other-node-version ()
  "Use other installed node version in current buffer."
  (interactive)
  (npmjs-confirm-node-version t)
  (when transient-current-command
    (npmjs)))

;;;###autoload
(defun npmjs-nvm-use-switch-system-node-version (version)
  "Modify environment variables to use VERSION of node."
  (interactive (list (npmjs-confirm-node-version t)))
  (let ((vars (npmjs-nvm-get-env-vars version)))
    (dolist (elem vars)
      (setenv
       (car elem)
       (cadr elem)))
    (npmjs)))

;;;###autoload
(defun npmjs-nvm-jump-to-installed-node ()
  "Read and jump to installed node version."
  (interactive)
  (let ((cell (npmjs-nvm-read-installed-node-versions)))
    (if (and (cdr cell)
             (file-exists-p (cdr cell))
             (find-file (cdr cell)))
        (user-error "Not found %s" cell))))

;; nvm end
(defun npmjs-get-npm-version ()
  "Return cons with description and parsed npm help output."
  (npmjs-nvm-with-current-node-version
   (string-trim (shell-command-to-string "npm -v"))))

(defun npmjs-online-p ()
  "Check internet connection and return non-nil if so."
  (if (fboundp 'network-interface-list)
      (seq-some (lambda (iface)
                  (unless (equal "lo" (car iface))
                    (member 'up (car (last (network-interface-info
                                            (car iface)))))))
                (network-interface-list))
    t))

(defvar npmjs-json-hash (make-hash-table :test 'equal))

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
                  (npmjs-json-parse-string
                   (buffer-string)
                   'alist
                   'list)))
          (setq cache (list
                       :tick tick
                       :json content-json))
          (puthash file cache npmjs-json-hash))
        (plist-get cache :json))
    (error (npmjs-message "Could't read %s as json" file))))

(defun npmjs-remove-package-json-cache ()
  "Cleanup `npmjs-json-hash'."
  (dolist (key
           (hash-table-keys npmjs-json-hash))
    (remhash key npmjs-json-hash)))

(defun npmjs-get-project-root ()
  "Look up directory hierarchy for directory containing package.json.
Return full path of containing directory or nil."
  (locate-dominating-file default-directory "package.json"))

(defun npmjs-get-package-json-path ()
  "Look up directory hierarchy for directory containing package.json.
Return absolute path to package.json or nil."
  (when-let ((project-root (npmjs-get-project-root)))
    (expand-file-name "package.json" project-root)))

(defun npmjs-get-package-json-alist ()
  "Return a JSON representation of nearest package.json in alist format."
  (when-let ((package-json-file (npmjs-get-package-json-path)))
    (ignore-errors (npmjs-read-json package-json-file
                                    'alist))))

(defun npmjs-get-package-json-scripts ()
  "Return alist of scripts in the nearest package.json.
Every script is a cons cell, where `car' is a symbol, and `cdr' is a string."
  (alist-get 'scripts (npmjs-get-package-json-alist)))

(defun npmjs-get-package-json-script (script)
  "Search for SCRIPT in package.json.
SCRIPT should be a symbol."
  (alist-get script
             (npmjs-get-package-json-scripts)))

(defun npmjs-find-exec-nodejs ()
  "Return absolute filename to node.js.
The version is specified in the local variable `npmjs--current-node-version'."
  (seq-find (lambda (it)
              (and (string-match-p "/node/" it)
                   (string-match-p "/bin/" it)))
            (or (npmjs-nvm-with-current-node-version
                 (parse-colon-path (getenv "PATH")))
                (executable-find "node"))))

(defun npmjs-find-npm-exec ()
  "Find executable PROGRAM in current node version."
  (seq-find (lambda (it)
              (and (string-match-p "/node/" it)
                   (string-match-p "/bin/" it)))
            (or (npmjs-nvm-with-current-node-version
                 (parse-colon-path (getenv "PATH")))
                (executable-find "npm"))))

(defun npmjs-find-exec-nodejs-program (program)
  "Find executable PROGRAM in current node version."
  (expand-file-name program
                    (seq-find (lambda (it)
                                (and (string-match-p "/node/" it)
                                     (string-match-p "/bin/" it)))
                              (or (npmjs-nvm-with-current-node-version
                                   (parse-colon-path (getenv "PATH")))
                                  (executable-find "node")))))

(defmacro npmjs-with-temp-buffer (&rest body)
  "Evaluate BODY in a temporary buffer with current npm environment."
  (let ((version (make-symbol "version")))
    `(npmjs-nvm-with-current-node-version
      (let ((,version npmjs--current-node-version))
        (with-temp-buffer
          (npmjs-nvm-with-env-vars
            (setq npmjs--current-node-version
                  ,version)
            (parse-colon-path (getenv "PATH"))
            ,@body))))))

(defun npmjs-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Return stdout output if command existed with zero status, nil otherwise."
  (npmjs-with-temp-buffer
   (let ((status (apply #'call-process command nil t nil
                        (flatten-list args))))
     (let ((result (string-trim (buffer-string))))
       (if (zerop status)
           result
         (npmjs-message result) nil)))))

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
                 (npmjs-message "finished")
                 (when callback
                   (funcall callback output))
                 (when (bufferp (process-buffer process))
                   (kill-buffer (process-buffer process))))
             (user-error "%s\n%s" command output)))))
      (require 'comint)
      (when (fboundp 'comint-output-filter)
        (set-process-filter proc #'comint-output-filter)))))

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
  (let* ((proj (npmjs-get-project-root))
         (dirs (npmjs-get-workspaces))
         (names (mapcar (lambda (proj-root)
                          (alist-get
                           'name
                           (npmjs-read-json
                            (expand-file-name
                             "package.json"
                             proj-root)
                            'alist)))
                        dirs))
         (relative-dirs
          (delete-dups
           (mapcan (lambda (it)
                     (let ((parent (file-name-parent-directory it))
                           (relname (file-relative-name it
                                                        proj)))
                       (delq nil
                             (list
                              relname
                              (unless (or
                                       (file-equal-p proj parent)
                                       (file-equal-p proj it))
                                (file-relative-name parent
                                                    proj))))))
                   dirs))))
    (completing-read-multiple (or prompt "Workspace: ")
                              (append names relative-dirs dirs) nil nil
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

(defvar npmjs-history-dependencies nil)

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
  "Run a npmjs command interpreter session for COMMAND with ARGS and VERSION."
  (let* ((default-directory (or (npmjs-get-project-default-directory)
                                default-directory))
         (version  (or version
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
        (when (fboundp 'comint-output-filter)
          (set-process-filter process #'comint-output-filter))
        (set-process-sentinel process
                              #'npmjs--process-sentinel)))
    (unless (and (get-buffer-window buffer)
                 (window-live-p (get-buffer-window buffer)))
      (with-selected-window (or (window-right (minibuffer-selected-window))
                                (window-left (minibuffer-selected-window))
                                (window-right (selected-window))
                                (window-left (selected-window))
                                (split-window-sensibly))
        (pop-to-buffer-same-window buffer t)))))

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
    (when (fboundp 'projectile-compile-project)
      (define-key map [remap projectile-compile-project]
                  npmjs-compile-command))
    (when (fboundp 'project-compile)
      (define-key map [remap project-compile]
                  npmjs-compile-command))
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

(defun npmjs-project-display-name ()
  "Return project name either from package.json or using it's directory."
  (let* ((package-json (npmjs-get-package-json-alist))
         (name (cdr (assoc 'name package-json)))
         (version (cdr
                   (assoc 'version package-json))))
    (cond ((and name version)
           (format "%s@%s" name version))
          (name name)
          ((setq name (npmjs-get-project-root))
           (file-name-nondirectory (directory-file-name name))))))

(defun npmjs--get-global-buffer-name ()
  "Get a create a suitable compilation buffer."
  (format "npmjs<global><%s>" npmjs--current-node-version))

(defun npmjs--get-project-buffer-name ()
  "Get a create a suitable compilation buffer."
  (when-let ((name (npmjs-get-project-root)))
    (format "npmjs<%s>" (replace-regexp-in-string "^~/\\|/$" "" name))))

(defun npmjs-get-project-default-directory ()
  "Get a create a suitable compilation buffer."
  (when-let ((name (npmjs-get-project-root)))
    (expand-file-name name)))

(defun npmjs--get-buffer ()
  "Get a create a suitable compilation buffer."
  (if (eq major-mode 'npmjs-mode)
      (current-buffer)
    (get-buffer-create
     (or
      (npmjs--get-project-buffer-name)
      (npmjs--get-global-buffer-name)))))

(defun npmjs--process-sentinel (proc state)
  "Process sentinel helper to run hooks after PROC finishes.
Also show message when STATE changed."
  (with-current-buffer (process-buffer proc)
    (npmjs-message "%s: %s" npmjs--current-command state)
    (run-hooks 'npmjs-finished-hook)))

(defvar npmjs-multi-completion-map
  (let ((map (make-sparse-keymap)))
    (define-key map
                (kbd "C-<return>")
                #'npmjs-throw-done)
    (define-key map (kbd "C-M-j")
                #'npmjs-throw-done)
    map)
  "Keymap used in `npmjs-multi-completing-read-annotated'.")


(defun npmjs-dependencies-get-ivy-map ()
  "Return adapted for ivy `npmjs-multi-completion-map'."
  (let ((map (make-sparse-keymap))
        (iv-keys
         (when (boundp 'ivy-minibuffer-map)
           (where-is-internal 'ivy-done ivy-minibuffer-map))))
    (dolist (key iv-keys)
      (define-key map key #'npmjs-ivy-throw-done))
    (dolist (key
             (where-is-internal 'npmjs-throw-done npmjs-multi-completion-map))
      (define-key map key #'npmjs-search-throw-done))
    map))

;;;###autoload
(defun npmjs-search-throw-done ()
  "Throw done with `ivy-text'."
  (interactive)
  (when (boundp 'ivy-text)
    (npmjs-throw-done ivy-text)))

;;;###autoload
(defun npmjs-ivy-throw-done ()
  "Throw either current ivy candidate or text."
  (interactive)
  (when (and
         (fboundp 'ivy-state-current)
         (boundp 'ivy-last)
         (boundp 'ivy-text))
    (let ((text ivy-text)
          (current (ivy-state-current ivy-last))
          (cand))
      (setq cand (or
                  (when current
                    (car (split-string current "[\s\t]" t)))
                  text))
      (npmjs-throw-done cand))))

(defun npmjs-ivy-minibuffer-setup ()
  "Setup keymap for `npmjs-ivy-read-npm-dependency'."
  (use-local-map
   (make-composed-keymap
    (npmjs-dependencies-get-ivy-map)
    (current-local-map))))

;;;###autoload
(defun npmjs-ivy-read-npm-dependency (&optional prompt initial-input history)
  "Call the npm search shell command with PROMPT, INITIAL-INPUT, and HISTORY.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (when (progn
          (require 'ivy nil t)
          (require 'counsel nil t)
          (and (fboundp 'ivy-more-chars)
               (fboundp 'counsel--async-command)
               (fboundp 'ivy-read)
               (fboundp 'counsel-delete-process)))
    (catch 'done
      (unwind-protect
          (progn
            (add-hook 'minibuffer-setup-hook #'npmjs-ivy-minibuffer-setup
                      t)
            (ivy-read
             (concat (string-trim (or prompt "Repo:")) " ")
             (lambda (str)
               (or
                (ivy-more-chars)
                (progn
                  (counsel--async-command
                   (concat "npm search --prefer-offline --no-color --parseable "
                           str))
                  '("" "working..."))))
             :initial-input
             (when initial-input
               (string-join (split-string
                             initial-input "[\s\t,]+"
                             t)
                            ","))
             :dynamic-collection t
             :history (or
                       history
                       'npmjs-history-dependencies)
             :action (lambda (c)
                       (or (car (split-string (or c
                                                  (when (boundp
                                                         'ivy-text)
                                                    ivy-text)
                                                  "")
                                              nil t))))
             :unwind #'counsel-delete-process
             :caller 'npmjs-ivy-read-npm-dependency))
        (remove-hook 'minibuffer-setup-hook #'npmjs-ivy-minibuffer-setup)))))

(defun npmjs-read-new-dependency (&optional prompt initial-input history)
  "Read dependency in minibuffer with completions from npm search command.
This function will use `ivy-read' for async completions when available.
If PROMPT is non, prompting with string PROMPT.
INITIAL-INPUT can be given as the initial minibuffer input.
The third arg HISTORY, if non-nil, specifies a history list and optionally the
 initial position in the list."
  (cond ((and (fboundp 'ivy-more-chars)
              (fboundp 'counsel--async-command)
              (fboundp 'ivy-read)
              (fboundp 'counsel-delete-process))
         (npmjs-ivy-read-npm-dependency prompt initial-input history))
        (t (npmjs-search-package))))

(defvar npmjs-bmenu-search-buff-name "*npm search*")
(defvar npmjs-bmenu-search-output nil)

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

(defun npmjs-search-package-fn (args buff)
  "Setup minibuffer BUFF for searching ARGS."
  (when-let ((input
              (with-current-buffer buff
                (use-local-map
                 (let ((map (make-sparse-keymap)))
                   (define-key map
                               [remap next-line]
                               #'npmjs-search-package--next-line)
                   (define-key map
                               [remap
                                previous-line]
                               #'npmjs-search-package--prev-line)
                   (define-key map
                               [remap
                                set-mark-command]
                               #'npmjs-search-package-mark-or-unmark)
                   (define-key map
                               [remap
                                scroll-down-command]
                               #'npmjs-search-package--beg-of-buffer)
                   (define-key map
                               [remap
                                scroll-up-command]
                               #'npmjs-search-package--end-of-buffer)
                   (set-keymap-parent
                    map
                    (current-local-map))
                   map))
                (ignore-errors
                  (car
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

;;;###autoload
(defun npmjs-search-package (&optional args)
  "Incremental search of npm packages with ARGS."
  (interactive)
  (let ((timer nil))
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (when (minibufferp)
                (setq timer (run-with-idle-timer
                             0.2 #'repeat
                             #'npmjs-search-package-fn
                             args
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
    (require 'json)
    (let ((json-object-type (or object-type 'alist))
          (json-array-type
           (pcase array-type
             ('list 'list)
             ('array 'vector)
             (_ 'vector)))
          (json-null (or null-object :null))
          (json-false (or false-object :false)))
      (json-read-from-string str))))

(defun npmjs-parse-json-from-output (cmd &rest args)
  "Execute CMD with ARGS synchronously and parse json output."
  (npmjs-nvm-with-current-node-version
   (with-temp-buffer
     (if (zerop (apply #'call-process (npmjs-find-exec-nodejs-program cmd) nil t
                       nil
                       (flatten-list args)))
         ;; npm output often contains warnings even with --json flag
         ;; so we need to cut json slice
         (let ((beg (progn (goto-char (point-min))
                           (when (re-search-forward "{" nil t 1)
                             (1- (point))))))
           (goto-char (point-max))
           (when (re-search-backward "}" nil t 1)
             (npmjs-json-parse-string
              (buffer-substring-no-properties
               beg
               (1+
                (point)))
              'alist
              'list)))
       (npmjs-message (buffer-string))))))

(defun npmjs-global-packages ()
  "Return list of globally installed packages."
  (npmjs-pluck-depenencies
   (npmjs-json-parse-string
    (npmjs-nvm-with-current-node-version
     (with-temp-buffer (shell-command
                        "npm ls --global --json"
                        (current-buffer))
                       (let ((beg)
                             (end))
                         (when (re-search-forward "{"
                                                  nil t
                                                  1)
                           (setq beg (1- (point))))
                         (goto-char (point-max))
                         (when (re-search-backward "}"
                                                   nil t
                                                   1)
                           (setq end (1+ (point))))
                         (buffer-substring-no-properties
                          beg
                          end)))))))

(defun npmjs-global-package-completion-table ()
  "Read globally installed js packages."
  (let* ((alist (npmjs-global-packages))
         (annotf (lambda (it)
                   (let ((value (cdr-safe (assoc it alist))))
                     (concat "@" (if (listp value)
                                     (string-join value " ")
                                   (or value "")))))))
    (lambda (str pred action)
      (if (eq action 'metadata)
          `(metadata
            (annotation-function . ,annotf))
        (complete-with-action action alist str pred)))))

(defun npmjs-local-package-completion-table ()
  "Read globally installed js packages."
  (let* ((alist (npmjs-local-packages))
         (annotf (lambda (it)
                   (let ((value (cdr-safe (assoc it alist))))
                     (concat "@" (if (listp value)
                                     (string-join value " ")
                                   (or value "")))))))
    (lambda (str pred action)
      (if (eq action 'metadata)
          `(metadata
            (annotation-function . ,annotf))
        (complete-with-action action alist str pred)))))

(defun npmjs-local-packages ()
  "Return list of installed packages from nearest package.json."
  (if-let* ((package-json-file (npmjs-get-package-json-path))
            (package-json (ignore-errors (npmjs-read-json package-json-file
                                                          'alist))))
      (npmjs-pluck-depenencies package-json)
    '()))

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
                (if-let ((deps
                          (when-let ((found
                                      (alist-get it package-json-alist)))
                            (mapcar (lambda (cell)
                                      (let* ((symb (car cell))
                                             (value (cdr cell))
                                             (version
                                              (if (stringp value)
                                                  value
                                                (cdr-safe (assoc 'version
                                                                 value))))
                                             (type
                                              (pcase it
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

(defun npmjs-get-package-dist-tags (package)
  "Exec \"npm view\" for PACKAGE and return alist."
  (let ((data (npmjs-get-package-info package)))
    (mapcar #'car (alist-get 'dist-tags data))))

(defun npmjs-get-package-info (package)
  "Exec \"npm view\" for PACKAGE and return alist."
  (npmjs-parse-json-from-output "npm" "view" "--json" package))

(defun npmjs-get-package-versions (package)
  "Exec \"npm info\" for PACKAGE and return list of available versions."
  (require 'json)
  (let ((data (npmjs-get-package-info package)))
    (nconc
     (mapcar #'cdr (alist-get 'dist-tags data))
     (alist-get 'versions
                data))))




(defun npmjs-confirm-package-dist-tags (package &optional prompt)
  "Confirm PACKAGE dist-tag with PROMPT."
  (let* ((dist-tags (or (npmjs-stringify
                         (ignore-errors
                           (alist-get 'dist-tags (npmjs-get-package-info
                                                  package))))
                        (list (list "latest" "not found"))))
         (annotate-fn (lambda (it)
                        (concat " "
                                (or
                                 (when-let
                                     ((vers (cdr (assoc-string it
                                                               dist-tags))))
                                   (format "(%s)" vers))
                                 ""))))
         (tag (completing-read
               (or prompt (format "%s: " package))
               (lambda (str pred action)
                 (if (eq action 'metadata)
                     `(metadata
                       (annotation-function . ,annotate-fn))
                   (complete-with-action action dist-tags str pred))))))
    (format "%s@%s" package tag)))

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

(defun npmjs-read-script (&optional prompt input hist)
  "Read package json script with PROMPT, INPUT and HIST."
  (let* ((alist
          (npmjs-get-package-json-scripts))
         (annotf
          (lambda (it)
            (concat ": " (cdr (assoc (intern it) alist))))))
    (completing-read (or prompt "Script: ")
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
                          pred)))
                     nil nil input hist)))

(defvar npmjs-man-paths nil)
(defvar Man-notify-method)
(defvar manual-program)


(defun npmjs-man-paths-files ()
  "Return man files for npm."
  (npmjs-nvm-with-env-vars npmjs--current-node-version
    (let* ((found (string-trim (shell-command-to-string
                                "which npm")))
           (dir (file-name-parent-directory
                 (file-name-parent-directory found))))
      (directory-files-recursively (expand-file-name
                                    "lib/node_modules/npm/man"
                                    dir)
                                   (string-join
                                    (mapcar #'car
                                            (seq-filter
                                             (lambda (it)
                                               (eq
                                                'nroff-mode
                                                (cdr it)))
                                             auto-mode-alist))
                                    "\\|")))))

(defun npmjs-view-npm-command-man (command)
  "Find and view manual for npm COMMAND."
  (when-let ((files (npmjs-man-paths-files))
             (file (seq-find (lambda (it)
                               (string= command (file-name-base it)))
                             files)))
    (let ((inhibit-message t))
      (npmjs-view-man-file file))))

;;;###autoload
(defun npmjs-show-help ()
  "Show help for current npm version."
  (interactive)
  (npmjs-run-as-comint "npm -l"))

;;;###autoload
(defun npmjs-show-manual (&optional command)
  "Read manual for npm COMMAND."
  (interactive (list
                (let* ((inhibit-message t)
                       (alist (mapcar (lambda (it)
                                        (cons (file-name-base it) it))
                                      (npmjs-man-paths-files)))
                       (action (lambda (it)
                                 (npmjs-view-man-file
                                  (if (consp it)
                                      (cdr
                                       it)
                                    (cdr (assoc-string it
                                                       alist)))))))
                  (if (and (eq completing-read-function
                               'ivy-completing-read)
                           (fboundp 'ivy-read))
                      (ivy-read "Man for" alist
                                :preselect
                                (when transient--prefix
                                  (get (oref transient--prefix command)
                                       'man-page))
                                :action
                                (lambda (it)
                                  (if
                                      (bound-and-true-p ivy-exit)
                                      it
                                    (funcall action it))))
                    (completing-read "Man: " alist
                                     nil t
                                     (when transient--prefix
                                       (oref transient--prefix command)))))))
  (npmjs-view-man-file
   (if (consp command)
       (cdr
        command)
     (seq-find (lambda (it)
                 (string= command (file-name-base it)))
               (npmjs-man-paths-files)))))

(defun npmjs-view-man-file (file)
  "Run man on FILE."
  (require 'man)
  (let* ((topic (shell-quote-argument file))
         (buff-name (concat "*Man " topic "*"))
         (viewbuf (get-buffer buff-name)))
    (when viewbuf
      (kill-buffer viewbuf))
    (when (fboundp 'Man-getpage-in-background)
      (npmjs-nvm-with-current-node-version
       (let* ((inhibit-message t)
              (Man-notify-method 'meek)
              (buff (Man-getpage-in-background topic))
              (proc (and buff (get-buffer-process buff))))
         (while (and proc (eq (process-status proc) 'run))
           (sit-for 0.1 proc))
         (when (buffer-live-p buff)
           (if-let ((wnd (or (seq-find
                              (npmjs--compose
                                (apply-partially #'string-match-p "*Man ")
                                buffer-name window-buffer)
                              (window-list))
                             (or (window-right (minibuffer-selected-window))
                                 (window-left (minibuffer-selected-window)))
                             (or (window-right (selected-window))
                                 (window-left (selected-window))
                                 (split-window-sensibly)))))
               (with-selected-window wnd
                 (pop-to-buffer-same-window buff))
             (pop-to-buffer buff)))
         (unless (active-minibuffer-window)
           (select-window (get-buffer-window buff))))))))

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
(defun npmjs-key-builder-shared-start (s1 s2)
  "Return the longest prefix S1 and S2 have in common."
  (declare (pure t)
           (side-effect-free t))
  (let ((search-length (min (length s1)
                            (length s2)))
        (i 0))
    (while (and (< i search-length)
                (= (aref s1 i)
                   (aref s2 i)))
      (setq i (1+ i)))
    (substring s1 0 i)))

(defun npmjs-key-builder-capitalize-variants (word)
  "Return list of words of WORD, but it with upcased letter."
  (let ((cands)
        (parts (split-string word "" t)))
    (dotimes (i (length parts))
      (let ((val (string-join (remove nil (list
                                           (when (> i 0)
                                             (string-join
                                              (seq-take parts i) ""))
                                           (upcase (nth i parts))
                                           (string-join
                                            (seq-drop parts (1+ i))
                                            "")))
                              "")))
        (push val
              cands)))
    (reverse cands)))

(defun npmjs-key-builder-safe-substring (len word)
  "Substring WORD from zero to LEN."
  (if (> (length word) len)
      (substring-no-properties word 0 len)
    word))

(defun npmjs-key-builder-get-all-key-strategies (word len)
  "Generate preffered shortcut from WORD with length LEN."
  (let* ((parts (append (split-string word "[^a-z]" t)
                        (list (replace-regexp-in-string "[^a-z]" "" word))))
         (parts-len (length parts))
         (finalize (lambda (short)
                     (while (> len (length short))
                       (setq short (concat short
                                           (number-to-string (random 10)))))
                     (npmjs-key-builder-safe-substring len short)))
         (vars
          (mapcar finalize (npmjs-key-builder-capitalize-variants
                            (npmjs-key-builder-safe-substring
                             len
                             (replace-regexp-in-string
                              "[^a-z]"
                              ""
                              word))))))
    (seq-sort-by
     (lambda (it)
       (cond ((string-match-p "[0-9]" it)
              -2)
             ((member it vars)
              -1)
             (t (length (npmjs-key-builder-shared-start word it)))))
     #'>
     (seq-uniq (append
                vars
                (mapcar
                 (lambda (n)
                   (funcall finalize (mapconcat
                                      (apply-partially
                                       #'npmjs-key-builder-safe-substring n)
                                      parts "")))
                 (number-sequence 1 (min len parts-len)))
                (mapcar
                 (lambda (n)
                   (funcall finalize (mapconcat
                                      (apply-partially
                                       #'npmjs-key-builder-safe-substring n)
                                      (reverse parts) "")))
                 (number-sequence 1 (min len parts-len))))))))

(defun npmjs-key-builder--generate-shortcuts (items &optional key-fn value-fn
                                                    used-keys key-len)
  "Generate shortcuts from list of ITEMS.
If KEY-FN is nil, ITEMS should be list of strings or symbols.
If KEY-FN is a function, it will be called with every item of list, and should
return string that will be as basis for shortcut.
If VALUE-FN is nil, result is an alist of generated keys and corresponding
items.
If VALUE-FN is non nil, return a list of results of calling VALUE-FN with two
arguments - generated shortcut and item.
USED-KEYS is a list of keys that shouldn't be used.
KEY-LEN is minimal length of keys."
  (let* ((value-fn (or value-fn (lambda (key value)
                                  (if (proper-list-p value)
                                      (append (list key) value)
                                    (cons key value)))))
         (total (length items))
         (random-variants (append
                           (mapcar #'char-to-string
                                   (number-sequence (string-to-char
                                                     "a")
                                                    (string-to-char
                                                     "z")))
                           (mapcar #'char-to-string
                                   (number-sequence (string-to-char
                                                     "A")
                                                    (string-to-char
                                                     "Z")))
                           (list "@" ".")))
         (variants-len (length random-variants))
         (min-len
          (or key-len
              (if used-keys
                  (length (car (seq-sort-by #'length #'> used-keys)))
                (cond ((>= variants-len total)
                       1)
                      ((>= variants-len (/ total 2))
                       2)
                      (t 3))))))
    (let ((shortcuts used-keys)
          (used-words '())
          (all-keys (mapcar (lambda (def)
                              (if key-fn
                                  (funcall key-fn def)
                                (if (symbolp def)
                                    (symbol-name def)
                                  def)))
                            items))
          (result))
      (dotimes (i (length items))
        (when-let* ((def (nth i items))
                    (word (if key-fn
                              (funcall key-fn def)
                            (if (symbolp def)
                                (symbol-name def)
                              def))))
          (when (not (member word used-words))
            (push word used-words)
            (let ((short
                   (downcase
                    (substring-no-properties word 0
                                             (min min-len
                                                  (length word))))))
              (setq short (if (string-match-p short "[a-z]")
                              (replace-regexp-in-string "[^a-z]" "" short)
                            short))
              (setq short
                    (seq-find
                     (lambda (it)
                       (not
                        (seq-find (apply-partially
                                   #'string-prefix-p it)
                                  shortcuts)))
                     (append
                      (npmjs-key-builder-get-all-key-strategies word
                                                                min-len)
                      (or (seq-remove (lambda (key)
                                        (seq-find (apply-partially
                                                   #'string-prefix-p
                                                   (downcase key))
                                                  all-keys))
                                      random-variants)
                          random-variants)
                      ;; (when (= min-len 1)
                      ;;   )
                      )))
              (while (and
                      (< (length short) min-len))
                (setq short (concat short (number-to-string (random 10)))))
              (push short shortcuts)
              (push
               (cond ((functionp value-fn)
                      (funcall value-fn short def))
                     (t (cons short def)))
               result)))))
      (reverse result))))

(defun npmjs-key-builder-generate-shortcuts (items &optional key-fn value-fn
                                                   used-keys key-len)
  "Generate shortcuts from list of ITEMS.
If KEY-FN is nil, ITEMS should be list of strings or symbols.
If KEY-FN is a function, it will be called with every item of list, and should
return string that will be as basis for shortcut.
If VALUE-FN is nil, result is an alist of generated keys and corresponding
items.
If VALUE-FN is non nil, return a list of results of calling VALUE-FN with two
arguments - generated shortcut and item.
USED-KEYS is a list of keys that shouldn't be used.
KEY-LEN is minimal length of keys."
  (npmjs-key-builder--generate-shortcuts items key-fn value-fn
                                         used-keys key-len))

(defun npmjs-key-builder-take-description (item)
  "Return description from transient ITEM."
  (when (proper-list-p item)
    (if (stringp (nth 1 item))
        (nth 1 item)
      (when-let ((d (car (seq-drop
                          (memq
                           :description
                           item)
                          1))))
        (if (functionp d)
            (funcall d)
          d)))))

(defun npmjs-key-builder-take-key (item)
  "Return key from transient ITEM."
  (when (stringp (car-safe item))
    (car item)))

(defun npmjs-make-command-name (&rest args)
  "Make name from ARGS."
  (let ((name (mapconcat (npmjs--compose
                           string-trim
                           (apply-partially #'format "%s"))
                         (delq nil
                               (mapcar #'npmjs-unquote
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

(defun npmjs-substitute-hints (str)
  "Remove hints from STR."
  (with-temp-buffer
    (insert
     str)
    (goto-char (point-min))
    (while (re-search-forward "[@./=]?\\(<\\([a-z0-9-/@][^>]+\\)[>][\s./=]?\\)+"
                              nil t 1)
      (replace-match ""))
    (buffer-string)))

(defun npmjs-arg-sort-transformer (argument)
  "Transform transient argument ARGUMENT to sort score as integer."
  (let ((arg (if (listp argument)
                 (car argument)
               argument)))
    (cond ((string= "--" arg)
           -4)
          ((string-prefix-p "--" arg)
           -3)
          ((string-prefix-p "-" arg)
           -2)
          ((string-prefix-p "<" arg)
           -1)
          (t 0))))

(defun npmjs-sort-args (args)
  "Sort transient arguments ARGS."
  (seq-sort-by #'npmjs-arg-sort-transformer #'> args))

(defun npmjs-format-args (args)
  "Format transient arguments ARGS to string."
  (let ((new-args (mapcar
                   (lambda (it)
                     (cond ((listp it)
                            (string-join it " "))
                           (t it)))
                   args)))
    (replace-regexp-in-string "=[\s]" "="
                              (npmjs-substitute-hints
                               (string-join new-args "\s")))))

(defun npmjs-get-arguments ()
  "Return current transient arguments ARGS."
  (let ((raw-args))
    (cond (transient-current-command
           (setq raw-args (transient-args transient-current-command)))
          (transient--prefix
           (setq transient-current-prefix transient--prefix)
           (setq transient-current-command (oref transient--prefix command))
           (setq transient-current-suffixes transient--suffixes)
           (setq raw-args (transient-args transient-current-command))))
    (npmjs-sort-args (npmjs-stringify raw-args))))

(defun npmjs-get-formatted-transient-args ()
  "Return string with formatted current transient arguments."
  (npmjs-format-args (npmjs-get-arguments)))

(defun npmjs-group-vectors (arguments &optional height win-width)
  "Group ARGUMENTS into vector.
Default value for HEIGHT is `max-mini-window-height',
and for WIN-WIDTH - window width."
  (let* ((descriptions
          (sort
           (mapcar (lambda (&rest args)
                     (length
                      (apply
                       (lambda
                         (&rest args)
                         (apply #'concat
                                (list
                                 (apply
                                  #'npmjs-key-builder-take-key
                                  args)
                                 (apply
                                  #'npmjs-key-builder-take-description
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
          (global (or (transient-arg-value "--global"
                                           (transient-args
                                            transient-current-command))
                      (transient-arg-value "-g"
                                           (transient-args
                                            transient-current-command))
                      (not (npmjs-get-project-root))))
          (args (npmjs-get-formatted-transient-args)))
      (let* ((default-directory (expand-file-name
                                 (if global
                                     default-directory
                                   (or
                                    (npmjs-get-project-root)
                                    (vc-root-dir)
                                    default-directory))))
             (cmd
              (append
               (npmjs-make-command-name "npm" name))))
        (npmjs-confirm-and-run cmd args)))))

;;;###autoload (autoload 'npmjs-show-args "npmjs.el" nil t)
(transient-define-suffix npmjs-show-args ()
  :transient t
  (interactive)
  (let ((name (get transient-current-command 'npm-command))
        (args
         (npmjs-get-formatted-transient-args)))
    (npmjs-message
     (propertize (npmjs-make-command-name name args)
                 'face 'success))))

(defvar npmjs-known-hints '())

(defun npmjs-get-npm-config ()
  "Return npm config as alist."
  (let ((args (remove "json" (npmjs-get-arguments))))
    (setq args (seq-remove (apply-partially #'string-prefix-p "<")
                           args))
    (when-let* ((str (string-trim (npmjs-nvm-with-current-node-version
                                   (shell-command-to-string
                                    (concat "npm config list --json "
                                            (remove "json"
                                                    (npmjs-format-args
                                                     args)))))))
                (json-start (string-match-p "{" str)))
      (setq str (npmjs-json-parse-string
                 (substring-no-properties str json-start))))))

;;;###autoload
(defun npmjs-throw-done (&optional arg)
  "Throw ARG to the catch for done.
By default ARG is t."
  (interactive)
  (throw 'done (or arg t)))


(defun npmjs-stringify (item)
  "Recoursively stringify ITEM."
  (pcase item
    ((pred not)
     item)
    ((pred stringp)
     (substring-no-properties item))
    ((pred numberp)
     (number-to-string item))
    ((pred vectorp)
     (apply #'vector (mapcar #'npmjs-stringify (append item nil))))
    ((pred proper-list-p)
     (mapcar #'npmjs-stringify item))
    ((guard (and (consp item)
                 (atom (cdr item))))
     (cons (npmjs-stringify (car item))
           (npmjs-stringify (cdr item))))
    ((guard (and item (symbolp item)))
     (substring-no-properties (symbol-name item)))
    (_ item)))

(defun npmjs-single-completing-read-annotated (&optional prompt collection
                                                         annot-value predicate
                                                         require-match
                                                         initial-input hist def
                                                         inherit-input-method)
  "Read multiple strings in the minibuffer, with completion.
If ANNOT-VALUE is non nil, it will be called with value and should return
string with annotation.
PROMPT, COLLECTION, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF,
and INHERIT-INPUT-METHOD are the same as for `completing-read'."
  (let* ((table-completion
          (when (functionp collection)
            collection))
         (alist
          (unless table-completion
            (npmjs-stringify collection)))
         (value-getter (if (stringp (car-safe alist))
                           (lambda (it)
                             (seq-find (apply-partially #'stringp it) alist))
                         (lambda (it)
                           (cdr (assoc-string it alist)))))
         (annotate-fn
          (lambda (k)
            (concat
             (if-let ((value
                       (when value-getter
                         (funcall value-getter k))))
                 (if annot-value
                     (funcall annot-value value)
                   (format " %s" value))
               "")))))
    (completing-read
     (concat (string-trim (or prompt "Item"))" \s")
     (or table-completion
         (lambda (&optional str pred action)
           (if (eq action 'metadata)
               `(metadata
                 (annotation-function . ,annotate-fn))
             (complete-with-action action alist str
                                   pred))))
     predicate
     require-match
     initial-input hist def inherit-input-method)))

(defun npmjs-multi-completing-read-annotated (&optional prompt collection
                                                        annot-value
                                                        initial-input hist)
  "Read multiple strings in the minibuffer, with completion.
If ANNOT-VALUE is non nil, it will be called with value and should return
string with annotation.
PROMPT, INITIAL-INPUT, HIST and COLLECTION are the same arguments
as those for `completing-read'."
  (let* ((table-completion
          (when (functionp collection)
            collection))
         (alist
          (unless table-completion
            (npmjs-stringify collection)))
         (value-getter
          (cond (table-completion nil)
                ((and (stringp (car-safe alist))
                      annot-value)
                 (lambda (it)
                   (seq-find
                    (apply-partially #'stringp
                                     it)
                    alist)))
                (t (lambda (it)
                     (cdr (assoc-string it alist))))))
         (annotate-fn
          (lambda (k)
            (concat
             (if-let ((value
                       (when value-getter
                         (funcall k value-getter))))
                 (if annot-value
                     (funcall annot-value value)
                   (format " %s" value))
               ""))))
         (choices)
         (curr)
         (done))
    (setq done
          (catch 'done
            (while (setq curr
                         (let ((ch
                                (minibuffer-with-setup-hook
                                    (lambda ()
                                      (use-local-map
                                       (make-composed-keymap
                                        npmjs-multi-completion-map
                                        (current-local-map))))
                                  (completing-read
                                   (concat (string-trim (or prompt
                                                            "Item"))
                                           "\s"
                                           (substitute-command-keys
                                            "(`\\<npmjs-multi-completion-map>\
\\[npmjs-throw-done]' to finish)\s")
                                           (if choices
                                               (concat
                                                "("
                                                (string-join
                                                 choices
                                                 ", ")
                                                ")")
                                             ""))
                                   (or
                                    table-completion
                                    (lambda (str pred action)
                                      (if
                                          (eq action 'metadata)
                                          `(metadata
                                            (annotation-function .
                                                                 ,annotate-fn))
                                        (complete-with-action
                                         action
                                         alist
                                         str
                                         pred))))
                                   (lambda (it)
                                     (if (consp it)
                                         (not (member (car it) choices))
                                       (not (member it choices))))
                                   nil initial-input hist))))
                           (if (string-empty-p (string-trim ch))
                               (npmjs-throw-done)
                             ch)))
              (setq choices (append choices (list curr))))))
    (if (stringp done)
        (append choices (list done))
      choices)))

(defun npmjs-single-reader (fn &optional prompt initial-input history)
  "Call minibuffer reader FN with PROMPT, INITIAL-INPUT, HISTORY.
It also temporarily setup extra keymap `npmjs-multi-completion-map'."
  (let* ((curr))
    (setq curr
          (catch 'done
            (let ((ch (minibuffer-with-setup-hook
                          (lambda ()
                            (use-local-map
                             (make-composed-keymap
                              npmjs-multi-completion-map
                              (current-local-map))))
                        (funcall fn (concat (or prompt "Item")
                                            " \s")
                                 initial-input history))))
              (cond ((or (not ch)
                         (when (listp ch)
                           (not (car ch)))
                         (and (stringp ch)
                              (string-empty-p (string-trim ch))))
                     (npmjs-throw-done))
                    (t ch)))))
    (if (stringp (car-safe curr))
        (car curr)
      curr)))

(defun npmjs-multi-reader (fn &optional prompt initial-input history)
  "Call minibuffer FN with PROMPT INITIAL-INPUT HISTORY."
  (let* ((choices)
         (curr)
         (done))
    (setq done
          (catch 'done
            (while (setq curr
                         (let ((ch (minibuffer-with-setup-hook
                                       (lambda ()
                                         (use-local-map
                                          (make-composed-keymap
                                           npmjs-multi-completion-map
                                           (current-local-map))))
                                     (funcall fn (concat
                                                  (string-trim
                                                   (or prompt
                                                       "Item"))
                                                  " "
                                                  (string-trim
                                                   (if choices
                                                       (concat
                                                        "(" (string-join choices
                                                                         ", ")
                                                        ")")
                                                     ""))
                                                  " "
                                                  (substitute-command-keys
                                                   "(`\\<npmjs-multi-completion-map>\
\\[npmjs-throw-done]' to finish): "))
                                              initial-input history))))
                           (cond ((or (not ch)
                                      (when (listp ch)
                                        (not (car ch)))
                                      (and (stringp ch)
                                           (string-empty-p (string-trim ch))))
                                  (npmjs-throw-done))
                                 (t ch))))
              (setq choices (append choices
                                    (if (and curr (listp curr))
                                        curr
                                      (list curr)))))))
    (if (stringp done)
        (append choices (list done))
      choices)))

(defun npmjs-make-reader (fn &optional collection annot-value multi)
  "Create minibuffer reader from FN with PROMPT INITIAL-INPUT HISTORY.
If ANNOT-VALUE is non nil, it will be called with value and should return
string with annotation.
COLLECTION should be either function that returns collection
for `completing-read' or list of choices.
If MULTI is non nil, create FN with `npmjs-multi-reader', othervise just return
FN."
  (cond ((and fn multi)
         (apply-partially #'npmjs-multi-reader fn))
        ((and fn (not multi))
         (apply-partially #'npmjs-single-reader fn))
        ((and collection)
         (npmjs-make-completing-read-reader collection annot-value multi))
        (t fn)))

(defun npmjs-make-completing-read-reader (&optional completion-table-creator
                                                    annot-value multi-value)
  "Create reader with `completing-read' using COMPLETION-TABLE-CREATOR.
If ANNOT-VALUE is non nil, it will be called with value and should return
string with annotation.
COMPLETION-TABLE-CREATOR should be either function that returns collection
for `completing-read' or list of choices.
If MULTI-VALUE is non nil, allow multiple choices and return value is list,
othervise string."
  (if multi-value
      (lambda (&optional prompt initial-input hist)
        (npmjs-multi-completing-read-annotated prompt
                                               (if (functionp
                                                    completion-table-creator)
                                                   (funcall
                                                    completion-table-creator)
                                                 completion-table-creator)
                                               annot-value
                                               initial-input hist))
    (lambda (&optional prompt initial-input hist)
      (npmjs-single-completing-read-annotated prompt
                                              (if (functionp
                                                   completion-table-creator)
                                                  (funcall
                                                   completion-table-creator)
                                                completion-table-creator)
                                              annot-value
                                              nil nil initial-input hist))))

(defun npmjs-make-combined-package-reader (reader transformer &optional
                                                  multi-value)
  "Create minibuffer READER with TRANSFORMER.
If MULTI-VALUE is non nil, return list of packages, othervise string."
  (lambda (&optional prompt initial-input hist)
    (let ((deps (funcall (npmjs-make-reader reader nil nil
                                            multi-value)
                         prompt initial-input hist)))
      (if (stringp deps)
          (funcall transformer deps)
        (mapcar transformer deps)))))

(defun npmjs-make-npm-package-reader-with-version (&optional multi-value)
  "Create minibuffer reader for packages from npm registry with version.
If MULTI-VALUE is non nil, return list of packages, othervise string."
  (npmjs-make-combined-package-reader #'npmjs-read-new-dependency
                                      #'npmjs-confirm-package-version
                                      multi-value))

(defun npmjs-make-npm-package-reader-with-tag (&optional multi-value)
  "Create minibuffer reader for packages from npm registry with version.
If MULTI-VALUE is non nil, return list of packages, othervise string."
  (npmjs-make-combined-package-reader #'npmjs-read-new-dependency
                                      #'npmjs-confirm-package-dist-tags
                                      multi-value))

(defun npmjs-npm-read-config-key-value (&optional prompt initial-input hist)
  "Read key from npm config in minibuffer with annotated completions.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially,
  with point positioned at the end.
HIST, if non-nil, specifies a history list and optionally the initial
  position in the list."
  (let* ((config (mapcar
                  (npmjs--converge cons
                                   [(npmjs--compose
                                      substring-no-properties
                                      symbol-name
                                      car)
                                    cdr])
                  (npmjs-get-npm-config)))
         (choice (completing-read (or prompt "Key: ")
                                  (npmjs-npm-config-completion-table
                                   config)
                                  nil nil initial-input hist))
         (cell (assoc-string choice config))
         (value (cdr-safe cell)))
    (format "%s=%s" choice
            (pcase value
              (:false (read-string (format
                                    "Toggle %s (current value: false) to "
                                    choice)
                                   "true"))
              ('t (read-string (format
                                "Toggle %s (current value: true) to "
                                choice)
                               "false"))
              (_ (read-string (format "New value %s: " choice)
                              (when value
                                (pcase value
                                  ((pred (stringp))
                                   (cdr-safe cell))
                                  (:json "json")
                                  (:null "null")
                                  (_ (format "%s" value))))))))))

(defun npmjs-npm-config-completion-table (&optional config)
  "Read key from npm config in minibuffer with annotated completions.
If CONFIG is non nil, use it as collection.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially,
  with point positioned at the end.
HISTORY, if non-nil, specifies a history list and optionally the initial
  position in the list."
  (let* ((config (mapcar
                  (npmjs--converge cons
                                   [(npmjs--compose
                                     substring-no-properties
                                     (npmjs--cond
                                      [numberp number-to-string]
                                      [symbolp symbol-name]
                                      [t identity])
                                     car)
                                    cdr])
                  (or config (npmjs-get-npm-config))))
         (annotate-fn (lambda (str)
                        (format " %s" (or (cdr (assoc-string str config))
                                          "")))))
    (lambda (str pred action)
      (if (eq action 'metadata)
          `(metadata
            (annotation-function . ,annotate-fn))
        (complete-with-action action config str pred)))))

(defun npmjs-pkg-get-dependencies-table ()
  "Read installed packages, either global or local.
PROMPT INITIAL-INPUT HISTORY is the same as for completing read."
  (let* ((args
          (npmjs-get-arguments))
         (global-p (seq-intersection args '("--global" "-g"))))
    (if
        (or global-p (not (npmjs-get-project-root)))
        (npmjs-global-package-completion-table)
      (npmjs-local-package-completion-table))))

(defun npmjs-url-reader (&optional prompt initial-input hist)
  "Read url in minibuffer with PROMPT, INITIAL-INPUT and HIST."
  (completing-read prompt
                   (seq-uniq
                    (remove nil
                            (seq-filter (npmjs--cond
                                          [(npmjs--and stringp
                                                       (apply-partially
                                                        #'string-prefix-p
                                                        "https://"))
                                           substring-no-properties])
                                        (append
                                         kill-ring
                                         (mapcar #'cdr
                                                 (npmjs-get-npm-config))))))
                   nil nil initial-input hist))


(defun npmjs-file-name-relative (filename)
  "Convert FILENAME to be relative to project root or default directory."
  (if-let ((proj-root (npmjs-get-project-root)))
      (file-relative-name filename proj-root)
    (file-relative-name filename default-directory)))

(defun npmjs-read-pkg-directory (&optional prompt &rest _)
  "Read file with .tar, .tar.gz, or .tgz extension.
If PROMPT is non nil, use this prompt."
  (npmjs-file-name-relative
   (file-local-name
    (read-directory-name (or prompt "Directory file: ")))))

(defun npmjs-read-tar-file (&optional prompt &rest _)
  "Read file with .tar, .tar.gz, or .tgz extension.
If PROMPT is non nil, use this prompt."
  (npmjs-file-name-relative
   (file-local-name
    (read-file-name (or prompt "Tarball file: ")
                    nil
                    nil
                    nil
                    nil
                    (npmjs--or file-directory-p
                               (npmjs--compose
                                 (npmjs--rpartial member '("tar"
                                                           "gz"
                                                           "tgz"))
                                 file-name-extension))))))

(defun npmjs-get-scope-matches (str)
  "Return scopes from STR."
  (let ((found))
    (with-temp-buffer
      (insert str)
      (while (re-search-backward "^@[a-z-0-9]+" nil t 1)
        (push (match-string-no-properties 0) found)))
    found))

(defun npmjs-guess-scopes ()
  "Return choices which looks like a scope."
  (mapcar #'list
          (seq-uniq
           (flatten-list
            (mapcar
             (npmjs--compose
               npmjs-get-scope-matches
               substring-no-properties)
             (seq-filter
              (apply-partially #'string-match-p "^@[a-z-0-9]+")
              (append
               kill-ring
               (remove nil
                       (append
                        (mapcar #'car
                                (npmjs-pluck-depenencies
                                 (npmjs-get-package-json-alist)))
                        (mapcar
                         (npmjs--compose
                           symbol-name
                           car)
                         (npmjs-get-npm-config)))))))))))

(defvar npmjs-all-known-hints nil)
(defvar npmjs-all-args nil)
(defvar npmjs-all-args-alist nil)

(defun npmjs-get-hint-reader (str &optional cmd multi-value)
  "Return reader from hint STR for CMD.
If MULTI-VALUE is non it return multi reader."
  (add-to-list 'npmjs-all-known-hints str)
  (pcase str
    ("<command>"
     (pcase cmd
       ("run-script" (npmjs-make-reader
                      #'npmjs-read-script
                      nil
                      nil
                      multi-value))))
    ((or "<registry>" "<url>")
     (npmjs-make-reader
      #'npmjs-url-reader
      nil
      nil multi-value))
    ((or "<@orgname>" "<@scope>" "<scope>")
     (npmjs-make-reader nil
                        #'npmjs-guess-scopes nil
                        multi-value))
    ("--registry="
     (npmjs-make-reader
      #'npmjs-url-reader
      nil
      nil multi-value))
    ((or "<pkg>"
         "<package-spec>"
         "<@scope><package>"
         "<@scope><pkg>"
         "<package>"
         "<pkgname>")
     (pcase cmd
       ((or "install"
            "unpublish"
            "view")
        (npmjs-make-reader #'npmjs-read-new-dependency nil nil
                           multi-value))
       ((or "update" "uninstall" "edit" "ls")
        (npmjs-make-reader nil #'npmjs-pkg-get-dependencies-table nil
                           multi-value))
       ("init" 'read-string)
       (_ (npmjs-make-reader #'npmjs-read-new-dependency nil nil
                             multi-value))))
    ((or "<@scope><pkg>@<tag>"
         "<pkg>@<tag>"
         "<package>@<tag>"
         "<name>@<tag>")
     (pcase cmd
       ((or "update" "uninstall" "edit" "ls")
        (npmjs-make-reader nil #'npmjs-pkg-get-dependencies-table nil
                           multi-value))
       ("init" 'read-string)
       (_ (npmjs-make-npm-package-reader-with-tag multi-value))))
    ((or "<@scope><name>@<version>"
         "<pkg>@<version>"
         "<@scope>/<name>"
         "<@scope><pkg>@<version>"
         "<@scope><pkg>@<version range>")
     (pcase cmd
       ((or "update" "uninstall" "edit" "ls")
        (npmjs-make-reader nil #'npmjs-pkg-get-dependencies-table nil
                           multi-value))
       ("init" 'read-string)
       (_ (npmjs-make-npm-package-reader-with-version multi-value))))
    ("<key>"
     (pcase (car (split-string cmd " " t))
       ((or "get" "config" "set")
        (npmjs-make-reader nil #'npmjs-npm-config-completion-table nil
                           multi-value))))
    ((or "<tarball>"
         "<tarball file>")
     (npmjs-make-reader #'npmjs-read-tar-file nil nil
                        multi-value))
    ("<file>"
     (npmjs-make-reader #'transient-read-file nil nil
                        multi-value))
    ("<depth>"
     (npmjs-make-reader #'transient-read-number-N0 nil nil
                        multi-value))
    ("<call>" (npmjs-make-reader (npmjs--compose
                                   (npmjs--cond
                                     [string-empty-p identity]
                                     [(npmjs--and (apply-partially
                                                   #'string-prefix-p "'")
                                                  (apply-partially
                                                   #'string-suffix-p "'"))
                                      identity]
                                     [t (apply-partially #'format "'%s'")])
                                   (apply-partially #'read-string)) nil nil
                                   multi-value))
    ((or "<folder>")
     (npmjs-make-reader #'transient-read-existing-directory nil nil
                        multi-value))
    ("<key>=<value>"
     (pcase (car (split-string cmd " " t))
       ((or "config" "set" "get")
        (npmjs-make-reader #'npmjs-npm-read-config-key-value nil
                           nil multi-value))))))

(defun npmjs-parse-hint (str &optional cmd parsed-props)
  "Parse hint STR for CMD.
Return plist merged with PARSED-PROPS."
  (let* ((default-props
          (pcase str
            ("<workspace-name>"
             (list
              :multi-value 'repeat
              :class 'transient-option
              :reader 'npmjs-workspace-reader))
            ((or "<pkg>"
                 "<package-spec>"
                 "<@scope><package>"
                 "<@scope><pkg>"
                 "<package>"
                 "<folder>"
                 "<pkgname>"
                 "<@scope><name>@<version>"
                 "<pkg>@<version>"
                 "<@scope>/<name>"
                 "<@scope><pkg>@<version>"
                 "<@scope><pkg>@<version range>"
                 "<@scope><pkg>@<tag>")
             (pcase cmd
               ("install"
                "uninstall"
                (list
                 :multi-value 'rest))))))
         (merged (if default-props
                     (npmjs-plist-merge default-props
                                        parsed-props)
                   parsed-props))
         (reader (or (plist-get merged :reader)
                     (npmjs-get-hint-reader str cmd
                                            (plist-get merged
                                                       :multi-value)))))
    (when reader
      (setq merged (if merged (plist-put merged :reader reader)
                     (list :reader reader))))
    (when (and merged (not (plist-get merged :class)))
      (setq merged (plist-put merged :class 'transient-option)))
    merged))

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
                        (car (last (split-string (car args) "-" t))))
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

(defun npmjs-hint-p (str)
  "Check whether STR is a plain hint."
  (and (string-prefix-p "<" str)
       (string-suffix-p ">" str)
       (not (string-match-p "|" str))))

(defun npmjs-plist-merge (plist-a plist-b)
  "Add props from PLIST-B to PLIST-A."
  (let ((props (seq-copy plist-a)))
    (dotimes (idx (length plist-b))
      (when (eq (logand idx 1) 0)
        (let ((prop-name (nth idx plist-b)))
          (let ((val (plist-get plist-b prop-name)))
            (setq props (plist-put props prop-name val))))))
    props))

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

(defun npmjs-normalize-description (str)
  "Stip STR."
  (let ((res (replace-regexp-in-string "^[-]+" "" (string-trim str))))
    (if (string-empty-p res)
        str
      res)))

(defun npmjs-extract-all-hints (str)
  "Extract all hints from STR."
  (let ((founds))
    (with-temp-buffer (insert str)
                      (while (re-search-backward "<\\([^>]+\\)>"
                                                 nil
                                                 t
                                                 1)
                        (push (match-string-no-properties 0) founds)))
    founds))

(defun npmjs-parse-argument (vect &optional cmd)
  "Parse argument VECT for CMD."
  (when (vectorp vect)
    (setq vect (append vect nil)))
  (when (stringp vect)
    (setq vect (list vect)))
  (let ((curr)
        (common-props (seq-filter #'symbolp vect))
        (result))
    (setq vect (seq-remove #'symbolp vect))
    (dolist (h (seq-filter #'npmjs-hint-p vect))
      (add-to-list 'npmjs-known-hints h))
    (while (setq curr (pop vect))
      (let* ((extra-props common-props)
             (value
              (when (and (car vect)
                         (not (or (string-prefix-p "--" (car vect))
                                  (when (npmjs-hint-p curr)
                                    (string-prefix-p "<" (car vect))))))
                (pop vect)))
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
        (when-let ((hint (or
                          (when value (npmjs-parse-hint value cmd extra-props))
                          (npmjs-parse-hint curr cmd extra-props))))
          (setq extra-props (npmjs-plist-merge extra-props hint)))
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
                                (seq-filter #'stringp
                                            (seq-take vect
                                                      (seq-position
                                                       vect
                                                       curr)))
                                "")))
                 (setq vect nil)
                 (append
                  (list
                   (npmjs-normalize-description
                    (or longarg curr))
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
                       (append `(,(npmjs-normalize-description arg)
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
                     `(,(npmjs-normalize-description arg)
                       :choices ',choices
                       :class 'transient-switches
                       :argument-format ,(concat arg "=" "%s")
                       :argument-regexp ,(regexp-opt choices)))))
                ((guard
                  (and argument
                       value
                       (string-match-p "|" value)
                       extra-props))
                 (append `(,(npmjs-normalize-description argument)
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
                   `(,(npmjs-normalize-description argument)
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
                       (not (string-match-p "|" value))))
                 (let* ((long (or argument (npmjs-ensure-option-ending curr)))
                        (description (npmjs-normalize-description long))
                        (props (append
                                (list
                                 description
                                 (if shortarg
                                     (list shortarg long)
                                   long))
                                (npmjs-plist-merge
                                 (list
                                  :class 'transient-option
                                  :prompt
                                  (when value (format "%s: " value)))
                                 extra-props))))
                   props))
                ((guard (and (stringp curr)
                             (not value)
                             (not (npmjs-short-long-option-p curr))
                             (string-match-p "|" curr)))
                 (let* ((extra-choices (seq-filter (apply-partially
                                                    #'string-prefix-p "|")
                                                   vect))
                        (choices))
                   (when extra-choices
                     (setq vect (seq-difference vect extra-choices))
                     (setq curr (string-join (append (list curr) extra-choices)
                                             "")))
                   (setq choices (npmjs-split-argument curr))
                   (let* ((hints (seq-filter (apply-partially
                                              #'string-prefix-p "<")
                                             choices)))
                     (setq choices (seq-difference choices hints))
                     (when hints
                       (setq hints (mapcar #'npmjs-extract-all-hints hints))
                       (setq vect (append vect (reverse
                                                (flatten-list hints))))))
                   (when choices
                     `(:choices ',choices
                                :class 'transient-switches
                                :argument-format "%s"
                                :argument-regexp ,(regexp-opt choices)))))
                ((guard (and argument
                             (not value)
                             (not extra-props)))
                 (list (npmjs-normalize-description argument)
                       (if shortarg
                           (list shortarg argument)
                         argument)))
                ((guard (and (stringp curr)
                             (not value)
                             (not (string-prefix-p "-" curr))))
                 (let ((props (npmjs-plist-merge (list :class 'transient-option)
                                                 extra-props)))
                   (append (list
                            curr
                            (npmjs-ensure-option-ending
                             curr))
                           props)))
                ((guard (and (stringp curr)
                             (not value)))
                 (list (npmjs-normalize-description curr) curr))
                ((guard (npmjs-parse-no-argument-p curr))
                 (let* ((choices (split-string curr "|" t))
                        (filtered (seq-remove (apply-partially
                                               #'string-prefix-p
                                               "--no-")
                                              choices))
                        (nodubs (seq-uniq filtered)))
                   (cond ((and value
                               (> (length filtered)
                                  (length nodubs))
                               (not (string-match-p value "<")))
                          (let ((final-choices (append
                                                (seq-filter (apply-partially
                                                             #'string-prefix-p
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

;;;###autoload
(defun npmjs-show-all-args ()
  "Print all known args."
  (interactive)
  (npmjs-pp (mapcar (lambda (it)
                      (cons it
                            (list (npmjs-parse-help--vector it "" t))))
                    (seq-copy npmjs-all-args))))

(defun npmjs-make-infix-switch-name (choices)
  "Make description from CHOICES."
  (or (car (car (npmjs-group-with (lambda (it)
                                    (car (split-string it "[-=]" nil)))
                                  (mapcar #'npmjs-normalize-description
                                          (seq-remove
                                           (apply-partially #'string-match-p
                                                            "^-[a-z]\\|--no-")
                                           choices)))))
      (npmjs-normalize-description (car choices))))

(defun npmjs-nested-args-p (args)
  "Return non nil if ARGS is a nested list."
  (and args
       (car args)
       (car-safe (car args))))

(defun npmjs-maybe-eval (cmd inhibit-eval args)
  "Eval infix for CMD from ARGS if INHIBIT-EVAL is non nil."
  (cond ((or (npmjs-get-keyword-value :argument-format args)
             (and (npmjs-get-keyword-value :choices args)
                  (npmjs-get-keyword-value :multi-value args)))
         (let* ((choices (npmjs-unquote (npmjs-get-keyword-value
                                         :choices args)))
                (basename (npmjs-make-infix-switch-name choices)))
           (npmjs-eval-infix (npmjs-make-symbol
                              "npmjs" cmd
                              basename)
                             (if (stringp (car args))
                                 args
                               (append (list basename) args))
                             inhibit-eval)))
        (t args)))

(defun npmjs-parse-help--vector (vect &optional cmd inhibit-eval)
  "Parse VECT with CMD.
CMD is used for evaluated infix.
If INHIBIT-EVAL is non nil, don't eval infixes."
  (add-to-list 'npmjs-all-args vect)
  (add-to-list 'npmjs-all-args-alist (cons cmd vect))
  (or
   (let ((args (npmjs-parse-argument vect cmd))
         (result))
     (setq result (if (npmjs-nested-args-p args)
                      (mapcar (apply-partially
                               #'npmjs-maybe-eval cmd inhibit-eval)
                              args)
                    (npmjs-maybe-eval cmd inhibit-eval args)))
     result)))

(defun npmjs-specifier-at-point ()
  "Return and skip hint at point."
  (when-let* ((start
               (when (looking-at "<")
                 (point)))
              (end (progn
                     (skip-chars-forward "^>")
                     (when (looking-at ">")
                       (forward-char 1)
                       (point)))))
    (skip-chars-forward "@/")
    (while (looking-at "<")
      (skip-chars-forward "^>")
      (when (looking-at ">")
        (forward-char 1)
        (skip-chars-forward "@/")
        (setq end (point))))
    (buffer-substring-no-properties start end)))

(defun npmjs-tokenize (&optional stop-char)
  "Tokenize current buffer with help output.
STOP-CHAR is used for recursive purposes."
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
           (when-let ((str (progn (forward-char -1)
                                  (npmjs-specifier-at-point))))
             (setq node
                   str)
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
           (let ((item (npmjs-tokenize "]")))
             (while
                 (when-let* ((specifier (npmjs-specifier-at-point)))
                   (setq item (nconc item (list specifier)))))
             (setq node
                   (apply #'vector item))))
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
  "Write all npm help outputs to the fixtures in current directory.
If the directory fixtures doesn't exists, create it.

This function is for debug purposes."
  (let ((versions (npmjs-nvm--installed-versions)))
    (dolist (cell versions)
      (npmjs-nvm-with-env-vars
        (car cell)
        (let ((npm-version (string-trim (shell-command-to-string "npm -v")))
              (npm-output (shell-command-to-string "npm -l")))
          (unless
              (file-exists-p (expand-file-name "fixtures" default-directory))
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

(defun npmjs--get-all-npm-versions-descriptions-alist ()
  "Parse all installed npm versions.
Results is stored in `npmjs-all-columns-alist' and
`npmjs-all-descriptions-alist'.

This function is for debug purposes."
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
          (seq-sort-by #'car 'version<
                       (nreverse
                        npmjs-all-columns-alist)))
    (setq npmjs-all-descriptions-alist (nreverse results))))

;;;###autoload
(defun npmjs-print-all-descriptions (&optional force)
  "Print `npmjs-all-descriptions-alist'.
If the value is nil or FORCE is non nil,
invoke call `npmjs--get-all-npm-versions-descriptions-alist'.

This function is for debug purposes."
  (interactive "P")
  (unless (and npmjs-all-descriptions-alist
               (not force))
    (npmjs--get-all-npm-versions-descriptions-alist))
  (npmjs-pp npmjs-all-descriptions-alist))


(defun npmjs-upcased-p (str)
  "Return non nil is string STR begins with letters in uppercase."
  (let ((case-fold-search nil))
    (string-match-p "^[A-Z]" str)))

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
  "Flatten vector ITEM."
  (cond ((vectorp item)
         (seq-reduce (lambda (acc it)
                       (setq acc
                             (pcase it
                               ((pred vectorp)
                                (vconcat acc
                                         (npmjs-flatten-vectors
                                          it)))
                               (_ (vconcat acc (vector it))))))
                     item
                     []))))

(defun npmjs-multi-extract-vector (vect)
  "If vector contains VECT with the same first element, return those element.
If not, just return VECT."
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
          (setq left (apply #'vector
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
                          "Run \"npm help"
                          it)))))
                lines)
   "\n"))

(defun npmjs-map-descriptions-lines (alist &optional inhibit-eval)
  "Map ALIST of commands and arguments.
If INHIBIT-EVAL is non nil, don't eval infixes."
  (mapcar (lambda (it)
            (npmjs-map-command-cell-lines it inhibit-eval))
          alist))

(defun npmjs-single-value-to-hint (str)
  "Replace single value in argument STR with hint.
For example, --registry=url to --registry=<url>."
  (with-temp-buffer
    (insert str)
    (while (re-search-backward
            "--\\([a-z-0-9@]+\\)=\\([a-z-0-9@]+\\)" nil t 1)
      (let ((value (match-string-no-properties 2)))
        (replace-match (concat "<" value ">") nil nil nil 2)))
    (buffer-string)))

(defun npmjs-prenormalize-line (curr)
  "Cleanup string CURR."
  (setq curr (replace-regexp-in-string "[(]same as[ ][^)]+)" "" curr))
  (setq curr (replace-regexp-in-string
              "(\\(with no args, \\)?in package dir)" "" curr))
  (setq curr (replace-regexp-in-string "[(]with no args,[^)]+)" "" curr))
  (setq curr (replace-regexp-in-string "[(]See [^)]+)" "" curr))
  (setq curr (replace-regexp-in-string "[\s]?|[\s]?" "|" curr))
  (setq curr (replace-regexp-in-string "\\[pkg\\]" "[<pkg>]" curr))
  (setq curr (replace-regexp-in-string "<pkg>\\[@<version>\\]"
                                       "[<pkg>@<version>]"
                                       curr))
  (when (string-match-p
         "\\[?\\(<[a-z][^>]+>\\)[ ]\\[?\\(<[a-z][^>]+>\\)\\]?\\]?+" curr)
    (setq curr (with-temp-buffer (insert curr)
                                 (goto-char (point-min))
                                 (when (re-search-forward
                                        "\\[?\\(<[a-z][^>]+>\\)[ ]\\[?\\(<[a-z][^>]+>\\)\\]?\\]?+"
                                        nil t 1)
                                   (let ((a (match-string-no-properties 1))
                                         (b (match-string-no-properties 2)))
                                     (replace-match (concat a " " b))))
                                 (buffer-string))))
  (npmjs-single-value-to-hint curr))

(defun npmjs-parse-normalized-vectors (cmd inhibit-eval vectors)
  "Map normalized list of VECTORS to arguments for CMD.
If INHIBIT-EVAL is nil, don't eval infixes."
  (seq-reduce
   (lambda (acc it)
     (when-let ((res (npmjs-parse-help--vector
                      it
                      cmd
                      inhibit-eval)))
       (if (npmjs-nested-args-p res)
           (setq acc (append acc res))
         (setq acc
               (append acc (list res)))))
     acc)
   vectors
   '()))

(defun npmjs-parse-subcommands-options (rawoptions)
  "Parse subcommand line options RAWOPTIONS."
  (nreverse
   (seq-reduce
    (lambda (acc it)
      (setq acc
            (pcase it
              ((pred vectorp)
               (let ((result (npmjs-normalize-vectors it))
                     (prev (car acc)))
                 (if-let* ((prev-el (npmjs-nth 0 prev))
                           (curr-el (npmjs-nth 0 result)))
                     (cond ((and (stringp prev-el)
                                 (stringp curr-el)
                                 (string-prefix-p prev-el curr-el))
                            (if (> (length prev)
                                   (length result))
                                acc
                              (pop acc)
                              (push result acc)))
                           (t (push result acc)))
                   (push result
                         acc))))
              ("'"
               (let ((prev (pop acc)))
                 (cond ((and (= (length prev) 1)
                             (stringp (npmjs-nth 0 prev))
                             (string-prefix-p "-" (npmjs-nth 0 prev)))
                        (push (apply #'vector (list (npmjs-nth 0 prev) "'"))
                              acc)))))
              ((pred stringp)
               (let ((prev (car acc)))
                 (cond ((and
                         (= 1
                            (length
                             prev))
                         (stringp (npmjs-nth 0 prev))
                         (string-suffix-p "=" (npmjs-nth 0 prev)))
                        (setq prev (pop acc))
                        (push (apply #'vector
                                     (append prev (list it)))
                              acc))
                       ((and (equal (car (last (append prev nil))) "'"))
                        (setq prev (pop acc))
                        (setq prev (append prev nil))
                        (setq prev (nbutlast prev 1))
                        (setq prev (append prev (list (format "'%s'" it))))
                        (push (apply #'vector prev) acc))
                       (t (push (vector it) acc)))))
              ((pred symbolp)
               (if-let ((prev (pop acc)))
                   (push (apply #'vector (append prev (list it))) acc)
                 acc))
              (_ (message "unknown type %s: " it)))))
    rawoptions '())))


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
      (setq curr (npmjs-prenormalize-line curr))
      (let ((parsed (npmjs-read-line curr)))
        (pcase-let* ((words
                      (seq-take-while (lambda (it)
                                        (and (stringp it)
                                             (not (string-prefix-p  "<" it))
                                             (not (string-prefix-p "(" it))
                                             (not (string-prefix-p "-" it))
                                             (not (string-match-p "=" it))))
                                      parsed)))
          (pcase (car words)
            ("common"
             (let ((options (seq-drop parsed (length words))))
               (setq common-options
                     (if common-options
                         (append common-options options)
                       options))))
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
            ((guard (and (car words)
                         (npmjs-upcased-p (car words))))
             (push curr descriptions))
            ((or "aliases:"
                 "alias:")
             (let ((strs (mapcar
                          (apply-partially #'replace-regexp-in-string "," "")
                          words)))
               (setq aliases (if aliases (nconc aliases strs)
                               strs))))
            ("npm"
             (when (and (cadr words)
                        (string= cmd (cadr words)))
               (let ((subcommand-name (string-join (cdr words) " "))
                     (new-options
                      (npmjs-parse-subcommands-options
                       (seq-drop
                        parsed
                        (length
                         words)))))
                 (when-let ((pos
                             (seq-position new-options
                                           (car (member ["--"]
                                                        new-options)))))
                   (setq new-options (seq-take new-options (1+ pos))))
                 (setq new-options
                       (npmjs-parse-normalized-vectors
                        subcommand-name
                        inhibit-eval new-options))
                 (if-let ((subcommand-cell (assoc-string subcommand-name
                                                         subcommands)))
                     (setcdr subcommand-cell
                             (append (cdr subcommand-cell) new-options))
                   (push (cons subcommand-name new-options) subcommands)))))
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
                                   :key
                                   (car (last (split-string cmd " " t)))
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

(defun npmjs-get-description-spec (command)
  "Pase COMMAND description from help output."
  (assoc-string command
                (npmjs-nvm-with-current-node-version
                 (npmjs-parse-help-with-output
                     (shell-command-to-string "npm -l")
                   (npmjs-parse-columns-to-alist)))))

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

(defun npmjs-get-and-forward-command-description (column)
  "Jump to next empty line that start with COLUMN whitespace."
  (let ((initial-pos (point))
        (end (line-end-position)))
    (when (and (> (- initial-pos column)
                  (point-min))
               (let ((str (buffer-substring-no-properties (- initial-pos column)
                                                          initial-pos)))
                 (string-empty-p (string-trim str))))
      (while
          (when (and (= 0 (forward-line)))
            (let* ((beg (point))
                   (colend (+ beg (1+ column))))
              (when (> (point-max) colend)
                (goto-char colend)
                (and (string-empty-p (string-trim
                                      (buffer-substring-no-properties
                                       beg
                                       colend)))
                     (looking-at "[\s\t]")))))
        (setq end (line-end-position)))
      (when end
        (when-let* ((description
                     (buffer-substring-no-properties initial-pos end))
                    (lines
                     (seq-remove #'string-empty-p
                                 (mapcar #'string-trim
                                         (split-string
                                          description "[\n\r\f]+" t))))
                    (first-line-words (split-string (pop lines) nil
                                                    t))
                    (cmd (pop first-line-words))
                    (first-line (string-join first-line-words " ")))
          (setq lines (if (string-empty-p (string-trim first-line))
                          lines
                        (push first-line lines)))
          (cons cmd lines))))))

(defun npmjs-parse-columns-to-alist ()
  "Search and parse COMMAND flags using REGEXP and NUM."
  (let ((rows)
        (regexp "^\\([\s\t]+\\)\\([a-z0-9]+[a-z0-9_-]+\\)[:]?[\s]")
        (description)
        (column))
    (goto-char (point-min))
    (when (re-search-forward
           regexp
           nil t 1)
      (goto-char (match-beginning 2))
      (setq column (current-column))
      (while (setq description
                   (npmjs-get-and-forward-command-description column))
        (push description rows))
      (nreverse rows))))

(defun npmjs-eval-symb (cmd options &optional description no-eval)
  "Eval prefix with OPTIONS, CMD and SUBCOMMANDS.
NO-EVAL is used for debug purposes."
  (let ((name (npmjs-make-symbol "npm" cmd)))
    (put name 'npm-command cmd)
    (put name 'npm-description description)
    (put name 'man-page (concat "npm-" (car (split-string cmd " " t))))
    (if no-eval
        (npmjs-message "Evaluating %s as %s" cmd name)
      (npmjs-eval-prefix name (if description
                                  `([:description ,description ,@options])
                                `([,@options]))))
    name))

;;;###autoload
(defun npmjs-eval-prefix (name body)
  "Eval and call transient prefix with NAME and BODY."
  (interactive)
  (eval `(progn (transient-define-prefix ,name ()
                  :value (lambda ()
                           (unless (npmjs-get-project-root)
                             (list "--global")))
                  :show-help (lambda (prefix)
                               (npmjs-show-manual (get (oref prefix command)
                                                       'man-page)))
                  ,@body)
                ',name)
        t))

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

(defvar npmjs-map-replace-commands '(("run-script" . npmjs-run-script)
                                     ("install" . npmjs-install))
  "Alist of commands and predefined commands to use instead.")

(defvar npmjs-extra-arguments
  `(("install" ("global" ("-g" "--global")))
    ("update" ("global" ("-g" "--global")))
    ("uninstall" ("global" ("-g" "--global")))
    ("query" ("global" ("-g" "--global")))
    ("prefix" ("global" ("-g" "--global")))
    ("ll" ("global" ("-g" "--global")))
    ("ls" ("global" ("-g" "--global")))))

(defvar npmjs-options-suffixes
  '(("RET" "Run" npmjs-done)
    ("C-c C-a" "Show arguments" npmjs-show-args)))

(defun npmjs-map-commands (commands)
  "Recoursively map and eval COMMANDS.
COMMANDS is a nested list of property lists with such props:
:cmd - a string, the command name e.g. \"install\", \"access list\".
:key - a string, which will be used for generating key.
:options - alist of descriptions (string) and its arguments.
:subcommands - same as COMMANDS."
  (cond ((and (listp (car commands))
              (keywordp (car-safe (car commands))))
         (npmjs-key-builder-generate-shortcuts
          (seq-remove
           (lambda (it)
             (member (plist-get it :cmd)
                     npmjs-omit-commmands))
           commands)
          (lambda (it)
            (or (plist-get it :key)
                (plist-get it :cmd)))
          (lambda (key value)
            (npmjs-map-commands (plist-put value :key key)))
          nil
          2))
        ((and (keywordp (car-safe commands))
              (plist-get commands :subcommands))
         (setq commands
               (plist-put commands :subcommands
                          (npmjs-key-builder-generate-shortcuts
                           (plist-get commands :subcommands)
                           (lambda (it)
                             (let ((key (or (plist-get it :key)
                                            (plist-get it :cmd))))
                               key))
                           (lambda (key value)
                             (setq value (plist-put value :key key))
                             (npmjs-map-commands
                              value))
                           nil)))
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
           (funcall #'fset name
                    `(lambda ()
                       (interactive)
                       (npmjs-confirm-and-run
                        "npm"
                        ,cmd)))
           (if-let ((props (cdr (assoc-string name npmjs-commands-props))))
               (append (list key cmd name) props)
             (list key cmd name))))
        (t (npmjs-message "unmatched commands %s" commands)
           commands)))

(defun npmjs-get-option-shortcut-word (option)
  "Return OPTION description to use in shortcut generation."
  (let ((description (car option)))
    (pcase description
      ((pred stringp)
       description)
      ((pred symbolp)
       (replace-regexp-in-string "^npmjs-" "" (symbol-name (car option)))))))

(defun npmjs-add-options-shortcuts (options &optional used-keys)
  "Add shortcuts to OPTIONS.
USED-KEYS is a list of keys that shouldn't be used."
  (npmjs-key-builder-generate-shortcuts
   options
   #'npmjs-get-option-shortcut-word
   (lambda (k v)
     (pcase (car v)
       ("--" v)
       ("." v)
       ("@" v)
       (_ (push k v))))
   used-keys))

(transient-define-argument npmjs-install-pkg-argument ()
  "Argument for packages without version and tags."
  :class 'transient-option
  :multi-value 'repeat
  :argument "<pkg>"
  :prompt "Package name:"
  :reader (npmjs-make-reader #'npmjs-read-new-dependency nil nil
                             t))

(transient-define-argument npmjs-install-pkg-tag-argument ()
  "Argument for packages with tag."
  :class 'transient-option
  :multi-value 'repeat
  :argument "<pkg>@<tag>"
  :prompt "Package name:"
  :reader (npmjs-make-npm-package-reader-with-tag t))

(transient-define-argument npmjs-install-pkg-version-argument ()
  "Argument for packages with version and tags."
  :class 'transient-option
  :multi-value 'repeat
  :argument "<pkg>@<version>"
  :prompt "Package name:"
  :reader (npmjs-make-npm-package-reader-with-version t))

(transient-define-argument npmjs-install-pkg-tarbal-argument ()
  "Argument for installing tarbal file."
  :class 'transient-option
  :argument "<tarball>"
  :prompt "Tarball file:"
  :reader #'npmjs-read-tar-file)

(transient-define-argument npmjs-install-pkg-directory ()
  "Argument for installing tarbal file."
  :class 'transient-option
  :argument "<directory>"
  :prompt "Directory:"
  :reader #'transient-read-existing-directory)


;;;###autoload
(defun npmjs-install-self-globally ()
  "Install current project as global dependency."
  (interactive)
  (when-let ((proj (npmjs-get-project-root)))
    (npmjs-nvm-with-current-node-version
     (let ((default-directory (expand-file-name proj)))
       (npmjs-run-as-comint "npm install . --global")))))

;;;###autoload
(defun npmjs-install-project-dependencies ()
  "Install current project as global dependency."
  (interactive)
  (when-let ((proj (npmjs-get-project-root)))
    (npmjs-nvm-with-current-node-version
     (let ((default-directory (expand-file-name proj)))
       (npmjs-run-as-comint (concat
                             "npm install"
                             (npmjs-format-args
                              (remove "--global"
                                      (seq-filter
                                       (npmjs--and #'stringp (apply-partially
                                                              #'string-prefix-p
                                                              "-"))
                                       (npmjs-get-arguments))))))))))

;;;###autoload (autoload 'npmjs-install "npmjs.el" nil t)
(transient-define-prefix npmjs-install ()
  "Run arbitrary package scripts."
  :value (lambda ()
           (unless (npmjs-get-project-root)
             (list "--global")))
  :show-help (lambda (&rest _)
               (npmjs-show-manual "npm-install"))
  [:description
   "Install packages"
   :setup-children
   (lambda (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       transient--prefix)
      '(("p" "package" npmjs-install-pkg-argument)
        ("@t" "package@tag" npmjs-install-pkg-tag-argument)
        ("@v" "package@version" npmjs-install-pkg-version-argument)
        ("-d" "directory" npmjs-install-pkg-directory)
        ("-t" "Install tarball file" npmjs-install-pkg-tarbal-argument))))]
  ["Options"
   :setup-children
   (lambda (&rest _argsn)
     (npmjs-nvm-with-current-node-version
      (npmjs-setup-npm)
      (let* ((props (npmjs-map-command-cell-lines
                     (cons
                      "install"
                      (append (list "npm install")
                              (seq-remove
                               (apply-partially #'string-prefix-p "npm install")
                               (assoc-string
                                "install"
                                (cdr npmjs-current-descriptions-alist)))))))
             (options (append
                       (seq-remove
                        (npmjs--compose
                         (npmjs--cond
                          [stringp (apply-partially #'string= "--global")]
                          [listp
                           (apply-partially
                            #'seq-find
                            (npmjs--or
                             (apply-partially #'string=
                                              "--global")
                             (apply-partially #'string= "-")))])
                         (apply-partially #'npmjs-nth 2))
                        (npmjs-add-options-shortcuts
                         (plist-get props
                                    :options)
                         '("p" "g")))
                       (list '("g" "global" ("-g" "--global"))))))
        (mapcar
         (apply-partially #'transient-parse-suffix
                          transient--prefix)
         options))))]
  ["Actions"
   ("." npmjs-install-self-globally
    :description (lambda ()
                   (format
                    "current project (%s) as global package"
                    (or
                     (alist-get
                      'name
                      (npmjs-get-package-json-alist))
                     "no project")))
    :inapt-if-not (lambda ()
                    (alist-get 'name (npmjs-get-package-json-alist))))
   ("RET" "Run" npmjs-done)
   ("C-c C-a" "Show arguments" npmjs-show-args)]
  (interactive)
  (transient-setup #'npmjs-install))

(put 'npmjs-install 'npm-description "Install packages")
(put 'npmjs-install 'npm-command "install")

;;;###autoload
(defun npmjs-show-package-json-man-page ()
  "Show man page about package.json handling."
  (interactive)
  (npmjs-show-manual "package-json"))

;;;###autoload
(defun npmjs-show-scripts-man-page ()
  "Show man page about npm scripts."
  (interactive)
  (npmjs-show-manual "scripts"))

(defun npmjs-get-scripts-suffixes ()
  "Return group specification with scripts for current project.
It is a suffixes in the same forms as expected by `transient-define-prefix'."
  (let ((scripts (npmjs-stringify (npmjs-get-package-json-scripts)))
        (proj-name (npmjs-project-display-name))
        (dir (npmjs-get-project-default-directory)))
    (npmjs-key-builder-generate-shortcuts
     scripts
     #'car
     (lambda (key value)
       (let ((descr (cdr value))
             (script (car value))
             (name (npmjs-make-symbol "npm-run" proj-name (car value))))
         (funcall #'fset name
                  `(lambda ()
                     ,(format "Run script %s in %s." script dir)
                     (interactive)
                     (let ((default-directory ,dir))
                       (npmjs-confirm-and-run
                        "npm"
                        "run-script"
                        ,script
                        (npmjs-get-formatted-transient-args)))))
         (list key name :description `(lambda ()
                                        (concat
                                         ,script
                                         " "
                                         "("
                                         (propertize
                                          ,descr
                                          'face
                                          'transient-value)
                                         ")"))))))))


;;;###autoload (autoload 'npmjs-run-script "npmjs.el" nil t)
(transient-define-prefix npmjs-run-script ()
  "Run arbitrary package scripts."
  :show-help (lambda (&rest _)
               (npmjs-show-manual "npm-run-script"))
  [:description
   (lambda ()
     (if-let ((name (npmjs-project-display-name)))
         (format "Run script (%s)" name)
       "Run script"))
   :setup-children
   (lambda (&rest _args)
     (mapcar
      (apply-partially #'transient-parse-suffix transient--prefix)
      (npmjs-get-scripts-suffixes)))]
  ["Options"
   :setup-children
   (lambda (&rest _args)
     (npmjs-nvm-with-current-node-version
      (npmjs-setup-npm)
      (let* ((props
              (npmjs-map-command-cell-lines
               (assoc-string "run-script"
                             (cdr
                              npmjs-current-descriptions-alist))))
             (options (npmjs-add-options-shortcuts
                       (plist-get props :options)))
             (children (append options
                               npmjs-options-suffixes)))
        (mapcar
         (apply-partially #'transient-parse-suffix
                          transient--prefix)
         children))))]
  ["Help"
   ("-s" "scripts" npmjs-show-scripts-man-page)
   ("-p" "package.json" npmjs-show-package-json-man-page)]
  (interactive)
  (transient-setup #'npmjs-run-script))

(put 'npmjs-run-script 'npm-description "Install packages")
(put 'npmjs-run-script 'npm-command "run-script")

(defvar npmjs-evaluated-commands (make-hash-table :test 'equal)
  "Last executed command lines, per project.")

(defun npmjs-pp (&rest objects)
  "Print OBJECTS in debug buffer."
  (let ((buff (get-buffer-create (format "*npmjs-debug-%s*"
                                         npmjs--current-node-version))))
    (with-current-buffer buff
      (erase-buffer)
      (let ((emacs-lisp-mode-hook nil))
        (dolist (obj objects)
          (princ "\n" buff)
          (let (print-level print-length)
            (pp obj buff)))
        (emacs-lisp-mode)
        (when
            (require 'prettier-elisp nil t)
          (when (fboundp 'prettier-elisp-buffer)
            (prettier-elisp-buffer)))
        (font-lock-ensure)))
    (unless (get-buffer-window buff)
      (pop-to-buffer buff))))

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


(defun npmjs-setup-npm ()
  "Setup, eval and call npm transient menu."
  (let ((npm-version (string-trim (shell-command-to-string
                                   "npm -v"))))
    (if-let ((descriptions (cdr (assoc-string npm-version
                                              npmjs-descriptions-alist))))
        (setq npmjs-current-descriptions-alist descriptions)
      (setq npmjs-current-descriptions-alist (npmjs-get-prefix-spec))
      (setq npmjs-descriptions-alist
            (push
             (cons npm-version
                   npmjs-current-descriptions-alist)
             npmjs-descriptions-alist)))
    npm-version))

(defmacro npmjs-csetq (variable value)
  "Set the value of VARIABLE to VALUE using the appropriate set function."
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

;;;###autoload
(defun npmjs-toggle-prefix-evaluation ()
  "Toggle the value of variable `npmjs-inhibit-prefix-cache'."
  (interactive)
  (npmjs-csetq npmjs-inhibit-prefix-cache (not npmjs-inhibit-prefix-cache))
  (npmjs-message "%s %s" 'npmjs-inhibit-prefix-cache
                 npmjs-inhibit-prefix-cache))

;;;###autoload
(defun npmjs ()
  "Setup, eval and call npm transient menu."
  (interactive)
  (npmjs-nvm-with-current-node-version
   (let* ((npm-version (npmjs-setup-npm))
          (command
           (or (and (not npmjs-inhibit-prefix-cache)
                    (gethash npm-version npmjs-evaluated-commands))
               (let* ((title (car npmjs-current-descriptions-alist))
                      (description (concat "NPM: "
                                           npm-version
                                           " "
                                           title))
                      (spec
                       (npmjs-map-descriptions-lines
                        (cdr npmjs-current-descriptions-alist)))
                      (mapped (npmjs-map-commands spec))
                      (groupped (npmjs-group-vectors mapped))
                      (prefix-symb
                       (let ((sym (npmjs-make-symbol "npmjs-"
                                                     npm-version)))
                         (put sym 'npm-command "npm")
                         (put sym 'man-page "npm")
                         (npmjs-eval-prefix
                          sym
                          `([:description
                             ,description
                             ,@(append
                                groupped
                                (list
                                 (apply
                                  #'vector
                                  '(("-o"
                                     "nvm"
                                     npmjs-nvm
                                     :inapt-if-not
                                     npmjs-nvm-path)
                                    ("-h" "Show npm help"
                                     npmjs-show-help)
                                    ("-m" "Show npm manual"
                                     npmjs-show-manual)))))]))
                         sym)))
                 (puthash npm-version prefix-symb
                          npmjs-evaluated-commands)
                 prefix-symb))))
     (transient-setup command))))

;;;###autoload (autoload 'npmjs-nvm "npmjs.el" nil t)
(transient-define-prefix npmjs-nvm ()
  "Menu for Node Version Manager (nvm) commands."
  ["Node Version Manager (nvm)"
   ("I" "Install new node version"
    npmjs-nvm-install-node-version :inapt-if-not npmjs-nvm-path)
   ("u" "Use other node version"
    npmjs-nvm-use-other-node-version :inapt-if-not npmjs-nvm-path)
   ("U" "Use other node version (globally)"
    npmjs-nvm-use-switch-system-node-version :inapt-if-not npmjs-nvm-path)
   ("f" "Jump to installed node"
    npmjs-nvm-jump-to-installed-node :inapt-if-not npmjs-nvm-path)]
  [("N" "install nvm" npmjs-install-nvm)])

;;;###autoload
(defun npmjs-debug ()
  "Display parsed results in debug buffer."
  (interactive)
  (npmjs-nvm-with-current-node-version
    (let* ((spec (npmjs-get-prefix-spec))
           (description (car spec))
           (alist (cdr spec))
           (results
            (pcase (car
                    (read-multiple-choice
                     "Print"
                     '((?d "Alist of descriptions" )
                       (?p "Specification from descriptions")
                       (?c "Mapped commands")
                       (?C "Commands and specification")
                       (?! "All"))))
              (?d alist)
              (?p (npmjs-map-descriptions-lines alist t))
              (?c (npmjs-map-commands (npmjs-map-descriptions-lines alist t)))
              (?C
               (let ((plists (npmjs-map-descriptions-lines alist t)))
                 (list plists (npmjs-map-commands plists))))
              (?!
               (let* ((plists (npmjs-map-descriptions-lines alist t))
                      (cmds (npmjs-map-commands plists)))
                 (list plists cmds alist))))))
      (apply #'npmjs-pp (append (list description) results)))))

(provide 'npmjs)
;;; npmjs.el ends here
;; Local Variables:
;; checkdoc-verb-check-experimental-flag: nil
;; End:
