;;; npmjs.el --- Command dispatcher for npm package manager -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>\

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/npmjs
;; Version: 0.2.0.50-git
;; Keywords: tools
;; Package-Requires: ((emacs "29.1") (transient "0.4.3"))
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
  "The directory path where Node Version Manager (NVM) is installed.

Specifies the directory where Node Version Manager (NVM) is installed.

If the environment variable NVM_DIR is set, its value is used. Otherwise, the
default value is the `.nvm' directory in the user's home directory.

This should be a valid directory path.

To change the value, customize it through the customization interface or set it
in the Emacs configuration file using `setq'.

For example:

```elisp \\=(setq npmjs-nvm-dir \"/path/to/custom/nvm\") ```

Ensure that the specified directory exists and contains the NVM installation for
proper integration with npm-related functionality.

Belongs to the `npmjs' customization group.

See https://github.com/nvm-sh/nvm."
  :group 'npmjs
  :type 'directory)



(defcustom npmjs-inhibit-prefix-cache nil
  "Set to non-nil to disable caching of package prefix requests.

Determines whether to bypass the local cache when fetching package information
from the npm registry.

When non-nil, each request to the npm registry will ignore the locally cached
data and retrieve fresh information. This can be useful when working with
frequently updated packages or when debugging network-related issues.

To enable this feature, set the value to t. To rely on the local cache for
improved performance, leave the value as nil.

This is a boolean variable. Toggle the caching behavior by setting the variable
accordingly."
  :group 'npmjs
  :type 'boolean)

(defcustom npmjs-nvm-download-url "https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh"
  "The URL to download the nvm installation script.

Specifies the URL from which the Node Version Manager (nvm) install script
should be downloaded.

This should be a STRING representing a valid URL pointing to the nvm install
script.

Changing this value is useful for directing the download process to a different
version of nvm or to a forked repository.

Ensure that the provided URL is accessible and points to a legitimate nvm
install script.

The default value points to the official nvm install script for version v0.39.3.
To apply changes to this variable, use the `customize-set-variable' function or
set it directly in an Emacs Lisp file."
  :type 'string
  :group 'npmjs)

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
  "A list of npm commands to exclude from the npmjs interface.

A list of strings representing npm commands to omit from execution.

Each element in the list should be a string that matches the name of an npm
command that is not desired to be run.

Commands listed here will be excluded from certain operations when interacting
with npm through Emacs tooling.

To modify this list, use the customization interface or set the variable
directly in Emacs Lisp code with `setq'.

For example, to omit the \"install\" and \"update\" commands, set the variable
to \\='(\"install\" \"update\").

Changes to this variable will take effect the next time npm-related functions
are invoked."
  :group 'npmjs
  :type '(repeat string))

(defcustom npmjs-compile-command 'npmjs
  "Set the command to compile JavaScript projects using npmjs.

Specifies the command to be used for compiling JavaScript projects managed by
npm.

The value should be a symbol referring to a function that will be called to
perform the compilation process.

To change the compile command, set this variable to the desired function symbol
using `setq' or customize the NPMJS group.

The function bound to this symbol should take no arguments and will be executed
in the context of the project's root directory."
  :group 'npmjs
  :type 'function)

(defcustom npmjs-repeat-compile-command 'npmjs-repeat
  "Set the command to repeat compilation in the npmjs package.

Specifies the function to be called repeatedly when compiling a project using
npm.

The value should be a symbol referring to a function that takes no arguments and
is responsible for running the desired compile command.

To use, set this variable to the function that invokes the npm build process for
the project.

For example, if there is a function named `my-npm-compile', assign it like so:

\\=(setq npmjs-repeat-compile-command \\='my-npm-compile)

After setting, the specified function will be executed whenever the repeat
compile command is triggered within the npmjs package context."
  :group 'npmjs
  :type 'function)

(defcustom npmjs-started-hook nil
  "Functions to run after the npmjs process has started.

A hook that gets run after npmjs operations are initiated.

Hooks are lists of functions to be called at specific times.

Functions added to this hook can be used to perform additional actions when
npmjs processes start.

To add a function to this hook, use `add-hook'.

For example: \\='(add-hook \\='npmjs-started-hook \\=#'my-custom-function)`.

Ensure that functions intended for this hook do not take any arguments and do
not rely on the return value."
  :group 'npmjs
  :type 'hook)

(defcustom npmjs-finished-hook nil
  "Functions to run after npmjs operations complete.

A hook that gets run after npmjs operations are completed.

Hooks are lists of functions to be called at specific times. When a npmjs
operation finishes, each function in this hook is called with no arguments.

To add a function to this hook, use `add-hook'. For example:

\\=(add-hook \\='npmjs-finished-hook \\='my-npmjs-finished-function)

Where `my-npmjs-finished-function' is the name of the function to be called.

Functions can be removed from the hook using `remove-hook'.

This hook is useful for performing cleanup, notifications, or other tasks that
should occur after npmjs operations."
  :group 'npmjs
  :type 'hook)

(defcustom npmjs-setup-hook nil
  "Functions to run after setting up the npmjs environment.

A list of functions to be called after setting up the npmjs environment.

Each function is called with no arguments and is useful for customizing the
behavior of npmjs-related commands and interactions.

To add a function to this hook, use `add-hook'. For example:

\\=(add-hook \\='npmjs-setup-hook \\='my-npmjs-setup-function)

Where `my-npmjs-setup-function' is the name of the function to be added.

This hook is typically used for tasks such as configuring npmjs settings,
initializing npmjs projects, or integrating with other tools and packages."
  :group 'npmjs
  :type 'hook)

(defcustom npmjs-message-function 'message
  "The function to use for displaying messages in the npmjs package.

Specifies the function to use for displaying messages from npmjs operations.

The value should be a function that accepts the same arguments as MESSAGE, or
nil to suppress messages.

To use a custom function, set the value to a symbol that names the function.

For example, to use a function named `my-npmjs-message-function', set the value
like this:

```elisp \\=(setq npmjs-message-function \\='my-npmjs-message-function) ```

To suppress all messages, set the value to nil:

```elisp \\=(setq npmjs-message-function nil) ```

The default value is MESSAGE, which displays messages in the echo area."
  :type '(choice
          (const :tag "None" nil)
          (function :tag "Function"))
  :group 'npmjs)

(defcustom npmjs-use-comint-in-scripts t
  "Whether to enable running package.json scripts in comint mode.

When non-nil, scripts are run in an interactive comint buffer, allowing for
input and interactive command execution.

If set to nil, scripts are executed using the `compile' command, which is
non-interactive. This option can be toggled to switch between interactive and
non-interactive script execution modes.

The variable is a boolean value.

To change the setting, customize the variable accordingly."
  :type '(boolean)
  :group 'npmjs)

(defvar-local npmjs--current-command nil)
(defvar-local npmjs--current-node-version nil)
(defvar-local npmjs-current-descriptions-alist nil)

(defvar npmjs-descriptions-alist nil)

;; common utilities
(defun npmjs-message (string &rest arguments)
  "Display a formatted message prefixed with \"npmjs: \".

Argument STRING is the format string for the message.

Remaining ARGUMENTS arguments are objects substituted into STRING according to
the format specifications."
  (when npmjs-message-function
    (funcall npmjs-message-function
             (concat "npmjs: " (apply #'format string arguments)))))

(defun npmjs--plist-remove-nils (plist)
  "Remove nil values from a property list.

Argument PLIST is a property list where each even element is a key and the
following odd element is its value."
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
  "Remove `function' symbols from list start.

Argument EXP is an expression to be unquoted."
  (declare (pure t)
           (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(eval-and-compile
  (defun npmjs--expand (init-fn)
    "Expand the given macro and return the expanded form.
Argument INIT-FN is the macro to be expanded."
    (setq init-fn (macroexpand init-fn))
    (if (symbolp init-fn)
        `(#',init-fn)
      `(,init-fn))))

(defmacro npmjs--compose (&rest functions)
  "Compose FUNCTIONS into a single callable chain.

Remaining arguments FUNCTIONS are Lisp functions to be composed."
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

(defmacro npmjs--const (value)
  "Define a constant-returning function.

Argument VALUE is the value to be returned by the generated function."
  (declare (pure t)
           (side-effect-free error-free))
  (let ((arg (make-symbol "_")))
    `(lambda (&rest ,arg) ,value)))

(defmacro npmjs--cond (&rest pairs)
  "Transform conditions into a lambda function.

Remaining arguments PAIRS are lists where each list contains a condition and a
result form."
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
  "Combine FUNCTIONS with logical OR, returning first truthy result.

Remaining arguments FUNCTIONS are Lisp expressions or symbols representing
FUNCTIONS to be called with a single argument."
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
  "Combine FUNCTIONS with logical AND on input `it'.

Remaining arguments FUNCTIONS are Lisp expressions or symbols representing
FUNCTIONS to be called with a single argument `it'."
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
  "Create a reversed partial function with fixed arguments.

Argument FN is a function or a symbol referring to a function.

Remaining arguments ARGS are the arguments to be appended to the call to FN when
the resulting lambda is invoked."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list (if (symbolp fn)
                     `(apply #',fn (append pre-args (list ,@args)))
                   `(apply ,fn (append pre-args (list ,@args))))))))

(defmacro npmjs--converge (combine-fn &rest functions)
  "Apply a combination function to results of multiple functions.

Argument COMBINE-FN is a function that combines the results of other functions.

Remaining arguments FUNCTIONS are functions whose results are combined by
COMBINE-FN. If the first argument in FUNCTIONS is a vector, it is converted to a
list before being processed."
  `(lambda (&rest args)
     (apply
      ,@(npmjs--expand combine-fn)
      (list
       ,@(mapcar (lambda (v)
                   `(apply ,@(npmjs--expand v) args))
                 (if (vectorp (car functions))
                     (append (car functions) nil)
                   functions))))))

(defun npmjs-set-env-vars (version &optional globaly)
  "Set Node.js environment variables based on version.

Argument VERSION is a string representing the Node.js version to set environment
variables for.

Optional argument GLOBALY is a boolean flag indicating whether to set the
environment variables globally or locally. If non-nil, variables are set
globally; otherwise, they are set locally."
  (let ((vars
         (npmjs-nvm-get-env-vars version)))
    (pcase-dolist (`(,var ,value) vars)
      (setenv var value)
      (when (string-equal "PATH" var)
        (if globaly
            (setq-default exec-path (append (parse-colon-path value) (list exec-directory))
                          eshell-path-env value)
          (setq-local exec-path (append (parse-colon-path value) (list exec-directory))))))))


(defmacro npmjs-nvm-with-env-vars (version &rest body)
  "Set environment variables for a specific Node.js version.

Argument VERSION is a string specifying the Node.js version to use.

Remaining arguments BODY are the forms to be executed with the Node.js VERSION
environment variables set."
  (declare
   (indent defun))
  `(let ((process-environment (copy-sequence process-environment)))
    (npmjs-set-env-vars ,version)
    (let ((npmjs--current-node-version ,version))
     ,@body)))

(defmacro npmjs-with-temp-buffer (&rest body)
  "Execute BODY with temporary buffer and Node.js version.

Remaining arguments BODY are Lisp expressions that are evaluated in the context
of the temporary buffer with Node.js version environment variables set."
  (let ((version (make-symbol "version")))
    `(let ((,version npmjs--current-node-version))
       (with-temp-buffer
         (npmjs-nvm-with-env-vars
           (setq npmjs--current-node-version
                 ,version)
           ,@body)))))

(defmacro npmjs-parse-help-with-output (output &rest body)
  "Parse npm help OUTPUT in a temporary buffer.

Argument OUTPUT is a string containing the help output to parse.

Remaining arguments BODY are forms that are evaluated with the OUTPUT inserted
into a temporary buffer."
  (declare (indent 1)
           (debug t))
  `(npmjs-with-temp-buffer
    (save-excursion
      (insert ,output))
    ,@body))

(defun npmjs-exec-with-args (command &rest args)
  "Execute npm COMMAND with arguments and return output.

Argument COMMAND is a string representing the npm command to execute.

Remaining arguments ARGS are strings that represent additional arguments to pass
to the npm command."
  (let ((cmdline (mapconcat (lambda (it)
                              (if (string-match-p "[\s\t\n]" it)
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

(defmacro npmjs-nvm-with-current-node-version (&rest body)
  "Set environment variables and run BODY with Node.js version.

Remaining arguments BODY are forms that are evaluated with the Node.js version
environment variables set."
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
  "Set Node Version Manager path from environment or home directory."
  (when-let* ((nvm-dir (or (getenv "NVM_DIR")
                           (when (file-exists-p "~/.nvm/")
                             "~/.nvm/")))
              (file (expand-file-name "nvm.sh" nvm-dir)))
    (when (file-exists-p file)
      file)))

(defvar npmjs-nvm-remote-node-versions-alist nil)

(defun npmjs-nvm-ls-remote (&rest args)
  "List remote Node versions using `nvm ls-remote`.

Remaining arguments ARGS are strings that represent additional arguments to pass
to the `nvm ls-remote` command."
  (when-let* ((nvm-path (npmjs-nvm-path))
              (node-versions
               (apply #'npmjs-exec-with-args "source" nvm-path
                      "&&" "nvm"
                      "ls-remote"
                      "--no-colors"
                      args)))
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
  "Fetch and display Node.js versions for selection."
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
  "Remove prefix from VERSION string if it matches a regex.

Argument VERSION is a string representing the version number to be processed."
  (if (and version (string-match-p npmjs-nvm-version-re version))
      (substring-no-properties version 1)
    version))

(defun npmjs-expand-when-exists (filename &optional directory)
  "Expand and return file path if it exists.

Argument FILENAME is the name of the file to expand.

Optional argument DIRECTORY is the directory to prepend to FILENAME; defaults to
the current DIRECTORY if not specified."
  (let ((file (expand-file-name filename directory)))
    (when (file-exists-p file)
      file)))

(defun npmjs-nvm--installed-versions-dirs ()
  "List Node.js versions installed by nvm."
  (let* ((files (mapcan
                 (lambda (versions-dir)
                   (directory-files versions-dir t
                                    directory-files-no-dot-files-regexp))
                 (delq nil
                       (list
                        (npmjs-expand-when-exists
                         npmjs-nvm-dir)
                        (npmjs-expand-when-exists
                         "versions"
                         npmjs-nvm-dir))))))
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
  "List Node.js versions installed via nvm."
  (mapcar (lambda (it)
            (cons (file-name-nondirectory it) it))
          (npmjs-nvm--installed-versions-dirs)))

(defun npmjs-nvm--version-from-string (version-string)
  "Extract numeric parts from a version string.

Argument VERSION-STRING is a string representing a version number, which is
split and converted to a list of numbers."
  (mapcar #'string-to-number (split-string version-string "[^0-9]" t)))

(defun npmjs-nvm--version-match-p (matcher version)
  "Check if VERSION lists match or start with nil.

Argument MATCHER is a list representing the VERSION pattern to match against.

Argument VERSION is a list representing the version to be checked for a match."
  (or (eq (car matcher) nil)
      (and (eq (car matcher)
               (car version))
           (npmjs-nvm--version-match-p (cdr matcher)
                                       (cdr version)))))

(defun npmjs-nvm-version-compare (a b)
  "Compare version lists recursively.

Argument A is a list representing a version number, where each element is a part
of the version to be compared.

Argument B is A list representing another version number, structured similarly
to A for comparison purposes."
  (if (eq (car a)
          (car b))
      (npmjs-nvm-version-compare (cdr a)
                                 (cdr b))
    (< (car a)
       (car b))))

(defun npmjs-nvm-find-exact-version-for (short)
  "Find exact Node.js version match.

Argument SHORT is a string representing a version number, which may or may not
start with \"v\", \"node\", or \"iojs\"."
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
  "Retrieve Node.js version from project's `.nvmrc' file."
  (when-let ((nvmrc (locate-dominating-file default-directory ".nvmrc")))
    (with-temp-buffer (insert-file-contents
                       (expand-file-name ".nvmrc" nvmrc))
                      (string-trim
                       (buffer-substring-no-properties (point-min)
                                                       (point-max))))))

(defun npmjs-nvm-read-installed-node-versions ()
  "List installed Node.js versions for selection."
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
  "Extract environment variables for a given Node version.

Argument VERSION is a string representing the Node.js version to get environment
variables for."
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
  "Retrieve Node version-specific environment variables.

Argument VERSION is a string representing the Node.js version to get the
environment for."
  (when-let* ((flags (npmjs-nvm-get-env-vars version)))
    (let ((regexp (mapconcat #'identity (mapcar #'car flags)
                             "\\|")))
      (append (mapcar (lambda (it)
                        (concat (car it) "=" (cadr it)))
                      flags)
              (seq-remove (apply-partially #'string-match-p regexp)
                          process-environment)))))

(defun npmjs-current-default-node-version ()
  "Fetch and format the current Node.js version installed."
  (let ((default-directory (expand-file-name "~/")))
    (npmjs-nvm-strip-prefix
     (string-trim
      (shell-command-to-string
       "node -v")))))

(defun npmjs-confirm-node-version (&optional all)
  "Check and select Node.js version.

Optional argument ALL is a boolean; if non-nil, all installed Node versions are
considered."
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
                   (append
                    (delq nil
                          (list
                           default-global
                           current-global
                           project-node
                           curr-node))
                    installed))))
         (annotf (lambda (it)
                   (cond ((equal it default-global)
                          " Global")
                         ((equal it current-global)
                          " Current global")
                         ((equal it project-node)
                          " required by (nvmrc)")
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
                                      ,annotf)
                                     (display-sort-fn .
                                      ,(lambda (it)
                                         (seq-sort-by
                                          (or
                                           (cdr
                                            (assoc-string
                                             it
                                             '((project-node . 1)
                                               (default-global . 2)
                                               (current-global . 3)
                                               (curr-node . 4))))
                                           5)
                                          #'< it))))
                                 (complete-with-action
                                  action
                                  cands
                                  str
                                  pred))))))))
(defvar npmjs-node-installing nil)

;;;###autoload
(defun npmjs-nvm-install-node-version (version &optional args)
  "Install a specified Node.js VERSION using NVM.

Argument VERSION is a string representing the Node.js version to install.

Optional argument ARGS is a list of additional arguments to pass to the Node.js
installation command."
  (interactive
   (let* ((targs (transient-args transient-current-command))
          (version
           (if (not (member "--lts" targs))
               (progn (setq npmjs-nvm-remote-node-versions-alist
                            (npmjs-nvm-ls-remote))
                      (npmjs-nvm-strip-prefix
                       (npmjs-nvm-read-remote-node-version)))
             (setq npmjs-nvm-remote-node-versions-alist
                   (npmjs-nvm-ls-remote "--lts"))
             (prog1 (npmjs-nvm-strip-prefix
                     (npmjs-nvm-read-remote-node-version))))))
     (list version targs)))
  (when-let ((nvm-path (npmjs-nvm-path)))
    (if (member version (mapcar (lambda (it)
                                  (npmjs-nvm-strip-prefix (car it)))
                                (npmjs-nvm--installed-versions)))
        (setq npmjs--current-node-version version)
      (let ((cmd (read-string "Run?" (string-join
                                      (delq nil (append (list "source" nvm-path
                                                              "&&"
                                                              "nvm"
                                                              "install"
                                                              version)
                                                        (remove "--lts" args)))
                                      "\s"))))
        (setq npmjs-node-installing t)
        (when transient-current-command
          (transient-setup transient-current-command))
        (npmjs-exec-in-dir
         cmd
         default-directory
         (lambda (&rest _)
           (setq npmjs-node-installing nil)
           (setq npmjs--current-node-version version)
           (when transient-current-command
             (transient-setup transient-current-command)))
         (lambda ()
           (setq npmjs-node-installing nil)
           (when transient-current-command
             (transient-setup transient-current-command))
           (message "Error installing %s" version)))))))

(defun npmjs--write-nvm-file (version)
  "Create or update the `.nvmrc' file with the given VERSION.

Argument VERSION is a string representing the Node.js version to write into the
.nvmrc file."
  (when-let* ((dir (or
                    (locate-dominating-file default-directory ".nvmrc")
                    (car (npmjs-get-project-roots))))
              (file (expand-file-name ".nvmrc" dir)))
    (when (yes-or-no-p (format "Write to %s?" file))
      (write-region (npmjs-nvm-strip-prefix version) nil file nil nil nil nil))))

;;;###autoload
(defun npmjs-write-nvm-file (version)
  "Create or update the `.nvmrc' file with selected Node.js VERSION.

Argument VERSION is a string representing the Node.js version to write into the
.nvmrc file."
  (interactive (list (npmjs-confirm-node-version)))
  (npmjs--write-nvm-file version))

(defvar npmjs-installing-nvm nil)

(defun npmjs--install-nvm (tag)
  "Download and install NVM from a given TAG.

Argument TAG is a string specifying the version tag of nvm to install."
  (let
      ((url (format
             "https://raw.githubusercontent.com/nvm-sh/nvm/%s/install.sh" tag)))
    (npmjs-message
     "Downloading %s" url)
    (setq npmjs-installing-nvm t)
    (url-retrieve url
                  (lambda (status &rest _)
                    (unwind-protect
                        (progn
                          (if-let ((err
                                    (when-let ((err (plist-get status :error)))
                                      (concat (propertize
                                               "npmjs-nvm error: "
                                               'face
                                               'error)
                                              (mapconcat (apply-partially #'format
                                                                          "%s")
                                                         (delq nil
                                                               (list (or
                                                                      (when-let
                                                                          ((type
                                                                            (ignore-errors
                                                                              (cadr
                                                                               err))))
                                                                        type)
                                                                      err)
                                                                     (ignore-errors
                                                                       (caddr
                                                                        err))
                                                                     (ignore-errors
                                                                       (alist-get
                                                                        'message
                                                                        (car-safe
                                                                         (last
                                                                          err))))
                                                                     (ignore-errors
                                                                       (alist-get
                                                                        'documentation_url
                                                                        (car-safe
                                                                         (last
                                                                          err))))))
                                                         " ")))))
                              (message err)
                            (let ((script
                                   (when (and (boundp
                                               'url-http-end-of-headers)
                                              url-http-end-of-headers)
                                     (goto-char url-http-end-of-headers)
                                     (delete-region (point-min)
                                                    (point))
                                     (buffer-string)))
                                  (script-status))
                              (with-temp-buffer
                                (insert script)
                                (setq script-status (shell-command-on-region (point-min)
                                                                             (point-max)
                                                                             "bash"
                                                                             (current-buffer)
                                                                             t
                                                                             (current-buffer)))
                                (if (zerop script-status)
                                    (npmjs-message "NVM installed")
                                  (npmjs-message (string-trim (buffer-string))))))))
                      (setq npmjs-installing-nvm nil))))))



;;;###autoload
(defun npmjs-install-nvm (&optional force)
  "Install Node Version Manager if confirmed or forced.

Optional argument FORCE is a prefix argument; if non-nil, it forces the
installation without confirmation."
  (interactive "P")
  (when (or force
            (yes-or-no-p "Download and install nvm?"))
    (npmjs--install-nvm (npmjs-nvm-read-nvm-release))))

;;;###autoload
(defun npmjs-ensure-nvm-install ()
  "Ensure NVM is installed or install it."
  (interactive)
  (unless (npmjs-nvm-path)
    (npmjs-install-nvm)))


(defun npmjs-download-url (url)
  "Download URL and return string."
  (let ((download-buffer (url-retrieve-synchronously url)))
    (prog1
        (with-current-buffer download-buffer
          (set-buffer download-buffer)
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (forward-char)
          (delete-region (point-min)
                         (point))
          (buffer-string))
      (kill-buffer download-buffer))))

(defvar npmjs-nvm-releases nil)

(defun npmjs--load-nvm-releases ()
  "Fetch and parse nvm releases from GitHub API."
  (let* ((str (decode-coding-string
               (npmjs-download-url
                "https://api.github.com/repos/nvm-sh/nvm/releases")
               'dos))
         (data (npmjs-json-parse-string str
                                        'alist 'list))
         (tags (mapcar
                (lambda (it)
                  (let ((tag (alist-get 'tag_name it))
                        (body (or (alist-get 'body it) "")))
                    (cons tag (replace-regexp-in-string "[\r]" "\n" body))))
                data)))
    tags))

(defun npmjs-minibuffer-get-metadata ()
  "Return current minibuffer completion metadata."
  (completion-metadata
   (buffer-substring-no-properties
    (minibuffer-prompt-end)
    (max (minibuffer-prompt-end)
         (point)))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun npmjs-minibuffer-ivy-selected-cand ()
  "Return the currently selected item in Ivy."
  (when (and (memq 'ivy--queue-exhibit post-command-hook)
             (boundp 'ivy-text)
             (boundp 'ivy--length)
             (boundp 'ivy-last)
             (fboundp 'ivy--expand-file-name)
             (fboundp 'ivy-state-current))
    (cons
     (completion-metadata-get (ignore-errors (npmjs-minibuffer-get-metadata))
                              'category)
     (ivy--expand-file-name
      (if (and (> ivy--length 0)
               (stringp (ivy-state-current ivy-last)))
          (ivy-state-current ivy-last)
        ivy-text)))))

(defun npmjs-minibuffer-get-default-candidates ()
  "Return all current completion candidates from the minibuffer."
  (when (minibufferp)
    (let* ((all (completion-all-completions
                 (minibuffer-contents)
                 minibuffer-completion-table
                 minibuffer-completion-predicate
                 (max 0 (- (point)
                           (minibuffer-prompt-end)))))
           (last (last all)))
      (when last (setcdr last nil))
      (cons
       (completion-metadata-get (npmjs-minibuffer-get-metadata) 'category)
       all))))

(defun npmjs-get-minibuffer-get-default-completion ()
  "Target the top completion candidate in the minibuffer.
Return the category metadatum as the type of the target."
  (when (and (minibufferp) minibuffer-completion-table)
    (pcase-let* ((`(,category . ,candidates)
                  (npmjs-minibuffer-get-default-candidates))
                 (contents (minibuffer-contents))
                 (top (if (test-completion contents
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate)
                          contents
                        (let ((completions (completion-all-sorted-completions)))
                          (if (null completions)
                              contents
                            (concat
                             (substring contents
                                        0 (or (cdr (last completions)) 0))
                             (car completions)))))))
      (cons category (or (car (member top candidates)) top)))))

(defvar npmjs-minibuffer-targets-finders
  '(npmjs-minibuffer-ivy-selected-cand
    npmjs-get-minibuffer-get-default-completion))

(defun npmjs-minibuffer-get-current-candidate ()
  "Return cons filename for current completion candidate."
  (let (target)
    (run-hook-wrapped
     'npmjs-minibuffer-targets-finders
     (lambda (fun)
       (when-let ((result (funcall fun)))
         (when (and (cdr-safe result)
                    (stringp (cdr-safe result))
                    (not (string-empty-p (cdr-safe result))))
           (setq target result)))
       (and target (minibufferp))))
    target))

(defun npmjs-minibuffer-exit-with-action (action)
  "Call ACTION with current candidate and exit minibuffer."
  (pcase-let ((`(,_category . ,current)
               (npmjs-minibuffer-get-current-candidate)))
    (progn (run-with-timer 0.1 nil action current)
           (abort-minibuffers))))


(defun npmjs-minibuffer-action-no-exit (action)
  "Call ACTION with minibuffer candidate in its original window."
  (pcase-let ((`(,_category . ,current)
               (npmjs-minibuffer-get-current-candidate)))
    (with-minibuffer-selected-window
      (funcall action current))))


(defun npmjs-completing-read-with-preview (prompt collection &optional
                                                  preview-action keymap
                                                  predicate require-match
                                                  initial-input hist def
                                                  inherit-input-method)
  "Read COLLECTION in minibuffer with PROMPT and KEYMAP.
See `completing-read' for PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF
INHERIT-INPUT-METHOD."
  (let ((collection (if (stringp (car-safe collection))
                        (copy-tree collection)
                      collection)))
    (minibuffer-with-setup-hook
        (lambda ()
          (when (minibufferp)
            (when keymap
              (let ((map (make-composed-keymap keymap
                                               (current-local-map))))
                (use-local-map map)))
            (when preview-action
              (add-hook 'after-change-functions (lambda (&rest _)
                                                  (interactive)
                                                  (npmjs-minibuffer-action-no-exit
                                                   preview-action))
                        nil t))))
      (completing-read prompt
                       collection
                       predicate
                       require-match initial-input hist
                       def inherit-input-method))))



(defun npmjs-nvm-read-nvm-release ()
  "Display NVM version details before selection."
  (minibuffer-with-setup-hook
      (lambda ()
        (when (minibufferp)
          (add-hook 'minibuffer-exit-hook (lambda ()
                                            (when-let ((buffer
                                                        (get-buffer
                                                         "*npmjs-nvm-release*")))
                                              (kill-buffer buffer)))
                    nil t)))
    (npmjs-completing-read-with-preview "NVM Version: "
                                        (or npmjs-nvm-releases
                                            (setq npmjs-nvm-releases
                                                  (npmjs--load-nvm-releases)))
                                        (lambda (tag)
                                          (when-let* ((body
                                                       (cdr
                                                        (assoc-string tag
                                                                      npmjs-nvm-releases)))
                                                      (buffer
                                                       (get-buffer-create
                                                        "*npmjs-nvm-release*")))
                                            (with-current-buffer buffer
                                              (with-current-buffer-window
                                                  buffer
                                                  (cons
                                                   'display-buffer-in-direction
                                                   '((window-height . window-preserve-size)))
                                                  (lambda (window _value)
                                                    (with-selected-window window
                                                      (setq buffer-read-only t)
                                                      (let
                                                          ((inhibit-read-only t))
                                                        (erase-buffer)
                                                        (save-excursion
                                                          (insert tag "\n" body))
                                                        (when (fboundp
                                                               'gfm-view-mode)
                                                          (gfm-view-mode))
                                                        (when-let*
                                                            ((quit-key
                                                              (where-is-internal
                                                               'quit-window
                                                               special-mode-map
                                                               t t t))
                                                             (map
                                                              (make-sparse-keymap)))
                                                          (define-key map
                                                                      quit-key
                                                                      #'quit-window)
                                                          (use-local-map
                                                           (make-composed-keymap
                                                            map
                                                            (current-local-map))))))))))))))


;;;###autoload
(defun npmjs-nvm-alias (version)
  "Set default Node VERSION and update environment variables.

Argument VERSION is a string representing the Node.js version to set as the
default alias in nvm."
  (interactive (list (npmjs-confirm-node-version t)))
  (npmjs-nvm-use-other-node-version-node-globally version)
  (npmjs-run-nvm-command
   nil
   "nvm" "alias" "default" version "&&"
   "nvm" "use" "default"))

;;;###autoload
(defun npmjs-nvm-use-other-node-version-node-globally (version)
  "Switch to system Node.js version.

Argument VERSION is a string specifying the Node.js version to switch to."
  (interactive (list (npmjs-confirm-node-version t)))
  (npmjs-nvm-use-other-node-version version t))

;;;###autoload
(defun npmjs-nvm-use-other-node-version (version &optional globally)
  "Switch to a different Node.js version.

Argument VERSION is a string representing the Node.js version to use.

Optional argument GLOBALLY is a boolean flag; if non-nil, the environment
variables are set globally."
  (interactive (list (npmjs-confirm-node-version t) current-prefix-arg))
  (if globally
      (npmjs-set-env-vars version globally)
    (let ((process-environment (copy-sequence process-environment)))
      (npmjs-set-env-vars version)))
  (when transient-current-command
    (transient-setup transient-current-command)))

;;;###autoload
(defun npmjs-nvm-uninstall-node (version)
  "Uninstall a selected Node.js VERSION using nvm.

Argument VERSION is a string representing the Node.js version to uninstall."
  (interactive (list (car (npmjs-nvm-read-installed-node-versions))))
  (npmjs-run-nvm-command (lambda ()
                           (message "Removed node %s" version))
                         "nvm" "uninstall" version))

;;;###autoload
(defun npmjs-nvm-jump-to-installed-node ()
  "Open selected Node.js version file."
  (interactive)
  (let ((cell (npmjs-nvm-read-installed-node-versions)))
    (if (and (cdr cell)
             (file-exists-p (cdr cell))
             (find-file (cdr cell)))
        (user-error "Not found %s" cell))))

;; nvm end

(defun npmjs-get-npm-version ()
  "Fetch and trim the current npm version."
  (npmjs-with-temp-buffer
   (let ((status (call-process "npm"  nil t nil "-v")))
     (when (zerop status)
       (string-trim (buffer-string))))))

(defun npmjs-online-p ()
  "Check if npmjs is online by testing network interfaces."
  (if (fboundp 'network-interface-list)
      (seq-some (lambda (iface)
                  (unless (equal "lo" (car iface))
                    (member 'up (car (last (network-interface-info
                                            (car iface)))))))
                (network-interface-list))
    t))

(defvar npmjs-json-hash (make-hash-table :test 'equal))

(defun npmjs-read-json (file &optional json-type)
  "Parse JSON from FILE and cache it.

Argument FILE is the path to the JSON file to read.

Optional argument JSON-TYPE specifies the type used to represent objects in the
returned JSON; it defaults to `alist'."
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
  "Clear cached package JSON data from hash table."
  (dolist (key
           (hash-table-keys npmjs-json-hash))
    (remhash key npmjs-json-hash)))

(defun npmjs-get-project-root ()
  "Find the root directory of an npm project."
  (locate-dominating-file default-directory "package.json"))

(defun npmjs-get-package-json-path ()
  "Find and return the path to `package.json'."
  (when-let ((project-root (npmjs-get-project-root)))
    (expand-file-name "package.json" project-root)))

(defun npmjs-get-package-json-alist ()
  "Fetch and parse package.json as an alist."
  (when-let ((package-json-file (npmjs-get-package-json-path)))
    (ignore-errors (npmjs-read-json package-json-file
                                    'alist))))

(defun npmjs-get-package-json-scripts ()
  "Extract scripts from a package.json alist."
  (alist-get 'scripts (npmjs-get-package-json-alist)))

(defun npmjs-get-package-json-script (script)
  "Retrieve a SCRIPT from a package.json file.

Argument SCRIPT is a string specifying the name of the script to retrieve from
the package.json."
  (alist-get script
             (npmjs-get-package-json-scripts)))

(defun npmjs-exec-in-dir (command &optional directory callback error-callback)
  "Run shell COMMAND in DIRECTORY with optional CALLBACKS.

Argument COMMAND is a string representing the shell command to execute.

Optional argument DIRECTORY is a string specifying the directory in which to
execute COMMAND. If not provided, the current DIRECTORY is used.

Optional argument CALLBACK is a function toOptional argument ERROR-CALLBACK is a
function to call when the process exits with a non-zero status."
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
           (if (zerop (process-exit-status process))
               (progn
                 (npmjs-message "finished")
                 (when callback
                   (funcall callback output))
                 (when (bufferp (process-buffer process))
                   (kill-buffer (process-buffer process))))
             (if error-callback
                 (funcall error-callback)
               (user-error "%s\n%s" command output))))))
      (require 'comint)
      (when (fboundp 'comint-output-filter)
        (set-process-filter proc #'comint-output-filter)))))

(defun npmjs-get-workspaces ()
  "Retrieve workspace directories from a project's `package.json'."
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
  "Read and select npm workspace directories.

Optional argument PROMPT is the string to display as the prompt when asking the
user for input. It defaults to \"Workspace: \".

Optional argument INITIAL-INPUT is the initial input provided to the user in the
minibuffer.

Optional argument HISTORY is the history list to use for the completion."
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
  "Retrieve hierarchy of npm project roots."
  (let ((project-root (npmjs-get-project-root))
        (roots))
    (while project-root
      (push project-root roots)
      (setq project-root
            (let ((default-directory (expand-file-name "../" project-root)))
              (npmjs-get-project-root))))
    roots))

(defvar npmjs-history-dependencies nil)

(defun npmjs-run-nvm-command (cb &rest args)
  "Execute nvm commands with callback in specified directory.

Argument CB is a callback function to be called after the command execution.

Remaining arguments ARGS are strings that represent the command and its
arguments to be executed with nvm."
  (npmjs-exec-in-dir (string-join
                      (append (delq nil (list "." (npmjs-nvm-path)
                                              "&&"))
                              args)
                      "\s")
                     default-directory
                     (lambda (output)
                       (message output)
                       (when cb
                         (funcall cb)))))

(defun npmjs-compile-global (npm-command)
  "Run global NPM command after Node version check.

Argument NPM-COMMAND is a string representing the npm command to execute
globally."
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
  "Compile npm commands with optional arguments.

Argument NPM-COMMAND is a string representing the npm command to execute.

Remaining arguments ARGS are additional strings or objects that will be
converted to strings and passed as command arguments to NPM-COMMAND."
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
  "Repeat the last npmjs command for the project."
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
  "Execute npm commands in a comint buffer.

Argument COMMAND is a string representing the npm command to run.

Optional argument VERSION is a string specifying the Node.js version to use; if
nil, the current or system VERSION is used."
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

(defun npmjs-run-compile (command &optional buff-name env)
  "Run a compilation COMMAND using the current Node.js version.

Argument COMMAND is a string representing the shell command to execute.

Optional argument BUFF-NAME is either a string specifying the buffer name or a
function that generates the buffer name.

Optional argument ENV is a list of environment variables to set before running
the COMMAND; it defaults to `process-environment'."
  (npmjs-nvm-with-current-node-version
   (let ((compenv (or env process-environment)))
     (let* ((compilation-read-command nil)
            (compilation-environment compenv)
            (compile-command command)
            (compilation-buffer-name-function
             (if (functionp buff-name)
                 buff-name
               (lambda (&optional _mode)
                 (or buff-name
                     (concat (npmjs--get-project-buffer-name) "-compilation"))))))
       (compile compile-command 'npmjs-compilation-mode)))))

(defun npmjs-compilation-filter-hook ()
  "Apply ANSI color to compilation output."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(define-derived-mode npmjs-compilation-mode compilation-mode "NPMJS"
  "Customize NPM compilation buffer with color and truncation.

Define `npmjs-compilation-mode' as a major mode derived from `compilation-mode'
specifically tailored for handling NPM compilation buffers. Enable line
truncation within the buffer to ensure long lines do not wrap. Apply ANSI color
codes to the compilation output to enhance readability and distinguish between
different types of messages. Utilize the standard compilation keymap for
consistent navigation and interaction within the buffer. Add a custom filter
hook to process the compilation buffer's output, applying colorization with
`ansi-color-apply-on-region' from the point where new output begins to the end
of the buffer."
  (use-local-map compilation-mode-map)
  (setq-local truncate-lines nil)
  (add-hook 'compilation-filter-hook
            #'npmjs-compilation-filter-hook nil t))

;;;###autoload
(define-minor-mode npmjs-minor-mode
  "Remap compile commands to npmjs functions.

Enable `npmjs-minor-mode' to override the default compile and recompile commands
with npmjs-specific functions. Bind the `compile' command to
`npmjs-compile-command' and the `recompile' command to
`npmjs-repeat-compile-command'. If available, also remap
`projectile-compile-project' and `project-compile' to `npmjs-compile-command'.
Customize the actual commands executed by setting `npmjs-compile-command' and
`npmjs-repeat-compile-command'."
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
  "Manage npmjs sessions with read-only prompts.

Define `npmjs-mode', a major mode for interacting with npmjs sessions. This mode
derives from `comint-mode' and is tailored for running npm (Node Package
Manager) commands within an Emacs buffer. Set the command prompt to be read-only
to prevent accidental modifications. Enable line truncation to ensure long lines
do not wrap and remain readable. Configure the local compile command to use the
current npm command being executed. Initialize the compilation mode settings to
integrate with Emacs' compilation facilities, providing features like error
navigation and output filtering."
  (setq-local comint-prompt-read-only nil)
  (setq-local truncate-lines t)
  (setq-local compile-command npmjs--current-command)
  (compilation-setup t))

(defun npmjs-project-name ()
  "Retrieve the project's name from package.json."
  (alist-get 'name (npmjs-get-package-json-alist)))

(defun npmjs-project-display-name ()
  "Display project's name and version or directory name."
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
  "Generate a buffer name with the current Node.js version."
  (format "npmjs<global><%s>" npmjs--current-node-version))

(defun npmjs--get-project-buffer-name ()
  "Generate a buffer name for an npm project."
  (when-let ((name (npmjs-get-project-root)))
    (format "npmjs<%s>" (replace-regexp-in-string "^~/\\|/$" "" name))))

(defun npmjs-get-project-default-directory ()
  "Retrieve and expand the path to the project root."
  (when-let ((name (npmjs-get-project-root)))
    (expand-file-name name)))

(defun npmjs--get-buffer ()
  "Retrieve or create a buffer for `npmjs-mode'."
  (if (eq major-mode 'npmjs-mode)
      (current-buffer)
    (get-buffer-create
     (or
      (npmjs--get-project-buffer-name)
      (npmjs--get-global-buffer-name)))))

(defun npmjs--process-sentinel (proc state)
  "Display process status and trigger hooks after completion.

Argument PROC is the process associated with the npmjs command.

Argument STATE is a string representing the state of PROC when the sentinel is
called."
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
  "Create key bindings for npmjs Ivy actions."
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
  "Throw `done' signal with current Ivy input."
  (interactive)
  (when (boundp 'ivy-text)
    (npmjs-throw-done ivy-text)))

;;;###autoload
(defun npmjs-ivy-throw-done ()
  "Throw `done' signal with selected npm package name."
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
  "Set up a local keymap for npmjs Ivy minibuffer."
  (use-local-map
   (make-composed-keymap
    (npmjs-dependencies-get-ivy-map)
    (current-local-map))))

(defvar url-http-end-of-headers)

(defun npmjs-abort-url-retrieve (buff)
  "Cancel ongoing URL fetch and close buffer.

Argument BUFF is the buffer associated with the URL retrieval process to be
aborted."
  (when (buffer-live-p buff)
    (let ((proc (get-buffer-process buff)))
      (when proc
        (delete-process proc))
      (kill-buffer buff))))

(defvar npmjs-packages-data (make-hash-table :test 'equal))
(defvar npmjs-packages-last-input nil)
(defun npmjs-parse-search-response (str)
  "Parse JSON response and store package info in hash table.

Argument STR is a string containing the JSON response from the npmjs search
API."
  (let* ((data (npmjs-json-parse-string str
                                        'alist
                                        'list))
         (objects (alist-get 'objects data)))
    (dolist (it objects)
      (let* ((info (alist-get 'package it))
             (key (alist-get 'name info)))
        (puthash key info npmjs-packages-data)))))

(defun npmjs--run-in-buffer (buffer timer-sym fn &rest args)
  "Execute a function in a specified BUFFER, optionally canceling a timer.

Argument BUFFER is the buffer in which to run the function FN.

Argument TIMER-SYM is a symbol whose value should be a timer object.

Argument FN is the function to be applied in BUFFER.

Remaining arguments ARGS are passed to the function FN."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (let ((buffer-window (get-buffer-window buffer)))
        (if (and buffer-window
                 (not (eq (selected-window) buffer-window)))
            (with-selected-window buffer-window
              (apply fn args))
          (apply fn args)))
      (when-let ((timer-value (symbol-value timer-sym)))
        (when (timerp timer-value)
          (cancel-timer timer-value))))))
(defun npmjs-cancel-timer (timer-sym)
  "Cancel the specified timer and reset it.

Argument TIMER-SYM is a symbol whose value is expected to be a timer object."
  (let ((timer-value (symbol-value timer-sym)))
    (when (timerp timer-value)
      (cancel-timer timer-value)
      (set timer-sym nil))))

(defun npmjs-debounce (timer-sym delay fn &rest args)
  "DELAY execution of a function with cancellation of prior calls.

Argument TIMER-SYM is a symbol used to store the reference to the timer.

Argument DELAY is a number representing the delay in seconds before the function
FN is called.

Argument FN is the function to be called after the delay.

Remaining arguments ARGS are passed to the function FN when it is called."
  (let ((timer-value (symbol-value timer-sym)))
    (when (timerp timer-value)
      (cancel-timer timer-value)))
  (set timer-sym (apply #'run-with-timer delay nil
                        #'npmjs--run-in-buffer
                        (current-buffer)
                        timer-sym
                        fn
                        args)))

(defvar npmjs-request-buffer nil)
(defvar npmjs-registry-timer nil)

(defun npmjs-wnd-complete ()
  "Complete npmjs search in minibuffer."
  (npmjs-abort-url-retrieve npmjs-request-buffer)
  (when-let ((wnd (active-minibuffer-window)))
    (with-selected-window wnd
    ;; (completion--flush-all-sorted-completions)
      (let ((text (string-trim (buffer-substring-no-properties
                                (minibuffer-prompt-end)
                                (line-end-position)))))
        (unless (string-empty-p text)
          (setq npmjs-request-buffer
                (url-retrieve
                 (format
                  "https://registry.npmjs.org/-/v1/search?%s"
                  (url-build-query-string
                   `(("text"
                      ,text))))
                 (lambda (status &rest _)
                   (cond ((plist-get status
                                     :error))
                         (t
                          (when url-http-end-of-headers
                            (npmjs-parse-search-response
                             (buffer-substring-no-properties
                              url-http-end-of-headers
                              (point-max)))
                            (when (window-live-p wnd)
                              (npmjs-debounce
                               'npmjs-registry-timer 2
                               (lambda ()
                                 (when (minibuffer-window-active-p (selected-window))
                                   (unwind-protect
                                       (progn (setq deactivate-mark nil)
                                              (throw 'exit nil))
                                     (run-with-timer 0.5 nil #'npmjs-search-native text)))))))))))))))))

(defun npmjs-search-native (&optional input)
  "Search for npm packages with dynamic completion.

Optional argument INPUT is the initial input in the minibuffer."
  (interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        (when (minibufferp)
          (add-hook 'post-self-insert-hook #'npmjs-wnd-complete nil t)))
    (completing-read
     "Package "
     (completion-table-dynamic (lambda (_text)
                                 (hash-table-keys npmjs-packages-data)))
     nil nil input)))

;;;###autoload
(defun npmjs-ivy-read-npm-dependency (&optional prompt initial-input history)
  "Search npm dependencies with Ivy interface.

Optional argument PROMPT is the string to display as the prompt in the
minibuffer.

Optional argument INITIAL-INPUT is the initial input when the minibuffer is
activated.

Optional argument HISTORY is the minibuffer history list to use."
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
            (let* ((cmd-str
                    (if (ignore-errors (npmjs-online-p))
                        "npm search --no-color --parseable "
                      "npm search --prefer-offline --no-color --parseable ")))
              (ivy-read
               (concat (string-trim (or prompt "Repo:")) " ")
               (lambda (str)
                 (or
                  (ivy-more-chars)
                  (progn
                    (counsel--async-command
                     (concat cmd-str
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
               :caller 'npmjs-ivy-read-npm-dependency)))
        (remove-hook 'minibuffer-setup-hook #'npmjs-ivy-minibuffer-setup)))))

(defun npmjs-read-new-dependency (&optional prompt initial-input history)
  "PROMPT for a new npm dependency.

Optional argument PROMPT is the string to display as the prompt when asking for
input.

Optional argument INITIAL-INPUT is the initial input provided to the minibuffer.

Optional argument HISTORY is the minibuffer history list to use."
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
  "Highlight the current line with a success face."
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
  "Navigate to the beginning of a list entry."
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
  "Convert npm search OUTPUT to rows.

Argument OUTPUT is a string containing the raw output from the npmjs search
command."
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
  "Refresh the table with updated npmjs search results."
  (setq tabulated-list-entries (or (npmjs-search-parseable-to-rows
                                    npmjs-bmenu-search-output)
                                   tabulated-list-entries))
  (tabulated-list-print t)
  (npmjs-search-highlight-current))

(defun npmjs-search-package-by-regexp (regexp &optional args)
  "Search NPM packages using regex and display results.

Argument REGEXP is a string containing the regular expression to search for in
package names.

Optional argument ARGS is a string or list of strings representing additional
command-line arguments to pass to the `npm search` command."
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
  "Move to next or previous line, wrapping around at edges.

Optional argument N is the number of lines to move forward; it defaults to 1."
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
  "Move forward N lines in npmjs search buffer.

Optional argument N is the number of lines to move forward; it defaults to 1."
  (if (eq npmjs-bmenu-search-buff-name (buffer-name (current-buffer)))
      (npmjs-search-package--forward-line-0 n)
    (when-let ((buff (get-buffer npmjs-bmenu-search-buff-name)))
      (with-current-buffer buff
        (npmjs-search-package--forward-line-0 n)))))

(defun npmjs-search-package--next-line ()
  "Move to the next line in npmjs package search."
  (interactive)
  (npmjs-search-package--forward-line 1))

(defun npmjs-search-package--prev-line ()
  "Move to the previous line in search results."
  (interactive)
  (npmjs-search-package--forward-line -1))

(defun npmjs-search-package--beg-of-buffer ()
  "Move to the beginning of a buffer window."
  (interactive)
  (when-let ((wind (get-buffer-window npmjs-bmenu-search-buff-name)))
    (with-selected-window wind
      (goto-char (point-min))
      (unless (tabulated-list-get-id)
        (forward-line 1)
        (npmjs-search-highlight-current)))))

(defun npmjs-search-package--end-of-buffer ()
  "Navigate to and highlight the last entry."
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
  "Toggle mark on a package in npmjs search buffer."
  (interactive)
  (if (eq npmjs-bmenu-search-buff-name (buffer-name (current-buffer)))
      (npmjs--mark-or-unmark)
    (when-let ((buff (get-buffer npmjs-bmenu-search-buff-name)))
      (with-current-buffer buff
        (npmjs--mark-or-unmark)))))

(defun npmjs--update-current (cand)
  "Update the minibuffer with the provided candidate.

Argument CAND is a string representing the candidate to be inserted into the
minibuffer."
  (if (minibufferp)
      (progn (delete-minibuffer-contents)
             (insert cand))
    (when-let ((wind (active-minibuffer-window)))
      (with-selected-window wind
        (delete-minibuffer-contents)
        (insert cand)))))

(defun npmjs--mark-or-unmark ()
  "Toggle package marking for npmjs operations."
  (when-let ((name (tabulated-list-get-id (point))))
    (setq npmjs--marked-packages
          (if (member name npmjs--marked-packages)
              (delete name npmjs--marked-packages)
            (append npmjs--marked-packages (list name))))
    (let ((marked npmjs--marked-packages))
      (npmjs--update-current (string-join marked ",")))))

(defun npmjs-search-package-fn (args buff)
  "Customize keybindings and search npm packages.

Argument ARGS is a list of additional arguments to pass to the search function.

Argument BUFF is the buffer where the search is performed."
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
  "Search NPM packages with idle timer.

Optional argument ARGS is a list of additional arguments to pass to the
`npmjs-search-package-fn' function."
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
  "Display npm package search results in a table.

Display a tabulated list of npm packages with columns for name, description, and
version. Enable highlighting of the current selection and disable the cursor.
Set custom functions for reverting the buffer and handling post-command actions.
Initialize the list header with the specified column formats."
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
  "Display a list of unused definitions with name, version, and description.

Display a tabulated list report of unused definitions, including columns for the
package name, version, and description. Set the column widths for `Name' to 40
characters, `Version' to 10 characters, and `Description' to 33 characters, with
additional padding for readability. Initialize the list header with these
settings."
  (setq-local tabulated-list-format
              [("Name" 40 nil)
               ("Version" 10 nil)
               ("Description" 33 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun npmjs-json-parse-string (str &optional object-type array-type null-object
                                    false-object)
  "Parse JSON from string with customizable types.

Argument STR is a JSON string to parse.

Optional argument OBJECT-TYPE specifies the Lisp type to represent JSON objects;
defaults to `alist'.

Optional argument ARRAY-TYPE specifies the Lisp type to represent JSON arrays;
defaults to `array'.

Optional argument NULL-OBJECT specifies the Lisp object to represent JSON null;
defaults to :null.

Optional argument FALSE-OBJECT specifies the Lisp object to represent JSON
false; defaults to :false."
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

(declare-function json-read "json")
(defun npmjs-json-read-buffer (&optional object-type array-type null-object
                                         false-object)
  "Parse JSON from buffer using specified types.

Optional argument OBJECT-TYPE is the type used to represent objects; it can be
`hash-table', `alist' or `plist'. It defaults to `alist'.

Optional argument ARRAY-TYPE specifies which Lisp type is used to represent
arrays; it can be `list' or `vector'. It defaults to `vector'.

Optional argument NULL-OBJECT specifies which object to use to represent a JSON
null value. It defaults to `:null'.

Optional argument FALSE-OBJECT specifies which object to use to represent a JSON
false value. It defaults to `:false'."
  (if (and (fboundp 'json-parse-string)
           (fboundp 'json-available-p)
           (json-available-p))
      (json-parse-buffer
       :object-type (or object-type 'alist)
       :array-type
       (pcase array-type
         ('list 'list)
         ('vector 'array)
         (_ 'array))
       :null-object (or null-object :null)
       :false-object (or false-object :false))
    (let ((json-object-type (or object-type 'alist))
          (json-array-type
           (pcase array-type
             ('list 'list)
             ('array 'vector)
             (_ 'vector)))
          (json-null (or null-object :null))
          (json-false (or false-object :false)))
      (json-read))))

(declare-function json-read "json")

(defun npmjs-json-read-file (file &optional object-type array-type null-object
                                  false-object)
  "Parse JSON from a FILE into an Emacs Lisp object.

Argument FILE is the name of the file to read from.

Optional argument OBJECT-TYPE is the type used to represent objects; it can be
`hash-table', `alist' or `plist'. It defaults to `alist'.

Optional argument ARRAY-TYPE specifies which Lisp type is used to represent
arrays; it can be `list' or `vector'. It defaults to `vector'.

Optional argument NULL-OBJECT specifies which object to use to represent a JSON
null value. It defaults to `:null'.

Optional argument FALSE-OBJECT specifies which object to use to represent a JSON
false value. It defaults to `:false'."
  (if (and (fboundp 'json-parse-string)
           (fboundp 'json-available-p)
           (json-available-p))
      (with-temp-buffer (insert-file-contents file)
                        (goto-char (point-min))
                        (json-parse-buffer
                         :object-type (or object-type 'alist)
                         :array-type
                         (pcase array-type
                           ('list 'list)
                           ('vector 'array)
                           (_ 'array))
                         :null-object (or null-object :null)
                         :false-object (or false-object :false)))
    (with-temp-buffer
      (require 'json)
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((json-object-type (or object-type 'alist))
            (json-array-type
             (pcase array-type
               ('list 'list)
               ('array 'vector)
               (_ 'vector)))
            (json-null (or null-object :null))
            (json-false (or false-object :false)))
        (json-read)))))

(defun npmjs-parse-json-from-output (cmd &rest args)
  "Parse JSON from command output.

Argument CMD is the command to run in the shell.

Remaining arguments ARGS are strings passed as command arguments to CMD."
  (npmjs-with-temp-buffer
   (when (zerop (apply #'call-process cmd nil t nil args))
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
          'list))))))

(defun npmjs-get-package-files (dir)
  "Retrieve package files from a directory.

Argument DIR is a string specifying the directory to search for package files."
  (when (and (file-exists-p dir)
             (file-directory-p dir))
    (if-let ((package-json (npmjs-expand-when-exists "package.json" dir)))
        (list package-json)
      (seq-reduce
       (lambda (acc it)
         (when (file-directory-p it)
           (when-let ((found (npmjs-get-package-files it)))
             (setq acc (nconc acc found))))
         acc)
       (directory-files dir t directory-files-no-dot-files-regexp)
       '()))))



(defun npmjs-global-packages ()
  "List global npm packages installed on the system."
  (condition-case nil
      (npmjs-pluck-depenencies
       (npmjs-json-parse-string
        (npmjs-with-temp-buffer
         (shell-command
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
            end)))))
    (error
     (npmjs-global-packages-from-node-modules))))

(defun npmjs-current-global-node-modules-path ()
  "Retrieve global Node modules path."
  (ignore-errors
    (npmjs-nvm-with-current-node-version
     (npmjs-expand-when-exists
      "lib/node_modules"
      (car
       (process-lines
        "npm" "prefix"
        "--global"))))))

(defun npmjs-global-packages-from-node-modules ()
  "List global npm packages with versions from node_modules."
  (when-let ((node-modules-path (npmjs-current-global-node-modules-path)))
    (mapcar
     (npmjs--compose
       (npmjs--converge
        list
        [(apply-partially #'alist-get 'name)
         (apply-partially #'alist-get
                          'version)])
       (npmjs--rpartial npmjs-read-json 'alist))
     (npmjs-get-package-files node-modules-path))))

(defun npmjs-global-package-completion-table ()
  "Provide completion for global npm packages."
  (let* ((alist (ignore-errors (npmjs-global-packages)))
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
  "Provide completion for local npm packages."
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
  "List local npm package dependencies."
  (if-let* ((package-json-file (npmjs-get-package-json-path))
            (package-json (ignore-errors (npmjs-read-json package-json-file
                                                          'alist))))
      (npmjs-pluck-depenencies package-json)
    '()))

(defun npmjs-s-strip-props (item)
  "Remove text properties from a string.

Argument ITEM is the object from which text properties will be removed. It can
be a string, a symbol, or nil."
  (cond ((stringp item)
         (let ((str (seq-copy item)))
           (set-text-properties 0 (length str) nil str)
           str))
        ((and item (symbolp item))
         (symbol-name item))
        (nil item)))

(defun npmjs-pluck-depenencies (package-json-alist)
  "Extract dependencies from a given alist.

Argument PACKAGE-JSON-ALIST is an alist representing the parsed package.json
file."
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
  "Fetch distribution tags for an NPM package.

Argument PACKAGE is the name of the npm package to retrieve distribution tags
for."
  (let ((data (npmjs-get-package-info package)))
    (mapcar #'car (alist-get 'dist-tags data))))

(defun npmjs-get-package-info (package)
  "Fetch and parse JSON info for an npm package.

Argument PACKAGE is the name of the npm package to retrieve information for."
  (npmjs-parse-json-from-output "npm" "view" "--json" package))

(defun npmjs-get-package-versions (package)
  "Fetch version list of a given npm package.

Argument PACKAGE is a string specifying the name of the npm package to retrieve
versions for."
  (require 'json)
  (let ((data (npmjs-get-package-info package)))
    (alist-get 'versions data)))

(defun npmjs-confirm-package-dist-tags (package &optional prompt)
  "Check npm package distribution tags and select one.

Argument PACKAGE is a string representing the name of the npm package.

Optional argument PROMPT is a string to display as the prompt in the minibuffer
for completion."
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

(defun npmjs-strip-package-prefix-version (str)
  "Remove version after \"@\" from package name.

Argument STR is a string to be processed for stripping the package prefix and
version."
  (let ((end (string-match-p "@" str
                             (if (string-prefix-p "@" str)
                                 1
                               0))))
    (if end
        (substring-no-properties str 0 end)
      str)))

(defun npmjs-confirm-package-version (package &optional prompt)
  "Verify and select a PACKAGE version.

Argument PACKAGE is a string representing the name of the npm package.

Optional argument PROMPT is a string to display as the prompt in the minibuffer
for completion."
  (setq package (npmjs-strip-package-prefix-version package))
  (let ((versions (seq-reduce
                   (lambda (acc it)
                     (let ((prefixes '("^" ">="))
                           (l `(,it)))
                       (dolist (prefix prefixes)
                         (push (format "%s%s" prefix it) l))
                       (setq acc (append acc l))))
                   (npmjs-get-package-versions package)
                   '())))
    (format "%s@%s" package
            (completing-read
             (or prompt (format "%s@" package))
             versions))))

(defun npmjs-read-script (&optional prompt input hist)
  "Extract script from `package.json'.

Optional argument PROMPT is the prompt string to display when asking the user
for input. If not provided, the default PROMPT \"Script: \" is used.

Optional argument INPUT is the initial input to prefill the minibuffer.

Optional argument HIST is the history list to use for storing the INPUT
history."
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
  "List NPM man page file paths."
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
  "Display npm COMMAND manual page.

Argument COMMAND is a string representing the name of the npm command to view
the manual for."
  (when-let ((files (npmjs-man-paths-files))
             (file (seq-find (lambda (it)
                               (string= command (file-name-base it)))
                             files)))
    (let ((inhibit-message t))
      (npmjs-view-man-file file))))

;;;###autoload
(defun npmjs-show-help ()
  "Display npm help in a comint buffer."
  (interactive)
  (npmjs-run-as-comint "npm -l"))

;;;###autoload
(defun npmjs-show-manual (&optional command)
  "Display npm manual pages for selection.

Optional argument COMMAND is a cons cell where the car is the name of the npm
COMMAND and the cdr is the path to its manual page. If not provided, it defaults
to nil, and the user is prompted to select a command."
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
  (when-let ((file (if (consp command)
                       (cdr
                        command)
                     (seq-find (lambda (it)
                                 (string= command (file-name-base it)))
                               (npmjs-man-paths-files)))))
    (npmjs-view-man-file file)))

(defun npmjs-view-man-file (file)
  "Display a Node.js manual page for a given file.

Argument FILE is a string specifying the name of the manual page to view."
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
  "Retrieve or update npm man page paths.

Optional argument FORCE is a boolean; when non-nil, it forces the function to
recalculate the man paths."
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
  "Display Node.js manual pages using `man'.

Optional argument FN is a function to call instead of `man'.

Remaining arguments ARGS are strings passed as command arguments to FN or
`man'."
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
  "Find common string prefix between S1 and S2.

Argument S1 is a string to compare.

Argument S2 is another string to compare against S1."
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
  "Generate capitalized variants of a string.

Argument WORD is a string to be split and capitalized in various ways."
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
  "Truncate WORD to LEN characters, preserving properties.

Argument LEN is an integer representing the maximum length of the substring.

Argument WORD is a string from which the substring will be extracted."
  (if (> (length word) len)
      (substring-no-properties word 0 len)
    word))

(defun npmjs-key-splitted-variants (word len separator)
  "Generate unique key variants from a word.

Argument WORD is a string to be split into variants.

Argument LEN is an integer representing the length of each variant to be
generated.

Argument SEPARATOR is a string used as the delimiter for splitting WORD."
  (when-let* ((slen
               (when (> len 1)
                 (1- len)))
              (splitted (mapcar (apply-partially
                                 #'npmjs-key-builder-safe-substring 1)
                                (seq-drop (split-string word separator t)
                                          1)))
              (first-letter (npmjs-key-builder-safe-substring 1 word)))
    (seq-uniq
     (append (reverse (mapcar (lambda (it)
                                (unless (> slen (length it))
                                  (concat first-letter
                                          (string-join it ""))))
                              (seq-split splitted slen)))
             (list
              (mapconcat (lambda (_) first-letter)
                         (number-sequence 0 slen)
                         ""))))))


(defun npmjs-key-builder-get-all-key-strategies (word len)
  "Generate unique key strategies from a word.

Argument WORD is a string to be processed for key strategies.

Argument LEN is an integer specifying the desired length of the key strategies."
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
                             word)))))
    (seq-sort-by
     (lambda (it)
       (cond ((string-match-p "[0-9]" it)
              -2)
             ((member it vars)
              -1)
             (t (length (npmjs-key-builder-shared-start (downcase word)
                                                        (downcase it))))))
     #'>
     (seq-uniq (delq nil
                     (append
                      vars
                      (npmjs-key-splitted-variants word len "^[a-z]")
                      (mapcar
                       (lambda (n)
                         (funcall finalize (mapconcat
                                            (apply-partially
                                             #'npmjs-key-builder-safe-substring
                                             n)
                                            parts "")))
                       (number-sequence 1 (min len parts-len)))
                      (npmjs-key-splitted-variants word len "")
                      (mapcar
                       (lambda (n)
                         (funcall finalize (mapconcat
                                            (apply-partially
                                             #'npmjs-key-builder-safe-substring
                                             n)
                                            (reverse parts) "")))
                       (number-sequence 1 (min len parts-len)))))))))




(defun npmjs-key-builder--generate-shortcut-key (word key-len shortcuts
                                                      all-keys)
  "Generate a unique shortcut key.

Argument WORD is a string from which the shortcut key is generated.

Argument KEY-LEN is an integer that specifies the maximum length of the
generated shortcut key.

Argument SHORTCUTS is a list of strings representing existing shortcuts to avoid
conflicts.

Argument ALL-KEYS is a list of strings representing all possible keys to ensure
uniqueness."
  (let ((short
         (downcase
          (substring-no-properties word 0
                                   (min key-len
                                        (length word))))))
    (setq short (if (string-match-p short "[a-z]")
                    (replace-regexp-in-string "^[^a-z]+" "" short)
                  short))
    (setq short
          (seq-find
           (lambda (it)
             (not
              (seq-find
               (apply-partially
                #'string-prefix-p it)
               shortcuts)))
           (append
            (npmjs-key-builder-get-all-key-strategies
             word
             key-len)
            (let ((random-variants
                   (npmjs-key-builder-get-alphabet)))
              (or (seq-remove (lambda (key)
                                (seq-find (apply-partially
                                           #'string-prefix-p
                                           (downcase key))
                                          all-keys))
                              random-variants)
                  random-variants)))))
    (while (and
            (< (length short) key-len))
      (setq short (concat (or short "")
                          (number-to-string (random 10)))))
    short))

(defun npmjs-key-builder-generate-shortcuts (items &optional key-fn value-fn
                                                   used-keys key-len)
  "Generate keyboard shortcuts for items.

Argument ITEMS is a list of items to generate shortcuts for.

Optional argument KEY-FN is a function used to extract a key from an item. It
defaults to `npmjs-key-builder-default-key-fn'.

Optional argument VALUE-FN is a function used to create a value from a shortcut
and an item. It defaults to `npmjs-key-builder-default-value-fn'.

Optional argument USED-KEYS is a list of already used keys to avoid conflicts.

Optional argument KEY-LEN is an integer specifying the minimum length of the
generated keys."
  (unless key-fn (setq key-fn #'npmjs-key-builder-default-key-fn))
  (unless value-fn (setq value-fn #'npmjs-key-builder-default-value-fn))
  (let ((min-len
         (or key-len
             (if used-keys
                 (length (car (seq-sort-by #'length #'> used-keys)))
               (let ((variants-len (length (npmjs-key-builder-get-alphabet)))
                     (total (length items)))
                 (cond ((>= variants-len total)
                        1)
                       ((>= variants-len (/ total 2))
                        2)
                       (t 3)))))))
    (let ((shortcuts used-keys)
          (used-words '())
          (all-keys (mapcar
                     (npmjs--compose
                       (npmjs--cond
                         [(npmjs--compose not
                            (apply-partially #'string-match-p "[a-z]"))
                          identity]
                         [t (apply-partially #'replace-regexp-in-string
                                             "^[^a-z]+" "")])
                       (lambda (it)
                         (funcall key-fn it)))
                     items))
          (result))
      (dotimes (i (length items))
        (let ((word (nth i all-keys))
              (def (nth i items)))
          (when-let* ((shortcut
                       (when (not (member word used-words))
                         (npmjs-key-builder--generate-shortcut-key
                          word
                          min-len
                          shortcuts
                          all-keys)))
                      (value (funcall value-fn shortcut def)))
            (setq used-words (push word used-words))
            (setq shortcuts (push shortcut shortcuts))
            (setq result (push value result)))))
      (reverse result))))

(defun npmjs-key-builder-default-key-fn (def)
  "Generate string from symbol or return argument.

Argument DEF is a symbol or a string that represents the key to be built."
  (if (symbolp def)
      (symbol-name def)
    def))

(defun npmjs-key-builder-default-value-fn (key value)
  "Generate list with key and value(s).

Argument KEY is the key associated with the VALUE in the key-value pair.

Argument VALUE is the value associated with the KEY; it can be a proper list or
any other object."
  (if (proper-list-p value)
      (append (list key) value)
    (cons key value)))

(defun npmjs-key-builder-get-alphabet ()
  "Generate list of lowercase, uppercase letters and symbols @ .."
  (append
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


(defun npmjs-key-builder-take-description (item)
  "Extract description from a given list.

Argument ITEM is a proper list representing an item from which to extract the
description."
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
  "Extract the first element if it's a string.

Argument ITEM is a list where the first element is expected to be a string."
  (when (stringp (car-safe item))
    (car item)))

(defun npmjs-make-command-name (&rest args)
  "Convert arguments into a trimmed, space-separated string.

Remaining arguments ARGS are strings that will be concatenated to form the
command name after being unquoted and trimmed."
  (let ((name (mapconcat (npmjs--compose
                           string-trim
                           (apply-partially #'format "%s"))
                         (delq nil
                               (mapcar #'npmjs-unquote
                                       (flatten-list args)))
                         " ")))
    name))

(defun npmjs-make-symbol (&rest args)
  "Generate a unique symbol from concatenated arguments.

Remaining arguments ARGS are strings that will be concatenated and sanitized to
form a symbol name."
  (make-symbol
   (replace-regexp-in-string "[\s\t]+" "-"
                             (npmjs-make-command-name args))))

(defun npmjs-confirm-command (cmd &rest args)
  "Prompt user to confirm npm command before execution.

Argument CMD is the name of the npm command to run.

Remaining arguments ARGS are additional command-line arguments to pass to the
npm command."
  (read-string "Run: "
               (npmjs-make-command-name
                cmd args)))

(defun npmjs-confirm-and-run (cmd &rest args)
  "Confirm global flags and execute npm commands.

Argument CMD is a string representing the npm command to execute.

Remaining arguments ARGS are strings representing additional arguments to pass
to the npm command."
  (if (seq-find
       (apply-partially #'string-match-p
                        (regexp-opt
                         '("--global" "-g")
                         'symbols))
       args)
      (npmjs-compile-global (npmjs-confirm-command cmd args))
    (npmjs-compile (npmjs-confirm-command cmd args))))

(defun npmjs-substitute-hints (str)
  "Replace HTML-like tags in a string.

Argument STR is a string containing the text to be processed."
  (with-temp-buffer
    (insert
     str)
    (goto-char (point-min))
    (while (re-search-forward "[@./=]?\\(<\\([a-z0-9/@-][^>]+\\)[>][\s./=]?\\)+"
                              nil t 1)
      (replace-match ""))
    (buffer-string)))

(defun npmjs-arg-sort-transformer (argument)
  "Sort command-line arguments by prefix type.

ARGUMENT argument is a string or a list where the first element is a string
representing a command-line argument."
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
  "Sort arguments using a custom transformer.

Argument ARGS is a sequence of elements to be sorted."
  (seq-sort-by #'npmjs-arg-sort-transformer #'> args))

(defun npmjs-format-args (args)
  "Format npm arguments by joining lists and substituting hints.

Argument ARGS is a list of arguments to be formatted."
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
  "Retrieve and process arguments for npmjs command."
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
  "Format transient arguments for npmjs."
  (npmjs-format-args (npmjs-get-arguments)))

(defun npmjs-group-vectors (arguments &optional height win-width)
  "Group and sort argument vectors by length.

ARGUMENTS arguments are lists of strings to be processed.

Optional argument HEIGHT is the maximum height of the window in lines.

Optional argument WIN-WIDTH is the desired width of the window in characters."
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
  "Execute npm commands with transient arguments."
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

;;;###autoload (autoload 'npmjs-show-args "npmjs" nil t)
(transient-define-suffix npmjs-show-args ()
  "Display formatted npm command with arguments."
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
  "Retrieve npm configuration as JSON."
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
  "Throw `done' signal with argument or true.

Optional argument ARG is the value to throw with the tag `done'. If not
provided, it defaults to t."
  (interactive)
  (throw 'done (or arg t)))


(defun npmjs-stringify (item)
  "Convert various data types to string representation.

Argument ITEM is the object to be stringified; it can be a string, number,
vector, list, cons cell, symbol, or any other type."
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

(defun npmjs-make-alist-annotated-completion-table (collection)
  "Generate annotated completion table from collection.

Argument COLLECTION is a list or vector to be converted into an alist for
completion."
  (let* ((alist (npmjs-stringify collection))
         (longest (+ 5
                     (apply #'max (mapcar
                                   (npmjs--compose
                                    (npmjs--cond
                                     [stringp length]
                                     [t (npmjs--const 0)])
                                    car-safe)
                                   alist))))
         (fmt (concat (propertize " " 'display
                                  `(space
                                    :align-to
                                    ,longest))
                      " "))
         (annotate-fn (lambda (k)
                        (if-let ((cell (assoc-string k alist)))
                            (if (stringp cell) ""
                              (concat fmt (truncate-string-to-width
                                           (format
                                            "%s"
                                            (cdr cell))
                                           60
                                           nil nil "...")))
                          ""))))
    (lambda (&optional str pred action)
      (if (eq action 'metadata)
          `(metadata
            (annotation-function . ,annotate-fn))
        (complete-with-action action alist str
                              pred)))))

(defun npmjs-single-completing-read-annotated (&optional prompt collection
                                                         predicate require-match
                                                         initial-input hist def
                                                         inherit-input-method)
  "Display annotated completion options for selection.

Optional argument PROMPT is the string to display as the prompt in the
minibuffer.

Optional argument COLLECTION is a list of items or a function to dynamically
generate them.

Optional argument PREDICATE is a function to filter the items in COLLECTION.

Optional argument REQUIRE-MATCH determines if the user must select an existing
item.

Optional argument INITIAL-INPUT is the initial input in the minibuffer.

Optional argument HIST is the history list to use for the minibuffer input.

Optional argument DEF is the default value to return if the user enters an empty
string.

Optional argument INHERIT-INPUT-METHOD specifies whether to inherit the current
input method."
  (let ((table-completion
         (if (functionp collection)
             collection
           (npmjs-make-alist-annotated-completion-table collection))))
    (completing-read
     (concat (string-trim (or prompt "Item"))" \s")
     table-completion
     predicate
     require-match
     initial-input hist def inherit-input-method)))

(defun npmjs-multi-completing-read-annotated (&optional prompt collection
                                                        initial-input hist)
  "Choose multiple items with annotations.

Optional argument PROMPT is a string to display as the prompt in the minibuffer.

Optional argument COLLECTION can be a list of strings, an alist, or a function
that generates completions.

Optional argument INITIAL-INPUT is a string to prefill the minibuffer with.

Optional argument HIST is a symbol representing a history list to use for
completion."
  (let ((table-completion
         (if (functionp collection)
             collection
           (npmjs-make-alist-annotated-completion-table collection)))
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
\\[npmjs-throw-done]'to finish)\s")
                                           (if choices
                                               (concat
                                                "("
                                                (string-join
                                                 choices
                                                 ", ")
                                                ")")
                                             ""))
                                   table-completion
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
  "Fetch user input with completion for npmjs.

Argument FN is a function to call with the minibuffer input.

Optional argument PROMPT is a string to display as the prompt in the minibuffer.

Optional argument INITIAL-INPUT is a string to prefill the minibuffer with.

Optional argument HISTORY is a symbol representing a minibuffer history list
variable or a HISTORY list itself."
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
  "Fetch multiple inputs with custom completion.

Argument FN is a function to call for each minibuffer input.

Optional argument PROMPT is a string to display as the prompt in the minibuffer.

Optional argument INITIAL-INPUT is a string to prefill the minibuffer with.

Optional argument HISTORY is a symbol representing a minibuffer history list."
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
\\[npmjs-throw-done]'to finish): "))
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

(defun npmjs-make-reader (fn &optional collection multi)
  "Create a reader function for npmjs data.

Argument FN is a function to be partially applied to create a reader.

Optional argument COLLECTION is a list or a function returning a list to be used
for completion.

Optional argument MULTI is a boolean indicating whether to create a
multi-selection reader."
  (cond ((and fn multi)
         (apply-partially #'npmjs-multi-reader fn))
        ((and fn (not multi))
         (apply-partially #'npmjs-single-reader fn))
        ((and collection multi)
         (lambda (&optional prompt initial-input hist)
           (npmjs-multi-completing-read-annotated prompt
                                                  (if (functionp collection)
                                                      (funcall collection)
                                                    collection)
                                                  initial-input
                                                  hist)))
        ((and collection (not multi))
         (lambda (&optional prompt initial-input hist)
           (npmjs-single-completing-read-annotated
            prompt
            (if (functionp collection)
                (funcall collection)
              collection)
            initial-input hist)))
        (t fn)))


(defun npmjs-make-combined-package-reader (reader transformer &optional
                                                  multi-value)
  "Combine package READER with TRANSFORMER function.

Argument READER is a function that reads package names.

Argument TRANSFORMER is a function that transforms the package names read by
READER.

Optional argument MULTI-VALUE is a boolean indicating whether multiple values
can be read; it defaults to nil.

Optional argument PROMPT is a string used as the prompt when reading input.

Optional argument INITIAL-INPUT is a string or list of strings used as the
initial input when reading.

Optional argument HIST is a symbol representing a history list or a cons cell
\(HISTVAR . HISTPOS)."
  (lambda (&optional prompt initial-input hist)
    (let ((deps (funcall (npmjs-make-reader reader nil multi-value)
                         prompt initial-input hist)))
      (if (stringp deps)
          (funcall transformer deps)
        (mapcar transformer deps)))))

(defun npmjs-make-npm-package-reader-with-version (&optional multi-value)
  "Create reader for npm package versions.

Optional argument MULTI-VALUE is a boolean indicating whether to allow multiple
values; it defaults to nil."
  (npmjs-make-combined-package-reader #'npmjs-read-new-dependency
                                      #'npmjs-confirm-package-version
                                      multi-value))

(defun npmjs-make-npm-package-reader-with-tag (&optional multi-value)
  "Create a reader for npm packages with tags.

Optional argument MULTI-VALUE is a boolean indicating whether to return multiple
values; it defaults to nil."
  (npmjs-make-combined-package-reader #'npmjs-read-new-dependency
                                      #'npmjs-confirm-package-dist-tags
                                      multi-value))

(defun npmjs-npm-read-config-key-value (&optional prompt initial-input hist)
  "Retrieve and set npm config key values.

Optional argument PROMPT is the string to display as the prompt when asking the
user for input. It defaults to \"Key: \".

Optional argument INITIAL-INPUT is a string to prefill the minibuffer for the
user's input.

Optional argument HIST is the history list to use for the minibuffer input."
  (let* ((config (mapcar
                  (npmjs--converge cons
                                   [(npmjs--compose
                                      substring-no-properties
                                      symbol-name
                                      car)
                                    cdr])
                  (npmjs-get-npm-config)))
         (choice (npmjs-single-completing-read-annotated
                  (or prompt "Key: ")
                  (npmjs-get-npm-config)
                  nil nil initial-input
                  hist))
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
  "Generate completion table for npm CONFIG values.

Optional argument CONFIG is an alist where each element is a cons cell (KEY .
VALUE) representing npm configuration options. If not provided, the function
retrieves the configuration using `npmjs-get-npm-config'."
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
  "Retrieve global or local npm package dependencies table."
  (let* ((args
          (npmjs-get-arguments))
         (global-p (seq-intersection args '("--global" "-g"))))
    (if
        (or global-p (not (npmjs-get-project-root)))
        (npmjs-global-package-completion-table)
      (npmjs-local-package-completion-table))))

(defun npmjs-url-reader (&optional prompt initial-input hist)
  "Retrieve URLs from `kill-ring' and npm config.

Optional argument PROMPT is a string to prompt the user.

Optional argument INITIAL-INPUT is a string to insert before reading input.

Optional argument HIST is a symbol representing a history list variable or a
cons cell (HISTVAR . HISTPOS)."
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
  "Convert FILENAME to path relative to project root or default directory.

Argument FILENAME is the name of the file for which to compute the relative
path."
  (if-let ((proj-root (npmjs-get-project-root)))
      (file-relative-name filename proj-root)
    (file-relative-name filename default-directory)))

(defun npmjs-read-pkg-directory (&optional prompt &rest _)
  "Read and return relative path of selected directory.

Optional argument PROMPT is the message displayed when asking the user to choose
a directory. It defaults to \"Directory file: \".

Remaining arguments _ are ignored and have no effect on the function's
behavior."
  (npmjs-file-name-relative
   (file-local-name
    (read-directory-name (or prompt "Directory file: ")))))

(defun npmjs-read-tar-file (&optional prompt &rest _)
  "Extract tarball file name with optional prompt.

Optional argument PROMPT is the string used to prompt the user for a file name.
If not provided, the default PROMPT \"Tarball file: \" is used.

Remaining arguments _ are ignored and not used within the function."
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
  "Extract scoped package names from a string.

Argument STR is a string to be searched for scoped package names."
  (let ((found))
    (with-temp-buffer
      (insert str)
      (while (re-search-backward "^@[a-z0-9_-]+" nil t 1)
        (push (match-string-no-properties 0) found)))
    found))

(defun npmjs-guess-scopes ()
  "Extract unique npm scopes from various sources."
  (seq-uniq
   (flatten-list
    (mapcar
     (npmjs--compose
      npmjs-get-scope-matches
      substring-no-properties)
     (seq-filter
      (apply-partially #'string-match-p "^@[a-z0-9_-]+")
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
                 (npmjs-get-npm-config))))))))))

(defvar npmjs-all-known-hints nil)
(defvar npmjs-all-args nil)
(defvar npmjs-all-args-alist nil)

(defun npmjs-get-hint-reader (str &optional cmd multi-value)
  "Parse hints for npm commands and create readers.

Argument STR is a string that represents the hint to be processed.

Optional argument CMD is a string that specifies the command context in which
the hint is used.

Optional argument MULTI-VALUE is a boolean indicating whether multiple values
are allowed; it defaults to nil."
  (add-to-list 'npmjs-all-known-hints str)
  (pcase str
    ("<command>"
     (pcase cmd
       ("run-script" (npmjs-make-reader
                      #'npmjs-read-script
                      nil
                      multi-value))))
    ((or "<registry>" "<url>")
     (npmjs-make-reader
      #'npmjs-url-reader nil multi-value))
    ((or "<@orgname>" "<@scope>" "<scope>")
     (npmjs-make-reader nil #'npmjs-guess-scopes multi-value))
    ("--registry="
     (npmjs-make-reader
      #'npmjs-url-reader
      nil
      multi-value))
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
        (npmjs-make-reader #'npmjs-read-new-dependency nil multi-value))
       ((or "update" "uninstall" "edit" "ls")
        (npmjs-make-reader nil #'npmjs-pkg-get-dependencies-table multi-value))
       ("init" 'read-string)
       (_ (npmjs-make-reader #'npmjs-read-new-dependency nil multi-value))))
    ((or "<@scope><pkg>@<tag>"
         "<pkg>@<tag>"
         "<package>@<tag>"
         "<name>@<tag>")
     (pcase cmd
       ((or "update" "uninstall" "edit" "ls")
        (npmjs-make-reader nil #'npmjs-pkg-get-dependencies-table multi-value))
       ("init" 'read-string)
       (_ (npmjs-make-npm-package-reader-with-tag multi-value))))
    ((or "<@scope><name>@<version>"
         "<pkg>@<version>"
         "<@scope>/<name>"
         "<@scope><pkg>@<version>"
         "<@scope><pkg>@<version range>")
     (pcase cmd
       ((or "update" "uninstall" "edit" "ls")
        (npmjs-make-reader nil #'npmjs-pkg-get-dependencies-table multi-value))
       ("init" 'read-string)
       (_ (npmjs-make-npm-package-reader-with-version multi-value))))
    ("<key>"
     (pcase (car (split-string cmd " " t))
       ((or "get" "config" "set")
        (npmjs-make-reader nil
                           #'npmjs-npm-config-completion-table multi-value))))
    ((or "<tarball>"
         "<tarball file>")
     (npmjs-make-reader #'npmjs-read-tar-file nil multi-value))
    ("<file>"
     (npmjs-make-reader #'transient-read-file nil multi-value))
    ("<depth>"
     (npmjs-make-reader #'transient-read-number-N0 nil multi-value))
    ("<call>" (npmjs-make-reader (npmjs--compose
                                   (npmjs--cond
                                     [string-empty-p identity]
                                     [(npmjs--and (apply-partially
                                                   #'string-prefix-p "'")
                                                  (apply-partially
                                                   #'string-suffix-p "'"))
                                      identity]
                                     [t (apply-partially #'format "'%s'")])
                                   (apply-partially #'read-string))
                                 nil multi-value))
    ((or "<folder>")
     (npmjs-make-reader #'transient-read-existing-directory nil multi-value))
    ((or "<key>=<value>" "<key>=")
     (pcase (car (split-string cmd " " t))
       ((or "config" "set" "get")
        (npmjs-make-reader #'npmjs-npm-read-config-key-value nil
                           multi-value))))))

(defun npmjs-parse-hint (str &optional cmd parsed-props)
  "Parse hints for npm command arguments.

Argument STR is a string representing the hint to be parsed.

Optional argument CMD is a string representing the command associated with the
hint.

Optional argument PARSED-PROPS is a property list containing already parsed
properties."
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
  "Strip angle brackets from a string.

Argument STR is the string from which angle brackets should be removed."
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
  "Split a string based on various regex patterns.

Argument JSREGEX is a string representing a JavaScript regular expression."
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
  "Extract the value following a KEYWORD in a list.

Argument KEYWORD is the symbol to search for in ARGS.

Argument ARGS is a list where KEYWORD and its associated value are expected to
be found."
  (when (listp args)
    (when-let ((class (memq keyword args)))
      (cadr class))))

(defun npmjs-eval-infix (name &optional args inhibit-eval)
  "Evaluate or define transient infix for npmjs commands.

Argument NAME is a symbol representing the name of the infix command.

Optional argument ARGS is a list of arguments for the infix command.

Optional argument INHIBIT-EVAL is a boolean; when non-nil, the infix command is
not evaluated."
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
  "Check if string starts with \"<\", ends with \">\", and lacks \"|\".

Argument STR is a string to be checked for certain prefix and suffix."
  (and (string-prefix-p "<" str)
       (string-suffix-p ">" str)
       (not (string-match-p "|" str))))

(defun npmjs-plist-merge (plist-a plist-b)
  "Merge property lists, prioritizing the second list's values.

Argument PLIST-A is a property list to be merged.

Argument PLIST-B is another property list whose properties will be merged into
PLIST-A."
  (let ((props (seq-copy plist-a)))
    (dotimes (idx (length plist-b))
      (when (eq (logand idx 1) 0)
        (let ((prop-name (nth idx plist-b)))
          (let ((val (plist-get plist-b prop-name)))
            (setq props (plist-put props prop-name val))))))
    props))

(defun npmjs-nth (n list-or-vector)
  "Retrieve the nth element from a list or vector.

Argument N is an integer representing the index of the element to retrieve.

Argument LIST-OR-VECTOR is either a list or a vector from which the element at
index N will be retrieved."
  (when (> (length list-or-vector) n)
    (pcase list-or-vector
      ((pred vectorp)
       (aref list-or-vector n))
      ((pred proper-list-p)
       (nth n list-or-vector)))))

(defun npmjs-parse-no-argument-p (argument)
  "Check if ARGUMENT starts with \"--no-\".

ARGUMENT argument is a string to be checked for the presence of \"--no-\"."
  (string-match-p "--no-" argument))

(defun npmjs-normalize-description (str)
  "Trim and remove leading dashes from a string.

Argument STR is the string to normalize by trimming whitespace and leading
hyphens."
  (let ((res (replace-regexp-in-string "^[-]+" "" (string-trim str))))
    (if (string-empty-p res)
        str
      res)))

(defun npmjs-extract-all-hints (str)
  "Extract HTML tags from a string into a list.

Argument STR is a string from which to extract hints."
  (let ((founds))
    (with-temp-buffer (insert str)
                      (while (re-search-backward "<\\([^>]+\\)>"
                                                 nil
                                                 t
                                                 1)
                        (push (match-string-no-properties 0) founds)))
    founds))

(defun npmjs-parse-argument (vect &optional cmd)
  "Parse and transform command-line arguments.

Argument VECT is a vector that contains the command-line arguments to parse.

Optional argument CMD is the command to which the arguments apply.

Return an updated version of collection COLL with the KEY removed."
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
                   (string-match-p "^[a-z0-9-]+=\\([a-z0-9-]+|[a-z0-9|-]+\\)$"
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
  "Display parsed help for all npm arguments."
  (interactive)
  (npmjs-pp (mapcar (lambda (it)
                      (cons it
                            (list (npmjs-parse-help--vector it "" t))))
                    (seq-copy npmjs-all-args))))

(defun npmjs-make-infix-switch-name (choices)
  "Generate normalized switch name from choices.

Argument CHOICES is a list of strings representing command-line switch
descriptions."
  (or (car (car (npmjs-group-with (lambda (it)
                                    (car (split-string it "[-=]" nil)))
                                  (mapcar #'npmjs-normalize-description
                                          (seq-remove
                                           (apply-partially #'string-match-p
                                                            "^-[a-z]\\|--no-")
                                           choices)))))
      (npmjs-normalize-description (car choices))))

(defun npmjs-nested-args-p (args)
  "Check for nested arguments in a list.

Argument ARGS is a list to check for nested arguments."
  (and args
       (car args)
       (car-safe (car args))))

(defun npmjs-maybe-eval (cmd inhibit-eval args)
  "Evaluate command with dynamic argument formatting.

Argument CMD is a symbol representing the command to be executed.

Argument INHIBIT-EVAL is a boolean value; when non-nil, it prevents evaluation.

Arguments ARGS is a list of arguments and keyword-value pairs to be processed."
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
  "Parse and optionally evaluate a vector of npm arguments.

Argument VECT is a vector to be parsed.

Optional argument CMD is a command associated with the vector.

Optional argument INHIBIT-EVAL is a boolean; when non-nil, evaluation of the
vector is inhibited."
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
  "Extract text between angle brackets near point.

Return the text without any text properties."
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
  "Extract tokens from buffer until stop character.

Optional argument STOP-CHAR is a character or list of characters that will stop
the tokenization process.

Return a list of tokens parsed from the buffer at the current point, stopping at
an optional STOP-CHAR or at the end of the buffer."
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
  "List installed npm versions and write outputs."
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
  "Retrieve npm versions and descriptions."
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
  "Print descriptions of all npm packages.

Optional argument FORCE is a prefix argument; when non-nil, it forces the
refresh of the package descriptions regardless of whether they are already
available."
  (interactive "P")
  (unless (and npmjs-all-descriptions-alist
               (not force))
    (npmjs--get-all-npm-versions-descriptions-alist))
  (npmjs-pp npmjs-all-descriptions-alist))

(defun npmjs-upcased-p (str)
  "Check if string starts with an uppercase letter.

Argument STR is the string to be checked for an uppercase start."
  (let ((case-fold-search nil))
    (string-match-p "^[A-Z]" str)))

(defun npmjs-get-list-item-if-one (item)
  "Retrieve single element from list or vector, else return item.

Argument ITEM is the list or vector to be processed; it should contain exactly
one element to return that element, otherwise ITEM is returned as is."
  (cond ((and (proper-list-p item)
              (= 1 (length item)))
         (car item))
        ((and (vectorp item)
              (= 1 (length item)))
         (npmjs-nth
          0 item))
        (t item)))

(defun npmjs-ensure-option-ending (long)
  "Ensure option ends with space or equals.

Argument LONG is a string representing the command-line option to ensure it ends
with a space.

Ensure the returned string LONG ends with a space unless it already ends with
\"=\", \"--\", or a space."
  (if (or (string-suffix-p "=" long)
          (string= "--" long)
          (string-suffix-p " " long))
      long
    (concat long " ")))

(defun npmjs-short-long-option-p (str)
  "Check if string matches short and long option pattern.

Argument STR is a string to be matched against the pattern for short and long
options."
  (string-match-p "^\\(-\\([a-z]+\\)\\)[|]\\(--\\([a-z0-9-]+\\)\\)$"
                  str))

(defun npmjs-get-long-short-option (str)
  "Extract and reverse long and short options from a string.

Argument STR is a string containing the option to be parsed, expected to have a
long and short version separated by a pipe character \"|\"."
  (if (npmjs-short-long-option-p str)
      (reverse (split-string str "|" t))
    nil))

(defun npmjs-flatten-vectors (item)
  "Flatten nested vectors into a single vector.

Argument ITEM is a vector that will be flattened."
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
  "Extract nested vector if first elements match, else return original.

Argument VECT is a vector that may contain subvectors."
  (or
   (when-let ((subvect (and (vectorp vect)
                            (seq-find #'vectorp vect))))
     (when (equal (npmjs-nth 0 subvect)
                  (npmjs-nth 0 vect))
       subvect))
   vect))

(defun npmjs-normalize-vectors (vect)
  "Normalize and flatten vector data.

Argument VECT is a vector to be normalized.

Return a normalized vector by flattening and concatenating string elements,
with special handling of the \"|\" character."
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
  "Generate documentation for npm commands.

Argument CMD is a string representing the npm command to include in the
documentation.

Argument LINES is a list of strings, each representing a line of text to be
filtered and potentially included in the documentation."
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
  "Map descriptions to command cells.

Argument ALIST is a list of cons cells, where each cell contains a command and
its description lines.

Optional argument INHIBIT-EVAL is a boolean that, when non-nil, prevents
evaluation of infix expressions within the command descriptions."
  (mapcar (lambda (it)
            (npmjs-map-command-cell-lines it inhibit-eval))
          alist))

(defun npmjs-single-value-to-hint (str)
  "Extract and format key-value pairs from a string.

Argument STR is a string containing the text to process."
  (with-temp-buffer
    (insert str)
    (while (re-search-backward
            "--\\([a-z0-9@-]+\\)=\\([a-z0-9@-]+\\)" nil t 1)
      (let ((value (match-string-no-properties 2)))
        (replace-match (concat "<" value ">") nil nil nil 2)))
    (buffer-string)))

(defun npmjs-combine-matched-tags-in-string (curr)
  "Combine adjacent HTML tags in a string.

Argument CURR is a string to be processed for matching tags."
  (let ((re "\\[?\\(<[a-z][^>]+>\\) ?\\[?\\(<[a-z][^>]+>\\)\\]?"))
    (when (string-match-p re curr)
      (setq curr (with-temp-buffer (insert curr)
                                   (goto-char (point-min))
                                   (when (re-search-forward re
                                                            nil t 1)
                                     (let ((a (match-string-no-properties 1))
                                           (b (match-string-no-properties 2)))
                                       (replace-match (concat a " " b))))
                                   (buffer-string))))))

(defun npmjs-prenormalize-line (curr)
  "Remove specific patterns and normalize text in a line.

Argument CURR is a string to be processed."
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
  (npmjs-combine-matched-tags-in-string curr)
  (npmjs-single-value-to-hint curr))

(defun npmjs-parse-normalized-vectors (cmd inhibit-eval vectors)
  "Parse and flatten nested vectors.

Argument CMD is a command used for evaluated infix.

Argument INHIBIT-EVAL is a boolean value; when non-nil, it prevents evaluation
of infixes.

Argument VECTORS is a list of vectors to be parsed."
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
  "Parse and normalize npm command options.

Argument RAWOPTIONS is a list of raw options to be parsed.

Return a list of vectors representing parsed subcommands and options. Each
vector in the list contains elements that represent a command or an option and
its associated values or flags.

The function processes a sequence of raw options, normalizing and organizing
them into a structured format.

Strings, symbols, and nested vectors are handled according to specific rules to
ensure that related options and their values are grouped together correctly.

The result is a list of vectors where each vector corresponds to a command or
option with its arguments, if any. The list is in reverse order of the original
input sequence."
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
  "Parse and map npm command details from text.

Argument CELL is a cons cell where the car is a command and the cdr is a list of
lines describing the command.

Optional argument INHIBIT-EVAL is a boolean that, when non-nil, prevents
evaluation of infix expressions within the command description.

The function processes a CELL containing a command and its associated lines of
description, parsing and organizing the information into a structured result.

The result includes details such as subcommands, options, aliases, and
descriptions. It handles various cases and patterns found in the command's help
text, such as common options, usage patterns, and specific command structures.
The function also supports optional evaluation control, allowing the caller to
inhibit the evaluation of certain expressions if needed.

The structured result is suitable for further processing or display,
providing a comprehensive overview of the command's capabilities and usage."
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
  "Retrieve npm COMMAND description using current Node version.

Argument COMMAND is a string representing the npm command for which to get the
description.

Return an association list mapping npm COMMAND names to their descriptions."
  (assoc-string command
                (npmjs-nvm-with-current-node-version
                 (npmjs-parse-help-with-output
                  (shell-command-to-string "npm -l")
                  (npmjs-parse-columns-to-alist)))))

(defun npmjs-get-command-spec (command &optional inhibit-eval)
  "Retrieve COMMAND specification from npmjs description.

Argument COMMAND is a string representing the npm command to get the
specification for.

Optional argument INHIBIT-EVAL is a boolean; when non-nil, it prevents
evaluation of the command."
  (npmjs-map-command-cell-lines (cons
                                 command
                                 (npmjs-get-description-spec command))
                                inhibit-eval))

(defun npmjs-read-line (line)
  "Parse and tokenize a given string line.

Argument LINE is a string that will be tokenized if it is a string type."
  (if (stringp line)
      (with-temp-buffer
        (insert line)
        (goto-char (point-min))
        (npmjs-tokenize))
    line))

(defun npmjs-group-with (fn items &optional transform-fn)
  "Group ITEMS by a function, optionally transforming them.

Argument FN is a function that takes an item from ITEMS and returns a key to
group by.

Argument ITEMS is a list of elements to be grouped.

Optional argument TRANSFORM-FN is a function applied to each item before
grouping; if nil, the item is used as is."
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
  "Retrieve and display npm command descriptions.

Argument COLUMN is the column number to start searching from."
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
  "Return a list of descriptions parsed from columns in npmjs buffer."
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
  "Eval and call transient prefix with NAME and BODY.

NAME is the name of the prefix command to define. It should be a symbol.

BODY is a list of arguments for `transient-define-prefix'.

It set the default value of the command's arguments using a lambda function.
If `npmjs-get-project-root' returns nil, it sets the default value to
--global argument.

It also sets a help function for the prefix command. The help function
calls `npmjs-show-manual' with the value of the command's
`man-page' property retrieved from NAME."
  (interactive)
  (eval `(progn (transient-define-prefix ,name ()
                  :value (lambda ()
                           (unless (npmjs-get-project-root)
                            (list "--global")))
                  :show-help (lambda (prefix)
                               (when-let ((man-page (oref prefix command)))
                                (npmjs-show-manual (get (oref prefix command)
                                                    'man-page))))
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
    ("C-c C-a" "Show arguments" npmjs-show-args))
  "Key bindings and descriptions for npmjs command options.")

(defun npmjs-map-commands (commands)
  "Recoursively process a list of COMMANDS and generate key bindings for them.

COMMANDS is a nested list of property lists with such props:
:cmd - a string, the command name e.g. \"install\", \"access list\".
:key - a string, which will be used for generating key.
:options - alist of descriptions (string) and its arguments.
:subcommands - same as COMMANDS.

The function first checks if the COMMANDS list contains subcommands specified
as keyword arguments. If so, it generates shortcuts for the subcommands using
the `npmjs-key-builder-generate-shortcuts' function. It then recursively calls
`npmjs-map-commands' on each subcommand and updates the COMMANDS list
accordingly.

Next, the function checks if the COMMANDS list contains options specified as
keyword arguments. It adds shortcuts for the options using the
`npmjs-add-options-shortcuts' function.

If additional options are defined in `npmjs-extra-arguments',
they are appended to the options list.

The function then attaches common suffixes from `npmjs-options-suffixes'.

It finally calls `npmjs-eval-symb' to evaluate the command and
retrieve its associated symbol.

If the COMMANDS list matches a command in `npmjs-map-replace-commands', it
replaces the command with its corresponding name and key.

If none of the above conditions match, the function assumes the COMMANDS list
contains a standalone command.

It generates a symbol for the command using `npmjs-make-symbol' with the
property `npm-command'.

If additional properties are defined in `npmjs-commands-props' for the command,
they are appended to the result.

The function returns the modified COMMANDS list after processing."
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
  :reader (npmjs-make-reader #'npmjs-read-new-dependency nil t))

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
  "Prompt for a tarball file path."
  :class 'transient-option
  :argument "<tarball>"
  :prompt "Tarball file:"
  :reader #'npmjs-read-tar-file)

(transient-define-argument npmjs-install-pkg-directory ()
  "Set npm install target directory."
  :class 'transient-option
  :argument "<directory>"
  :prompt "Directory:"
  :reader #'transient-read-existing-directory)

(transient-define-argument npmjs-file-argument ()
  "Prompt for a file relative to project root."
  :class 'transient-option
  :argument "<file>"
  :prompt "File:"
  :reader #'npmjs-read-file-name)


(defun npmjs-read-file-name (&rest args)
  "Prompt for a file name and return its path relative to project root.

Remaining arguments ARGS are strings or"
  (let ((file (apply #'transient-read-existing-file args)))
    (npmjs-file-name-relative file)))

(defun npmjs-read-project-file (&optional prompt initial-input hist)
  "PROMPT for a project file with completion.

Optional argument PROMPT is the string to display as the prompt when asking the
user for input. It defaults to \"Project file: \".

Optional argument INITIAL-INPUT is a string to insert before reading input from
the user.

Optional argument HIST is the history list to use for the input."
  (require 'project)
  (npmjs-file-name-relative
   (completing-read (or prompt "Project file: ")
                    (ignore-errors
                      (when (fboundp 'project-files)
                        (project-files (project-current))))
                    nil
                    nil
                    initial-input
                    hist)))

(transient-define-argument npmjs-project-file-argument ()
  "Argument for installing tarbal file."
  :class 'transient-option
  :argument "<project-file>"
  :prompt "Project file:"
  :reader #'npmjs-read-project-file)

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
  "Install dependencies for current project."
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


;;;###autoload (autoload 'npmjs-install "npmjs" nil t)
(transient-define-prefix npmjs-install ()
  "Install Node.js packages using npm."
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
             (options
              (append
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
               (list '("g" "global" ("-g" "--global")
                       :show-help (lambda (&rest _)
                                    (with-selected-window (npmjs-show-manual
                                                           "npm-install")
                                     (goto-char (point-min))
                                     (re-search-forward
                                      "^[\s]+\\_<\\(global\\)\\_>"
                                      nil t 1))))))))
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
                       (if npmjs-use-comint-in-scripts
                           (npmjs-confirm-and-run
                            "npm"
                            "run-script"
                            ,script
                            (npmjs-get-formatted-transient-args))
                         (npmjs-run-compile
                          (npmjs-confirm-command
                           "npm"
                           "run-script"
                           ,script
                           (npmjs-get-formatted-transient-args)))))))
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

(defvar-local npmjs-current-scripts nil)


;;;###autoload (autoload 'npmjs-run-script "npmjs" nil t)
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
      npmjs-current-scripts))]
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
                       (plist-get props :options)
                       (mapcar #'car npmjs-current-scripts)))
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
  (npmjs-get-scripts-suffixes)
  (setq npmjs-current-scripts (npmjs-get-scripts-suffixes))
  (transient-setup #'npmjs-run-script))

(put 'npmjs-run-script 'npm-description "Install packages")
(put 'npmjs-run-script 'npm-command "run-script")

(defvar npmjs-evaluated-commands (make-hash-table :test 'equal)
  "Hash table of npm versions and corresponding evaluated prefix commands.")

(defvar npmjs-jest-commands (make-hash-table :test 'equal)
  "Hash table of npm versions and corresponding evaluated jest prefix commands.")

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
  "Set up npmjs with the current npm version and descriptions.

If descriptions for the current version are found, set them as the current
descriptions. Otherwise, set the current descriptions using the prefix spec and
update the descriptions alist accordingly. Return the current npm version."
  (let ((npm-version (npmjs-get-npm-version)))
    (if-let ((descriptions (cdr (assoc-string npm-version
                                              npmjs-descriptions-alist))))
        (setq npmjs-current-descriptions-alist descriptions)
      (setq npmjs-current-descriptions-alist (npmjs-get-prefix-spec))
      (setq npmjs-descriptions-alist (push
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
                                     npmjs-show-manual)
                                    ("-j" "Run jest"
                                     npmjs-jest :inapt-if-not
                                     npmjs-jest-version)))))]))
                         sym)))
                 (puthash npm-version prefix-symb
                          npmjs-evaluated-commands)
                 prefix-symb))))
     (transient-setup command))))


;;;###autoload (autoload 'npmjs-nvm-install-menu "npmjs" nil t)
(transient-define-prefix npmjs-nvm-install-menu ()
  "Command dispatcher."
  :value (lambda ()
           (when npmjs--current-node-version
             (list (concat "--reinstall-packages-from="
                           npmjs--current-node-version))))
  [:description (lambda ()
                  (if npmjs-node-installing
                      "Installing..."
                    "Install Arguments"))
   ("-n" "skip the default-packages file if it exists" "--skip-default-packages")
   ("-l" "Upgrade to then latest npm" "--latest-npm")
   ("d" "Upgrade to the latest working npm on the given node version"
    "--default")
   ("r" "reinstall-packages-from" "--reinstall-packages-from="
    :class
    transient-option
    :choices (lambda ()
               (mapcar #'car (npmjs-nvm--installed-versions))))
   ("l" "select from LTS" "--lts")
   ("-s" "install from source only" "-s")
   ("-b" "install from binary only" "-b")]
  ["Actions"
   ("C-c C-a" "Show arguments" npmjs-show-args)
   ("RET" "Run" npmjs-nvm-install-node-version)])


;;;###autoload (autoload 'npmjs-nvm "npmjs" nil t)
(transient-define-prefix npmjs-nvm ()
  "Menu for Node Version Manager (nvm) commands."
  [:description (lambda ()
                  (if npmjs-installing-nvm
                      "Node Version Manager (nvm) installing..."
                    "Node Version Manager (nvm)"))
   ["Node"
    ("I" npmjs-nvm-install-menu
     :description (lambda ()
                    (if npmjs-node-installing
                        "Installing..."
                      "Install new node version"))
     :inapt-if-not npmjs-nvm-path
     :transient t)
    ("u" npmjs-nvm-use-other-node-version
     :inapt-if-not npmjs-nvm-path
     :description (lambda ()
                    (concat "Use other node version ("
                            (propertize
                             (substring-no-properties (or
                                                       npmjs--current-node-version
                                                       (when (executable-find
                                                              "node")
                                                         (string-trim
                                                          (shell-command-to-string
                                                           "node -v")))
                                                       "None"))
                             'face
                             'transient-value)
                            ")")))
    ("U"
     npmjs-nvm-use-other-node-version-node-globally
     :inapt-if-not npmjs-nvm-path
     :description (lambda ()
                    (concat "Use other node version globally ("
                            (propertize
                             (substring-no-properties (or (with-temp-buffer
                                                            (when (executable-find
                                                                   "node")
                                                              (string-trim
                                                               (shell-command-to-string
                                                                "node -v"))))
                                                          "none"))
                             'face
                             'transient-value)
                            ")")))
    ("D" "uninstall node" npmjs-nvm-uninstall-node)
    ("f" "Jump to installed node"
     npmjs-nvm-jump-to-installed-node :inapt-if-not npmjs-nvm-path)
    ("d" "set default node version " npmjs-nvm-alias :inapt-if-not
     npmjs-nvm-path)
    ("w" npmjs-write-nvm-file
     :description (lambda ()
                    (concat
                     "Write current node version to .nvmrc "
                     (if-let ((dir (or
                                    (locate-dominating-file default-directory
                                                            ".nvmrc")
                                    (car (npmjs-get-project-roots)))))
                         (concat "in " (abbreviate-file-name (expand-file-name
                                                              ".nvrmc" dir)))
                       "")))
     :inapt-if-not (lambda ()
                     (or
                      (locate-dominating-file
                       default-directory ".nvmrc")
                      (car (npmjs-get-project-roots)))))]
   [("N" npmjs-install-nvm :description (lambda ()
                                          (if (npmjs-nvm-path)
                                              "Update nvm"
                                            "Install nvm")))]])

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
             (?c (list (npmjs-map-commands (npmjs-map-descriptions-lines alist
                                                                         t))))
             (?C
              (let ((plists (npmjs-map-descriptions-lines alist t)))
                (list plists (npmjs-map-commands plists))))
             (?!
              (let* ((plists (npmjs-map-descriptions-lines alist t))
                     (cmds (npmjs-map-commands plists)))
                (list plists cmds alist))))))
     (apply #'npmjs-pp (append (list description) results)))))

(defun npmjs--parse-jest-output ()
  "Parse Jest test output into a structured list."
  (let ((result))
    (while (re-search-forward
            "^[\s]*\\(\\(-\\([a-z]\\)\\)\\(,[\s\t]?\\)\\)?\\([-]\\{2\\}\\([a-z0-9-]+\\)\\)[\s\t]+\\([a-z][^\\[]+\\(\\[\\([^\\[]+\\)\\]\\)\\)"
            nil t)
      (let ((short (match-string-no-properties 2))
            (arg (match-string-no-properties 5))
            (descr (match-string-no-properties 7))
            (type (match-string-no-properties 8))
            (parts))
        (setq descr (replace-regexp-in-string (concat (regexp-quote type) "$") "" descr))
        (setq parts (mapcar (lambda (it) (and it (string-join
                                                  (split-string it "[\s\t\n]" t) " ")))
                            (list arg type descr short)))
        (push parts result)))
    (nreverse result)))

(defun npmjs-jest-hint-props (value)
  "Generate hints for Jest command-line options.

Argument VALUE is a list where the first element is a command-line argument, the
second is its type, and the third is its description."
  (let* ((key (pop value))
         (type (cadr value))
         (arg (car value))
         (short-descr (replace-regexp-in-string
                       "^[-][-]" ""
                       (car value)))
         (pl
          (pcase arg
            ((or "--testPathPattern"
                 "--testPathIgnorePatterns")
             (list key short-descr
                   (concat arg " ")
                   :class 'transient-option
                   :choices (lambda ()
                              (let ((files (mapcar #'npmjs-file-name-relative
                                                   (ignore-errors
                                                     (when (fboundp
                                                            'project-files)
                                                       (project-files
                                                        (project-current)))))))
                                (or (seq-filter
                                     (apply-partially #'string-match-p
                                                      "__test__\\|\\.test\\.")
                                     files)
                                    files)))))
            (_
             (pcase type
               ("[boolean]" (list key short-descr
                                  arg))
               ("[string]" (list key short-descr
                                 (concat arg " ")
                                 :class 'transient-option))
               ("[array]"  (list key short-descr
                                 (concat arg " ")
                                 :class 'transient-option
                                 :multi-value 'rest))
               ("[number]"
                (list key short-descr
                      (concat arg " ")
                      :class 'transient-option
                      :reader 'transient-read-number-N0))
               ((pred (string-match-p "^\\[choices: "))
                (list key short-descr (concat arg " ")
                      :class 'transient-option
                      :choices (split-string (replace-regexp-in-string
                                              "^\\[choices:[\s\t]+\\|\\]$" ""
                                              type)
                                             "[,\s\t\n\r\f]"
                                             t))))))))
    (append pl (list :show-help 'npmjs-jest-show-help))))

;;;###autoload
(defun npmjs-jest-show-help (suffix)
  "Display Jest help for a given suffix.

Argument SUFFIX is an object containing a description property."
  (interactive)
  (let* ((descr (oref suffix description))
         (jest-output (npmjs-nvm-with-current-node-version
                       (shell-command-to-string
                        "npm exec -- jest --help")))
         (jest-help
          (with-temp-buffer
            (insert
             jest-output)
            (goto-char
             (point-min))
            (npmjs--parse-jest-output)))
         (buffer (get-buffer-create
                  "*npmjs-jest-help*"))
         (orign-wnd (selected-window)))
    (with-current-buffer buffer
      (with-current-buffer-window
          buffer
          (cons 'display-buffer-at-bottom
                '((window-height . fit-window-to-buffer)))
          (lambda (window _value)
            (with-selected-window window
              (setq buffer-read-only t)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (setq truncate-lines nil)
                (when-let* ((quit-key (where-is-internal
                                       'quit-window
                                       special-mode-map
                                       t t t))
                            (map
                             (make-sparse-keymap))
                            (buff (current-buffer)))
                  (define-key map quit-key
                              (lambda ()
                                (interactive)
                                (when (and orign-wnd
                                           (window-live-p orign-wnd))
                                  (select-window orign-wnd)
                                  (transient-resume)
                                  (when (buffer-live-p buff)
                                    (kill-buffer buff)))))
                  (use-local-map
                   (make-composed-keymap
                    map
                    (current-local-map))))
                (pcase-let* ((longarg (concat "--" descr))
                             (`(,arg ,type ,description ,short)
                              (seq-find (pcase-lambda (`(,arg ,_type
                                                         ,_description
                                                         ,_short))
                                          (and
                                           arg (string= longarg arg)))
                                        jest-help)))
                  (when arg
                    (save-excursion
                      (insert (propertize arg 'face 'font-lock-keyword-face))
                      (if short
                          (insert ", "
                                  (propertize short 'face
                                              'font-lock-keyword-face)
                                  " ")
                        (insert " "))
                      (insert (propertize (or type "") 'face
                                          'font-lock-type-face))
                      (insert "\n")
                      (when description
                        (let ((pos (point)))
                          (insert description)
                          (fill-region-as-paragraph pos (point)))))))))
            (select-window window))))))

(defun npmjs-parse-jest-output (output)
  "Parse Jest test OUTPUT and generate hints.

Argument OUTPUT is a string containing the Jest output to be parsed."
  (let* ((result (with-temp-buffer
                   (insert output)
                   (goto-char (point-min))
                   (npmjs--parse-jest-output)))
         (short-args (seq-filter (lambda (it)
                                   (car (last it)))
                                 result))
         (long-args
          (npmjs-key-builder-generate-shortcuts
           (seq-difference result short-args)
           #'car
           (lambda (key value)
             (npmjs-jest-hint-props (append (list key) value))))))
    (append
     (mapcar (lambda (it)
               (let ((shortarg (car (last it))))
                 (npmjs-jest-hint-props
                  (append (list shortarg)
                          (nbutlast it)))))
             short-args)
     long-args)))

(defun npmjs-jest-version ()
  "Fetch the local version of the Jest package."
  (cadr (assoc-string "jest" (npmjs-local-packages))))


;;;###autoload
(defun npmjs-jest-current-file ()
  "Return string with jest command for testing current file."
  (interactive)
  (when-let ((file (or buffer-file-name default-directory))
             (project-root (npmjs-get-project-default-directory)))
    (let ((path-pattern (shell-quote-argument
                         (file-relative-name file project-root)))
          (default-directory project-root))
      (npmjs-run-compile
       (npmjs-make-command-name
        "npm"
        (append (list "exec"
                      "--"
                      "jest"
                      "--testPathPattern"
                      path-pattern)))
       (concat project-root "jest-compilation")))))

;;;###autoload
(defun npmjs-jest (&optional arg)
  "Run Jest tests with transient commands.

Optional argument ARG is a prefix argument that, when non-nil, forces the
command to bypass the local cache."
  (interactive "P")
  (npmjs-nvm-with-current-node-version
   (when-let* ((jest-version
                (when-let ((vers (npmjs-jest-version)))
                  (concat "jest@" vers)))
               (dir (npmjs-get-project-default-directory))
               (prefix
                (or (and (not arg)
                         (not npmjs-inhibit-prefix-cache)
                         (gethash jest-version npmjs-jest-commands))
                    (let* ((args
                            (npmjs-parse-jest-output
                             (shell-command-to-string
                              "npm exec -- jest --help")))
                           (groupped
                            (npmjs-group-vectors
                             (append args
                                     (list
                                      '(".d" "Directory"
                                        npmjs-install-pkg-directory)
                                      '(".f" "File" npmjs-project-file-argument)
                                      (list "RET"
                                            "Run"
                                            `(lambda ()
                                               (interactive)
                                               (let ((default-directory ,dir))
                                                (npmjs-run-compile
                                                 (npmjs-confirm-command
                                                  "npm"
                                                  (append (list "exec" "--"
                                                           "jest")
                                                   (npmjs-get-formatted-transient-args)))
                                                 (concat
                                                  (npmjs--get-project-buffer-name)
                                                  "-jest")))))
                                      '("C-c C-a"
                                        "Show arguments"
                                        npmjs-show-args)))))
                           (description jest-version)
                           (sym (npmjs-make-symbol "npmjs-jest"
                                                   jest-version))
                           (form)
                           (command))
                      (put sym 'npm-command "npm exec -- jest")
                      (setq form `(transient-define-prefix ,sym ()
                                    [:description
                                     ,description
                                     ,@groupped]))
                      (setq command (eval `(progn ,form
                                            ',sym)
                                          t))
                      (puthash jest-version command npmjs-jest-commands)))))
     (transient-setup prefix))))

;;;###autoload
(define-minor-mode npmjs-jest-auto-test-mode
  "Runs jest on current file save when this mode is turned on."
  :lighter " npmjs-jest-after-save"
  :global nil
  (if npmjs-jest-auto-test-mode
      (add-hook 'after-save-hook #'npmjs-jest-current-file nil 'local)
    (remove-hook 'after-save-hook #'npmjs-jest-current-file 'local)))



(provide 'npmjs)
;;; npmjs.el ends here
;; Local Variables:
;; checkdoc-verb-check-experimental-flag: nil
;; End:
