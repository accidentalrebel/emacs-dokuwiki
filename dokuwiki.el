;;; dokuwiki.el --- Edit Remote DokuWiki Pages Using XML-RPC  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Juan Karlo Licudine
;; additional edits
;;   2021 vincowl
;;   2018, 2023 WillForan

;; Author: Juan Karlo Licudine <accidentalrebel@gmail.com>
;; URL: http://www.github.com/accidentalrebel/emacs-dokuwiki
;; Version: 1.2.0
;; Keywords: convenience
;; Package-Requires: ((emacs "24.3") (xml-rpc "1.6.8"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides a way to edit a remote Dokuwiki wiki on Emacs.
;; Uses Dokuwiki's XML-RPC API.

;; Usage:
;; (require 'dokuwiki) ;; unless installed as a package

;;; License:

;; This program is free software; you can redistributfe it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'xml-rpc)
(require 'ffap)
(require 'auth-source)
(require 'dokuwiki-mode)

(defgroup dokuwiki nil
  "Edit remote Dokuwiki pages using XML-RPC."
  :group 'dokuwiki)

(defcustom dokuwiki-xml-rpc-url ""
  "The url pointing to the \"xmlrpc.php\" file in the wiki to be accessed."
  :group 'dokuwiki
  :type 'string)

(defcustom dokuwiki-login-user-name ""
  "The user name to use when logging in to the wiki."
  :group 'dokuwiki
  :type 'string)

(defvar dokuwiki--has-successfully-logged-in nil
  "A variable that is set to true once successfully logged in to a wiki.")

;;;###autoload
(defun dokuwiki-login ()
  "Connects to the dokuwiki."
  (interactive)
  (let* ((xml-rpc-url (dokuwiki--get-xml-rpc-url))
         (credentials (dokuwiki--credentials))
         (login-user-name (plist-get credentials :user))
         (login-password (plist-get credentials :password)))
    (if (not (xml-rpc-method-call xml-rpc-url 'dokuwiki.login login-user-name login-password))
	(error "Login unsuccessful! Check if your dokuwiki-xml-rpc-url or login credentials are correct!")
      (message "Login successful!")
      (setq dokuwiki--has-successfully-logged-in t)
      (dokuwiki-pages-get-list-cache t)
      (dokuwiki-list-pages-cached))))

(defvar dokuwiki-open-page-hook nil
  "Hooks after logging in to a dokuwiki.")
(defun dokuwiki-open-page (page-name-or-url)
  "Opens a page from the wiki.

PAGE-NAME-OR-URL: The page id or url to open.

To open a page in a particular namespace add the namespace name before
the page-name.  For example, \"namespace:wiki-page\" to open the
\"wiki-page\" page inside the \"namespace\" namespace.

If the specified page does not exist, it creates a new page once the
buffer is saved."
  (interactive "sEnter page name: ")
  (if (not dokuwiki--has-successfully-logged-in)
      (user-error "Login first before opening a page")
    (let* ((page-name (car (last (split-string page-name-or-url "/"))))
	  (page-content (xml-rpc-method-call dokuwiki-xml-rpc-url 'wiki.getPage page-name)))
      (message "Page name is \"%s\"" page-name)
      (if (not page-content)
	  (message "Page not found in wiki. Creating a new buffer with page name \"%s\"" page-name)
	(message "Page exists. Creating buffer for existing page \"%s\"" page-name))
      (get-buffer-create (concat page-name ".dwiki"))
      (switch-to-buffer (concat page-name ".dwiki"))
      (erase-buffer)
      (if page-content
          (progn
            (insert page-content)
            (with-current-buffer
                (get-buffer (concat page-name ".dwiki"))))
        ;; no content? make page name the first heading
        (insert "====== " (replace-regexp-in-string ".*:" "" page-name ) " ======"))
      (dokuwiki-mode)               ; should this go in the launch-hook ?
      (goto-char (point-min))
      (run-hooks 'dokuwiki-open-page-hook))))

(defun dokuwiki-save-page ()
  "Save the current buffer as a page in the wiki.

Uses the buffer name as the page name.  A buffer of \"wiki-page.dwiki\"
is saved as \"wikiurl.com/wiki-page\".  On the other hand, a buffer of
\"namespace:wiki-page.dwiki\" is saved as \"wikiurl.com/namespace:wiki-page\""
  (interactive)
  (if (not dokuwiki--has-successfully-logged-in)
      (user-error "Login first before saving a page")
    (if (not (string-match-p ".dwiki" (buffer-name)))
	(error "The current buffer is not a .dwiki buffer")
      (let ((page-name (replace-regexp-in-string ".dwiki" "" (buffer-name))))
	(if (not (y-or-n-p (concat "Do you want to save the page \"" page-name "\"?")))
	    (message "Cancelled saving of the page."))
	 (let* ((summary (read-string "Summary: "))
		(minor (y-or-n-p "Is this a minor change? "))
		(save-success (xml-rpc-method-call dokuwiki-xml-rpc-url 'wiki.putPage page-name (buffer-string) `(("sum" . ,summary) ("minor" . ,minor)))))
	   (if save-success
	       (message "Saving successful with summary %s and minor of %s." summary minor)
	     (error "Saving unsuccessful!")))))))

(defun dokuwiki-get-wiki-title ()
  "Gets the title of the current wiki."
  (interactive)
  (if (not dokuwiki--has-successfully-logged-in)
      (user-error "Login first before getting the wiki title")
    (let ((dokuwiki-title (xml-rpc-method-call dokuwiki-xml-rpc-url 'dokuwiki.getTitle)))
      (message "The title of the wiki is \"%s\"" dokuwiki-title))))

(defun dokuwiki-get-page-list ()
  "Extract 'id' from page info."
  (if (not dokuwiki--has-successfully-logged-in)
      (user-error "Login first before listing the pages")
    (let ((page-detail-list (xml-rpc-method-call dokuwiki-xml-rpc-url 'wiki.getAllPages))
	  (page-list ()))
      (progn
        (dolist (page-detail page-detail-list)
        	(push (cdr (assoc "id" page-detail)) page-list))
        page-list))))

(defun dokuwiki-list-pages ()
  "Show a selectable list containing pages from the current wiki.  Not cached."
  (interactive)
  (dokuwiki-open-page (completing-read "Select a page to open: " (dokuwiki-get-page-list))))


(defun dokuwiki-insert-link-from-list ()
  "Insert link from wiki page list.  Not cached."
  (interactive)
  (insert (concat "[[" (completing-read "Select a page to link: " (dokuwiki-get-page-list)) "]]")))

;; Helpers
(defun dokuwiki--credentials ()
  "Read dokuwiki credentials either from auth source or from the user input."
  (let ((auth-source-credentials (nth 0 (auth-source-search :max 1 :host (dokuwiki--get-xml-rpc-url) :require '(:user :secret)))))
    (if auth-source-credentials
        (let* ((user (plist-get auth-source-credentials :user))
               (password-raw (plist-get auth-source-credentials :secret))
               (password (if (functionp password-raw) (funcall password-raw) password-raw)))
          (list :user user :password password))
      (let ((user (dokuwiki--get-login-user-name))
            (password (read-passwd "Enter password: ")))
        (list :user user :password password)))))

(defun dokuwiki--get-xml-rpc-url ()
  "Gets the xml-rpc to be used for logging in."
  (if (not (string= dokuwiki-xml-rpc-url ""))
      dokuwiki-xml-rpc-url
    (let ((xml-rpc-url (read-string "Enter wiki URL: ")))
      (message "The entered wiki url is \"%s\"." xml-rpc-url)
      xml-rpc-url)))

(defun dokuwiki--get-login-user-name ()
  "Gets the login user name to be used for logging in."
  (if (not (string= dokuwiki-login-user-name ""))
      dokuwiki-login-user-name
    (let ((login-name (read-string "Enter login user name: ")))
      (message "The entered login user name is \"%s\"." login-name)
      login-name)))

;; caching
(defvar dokuwiki-cached-page-list nil
  "List of all pages cached for quick linking and listing.")

(defun dokuwiki-pages-get-list-cache ( &optional refresh)
  "Get list of page; if cache is unset or REFRESH, fetch."
  (when (or (not dokuwiki-cached-page-list) refresh)
      (if (not dokuwiki--has-successfully-logged-in)
        (user-error "Login first before listing the pages")
        (let ((page-detail-list (xml-rpc-method-call dokuwiki-xml-rpc-url 'wiki.getAllPages))
              (page-list ()))
          (dolist (page-detail page-detail-list)
            (push (concat ":" (cdr (assoc "id" page-detail))) page-list))
          (setq dokuwiki-cached-page-list page-list))))
      dokuwiki-cached-page-list)

(defun dokuwiki-insert-link-from-cache ()
  "Show a selectable list containing pages from the current wiki.
Refresh when univesal arg."
  (interactive)
  (if-let (page (completing-read "Select a page to link: " (dokuwiki-pages-get-list-cache current-prefix-arg)))
      (insert (concat "[[:" page "]] "))))

(defun dokuwiki-list-pages-cached ()
  "Show a selectable list containing pages from the current wiki.
Refresh when univerasl arg."
  (interactive)
  (dokuwiki-open-page (completing-read "Select a page to open: " (dokuwiki-pages-get-list-cache current-prefix-arg))))


;;; completion
;; add completion canidates based on cached page names
;; useful with company mode
(defun dokuwiki-link-wrap ()
  "Wrap current word as link."
  (interactive)
  (let*
      ((bounds (bounds-of-thing-at-point 'filename))
      (x (car bounds))
      (y (cdr bounds))
      (s (buffer-substring-no-properties x y)))
    (progn
      (delete-region x y)
      (goto-char x)
      (insert (concat "[[" s "]]")))))

;; completion-at-point-functions
(defun dokuwiki--back-to-space-or-line (pt)
  "Get point of the closest space, or beginning of line if first.
Start search at PT."
  (let ((this-line (line-beginning-position)))
    (save-excursion
      (goto-char pt)
      (skip-syntax-backward "^ ")
      (max (point) this-line))))

(defun  dokuwiki--capf-link-wrap (comp-string status)
"When capf STATUS is finished, make the COMP-STRING into a link.
NB COMP-STRING not used: link-wrap using point instead."
  (when (eq status 'finished) (dokuwiki-link-wrap)))

(defun dokuwiki--capf ()
  "Use DOKUWIKI-CACHED-PAGE-LIST for completion.
Wrap as link when finished."
  (when (and dokuwiki-cached-page-list (looking-back ":[a-zA-Z:]+" (-(point)(line-beginning-position))))
    ;; (looking-back ":[a-zA-Z:]+" (-(point)(line-beginning-position)) t)
    (list (dokuwiki--back-to-space-or-line (match-beginning 0))
          (match-end 0)
          dokuwiki-cached-page-list
          :exit-function #'dokuwiki--capf-link-wrap)))

;;; links
(defun dokuwiki--fwpap (&optional at-point)
  "Find wiki path around point using 'find file around point'.
Start at AT-POINT if given.
Move past any '[' and behind ']' before looking under point for a path.
NB text is :a:b not /a/b but same file pattern rules apply."
  (save-excursion
    (when at-point (goto-char at-point))
    ;; skip ahead of [[ if looking at first part of link
    (skip-chars-forward "[")
    (skip-chars-backward "]")
    ;; requires ffap
    (ffap-string-at-point 'file)))

(defun dokuwiki-ffap ()
  "Open wiki path under cursor."
  (interactive)
  (dokuwiki-open-page (dokuwiki--fwpap)))

;;; clickable links
;; generated with:
;; (if (featurep 'button-lock) (button-lock-set-button "[[.*?]]" 'dokuwiki--ffap-path))
(defvar dokuwiki-font-lock-link
  '("\\[\\[.*?\\]\\]"
   (0 ’(face button-lock-face keymap (keymap (mouse-1 . dokuwiki--ffap-path))
             button-lock t mouse-face button-lock-mouse-face rear-nonsticky t)
       append))
  "Make a link clickable.")


;;; convient vairalbe setter

(defun dokuwiki-launch (url user)
  "Login to dokuwiki using URL and USER.  Open a page.
Set NO-LAUNCH for no page jump."
  (setq dokuwiki-xml-rpc-url url
        dokuwiki-login-user-name user)
  (dokuwiki-login))

(defun dokuwiki-in-browser ()
  "Open current page in the borwser.  Assumes fixed xmlrpc url suffixe."
  (interactive)
  (let ((base-url (replace-regexp-in-string "/lib/exe/xmlrpc.php" "/doku.php" dokuwiki-xml-rpc-url))
        (page-name (replace-regexp-in-string ".dwiki$" "" (buffer-name))))
    (browse-url (concat base-url "?id=" page-name))))

;;; default bindings and hooks
(defun dokuwiki-setup ()
  "Provide suggested default keys bindings.
Note: dokuwiki-mode is a separate package, modifying it's map."
  (interactive)
  (eval-after-load 'dokuwiki
    '(progn
       (add-hook 'completion-at-point-functions 'dokuwiki--capf nil 'local)
       ;; not sure hwo to do this
       ;; (add-hook 'dokuwiki-mode (lambda () (font-lock-add-keywords nil dokuwiki-font-lock-link)))
       (define-key dokuwiki-mode-map (kbd "C-c g") #'dokuwiki-list-pages-cached)
       (define-key dokuwiki-mode-map (kbd "C-c s") #'dokuwiki-save-page)
       (define-key dokuwiki-mode-map (kbd "C-c o") #'dokuwiki-ffap)
       (define-key dokuwiki-mode-map (kbd "C-c b") #'dokuwiki-in-browser)
       (define-key dokuwiki-mode-map (kbd "C-c l") #'dokuwiki-insert-link-from-cache))))


(provide 'dokuwiki)
;;; dokuwiki.el ends here
