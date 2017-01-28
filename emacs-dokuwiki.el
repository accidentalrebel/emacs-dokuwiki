;;; emacs-dokuwiki.el --- Edit DokuWiki Pages using Dokuwiki's XML-RPC API

;;; Commentary:

;;; TODO
;; * Rename *emacs-dokuwiki-xml-rpc-url* without the asterisks
;; * Only ask for the URL if the emacs-dokuwiki-xml-rpc-url variable is not set
;; * Make a variable that would hold the username
;; * Remove emacs-dokuwiki-setup, just use emacs-dokuwiki-login as the entry point

;;; Code:

(require 'xml-rpc)

(defvar *emacs-dokuwiki-xml-rpc-url* "")

(defun emacs-dokuwiki-setup()
  "Initial set up for the dokuwiki"
  (interactive)
  (let ((xml-rpc-url (read-string "Enter wiki URL: ")))
    (message "Emacs-dokuwiki: Saved the wiki url \"%s\"." xml-rpc-url)
    (setq *emacs-dokuwiki-xml-rpc-url* xml-rpc-url)))

(defun emacs-dokuwiki-open-page()
  "Opens a page from the wiki.

To open a page in a particular namespace add the namespace name before the page-name. For example, \"namespace:wiki-page\" to open the \"wiki-page\" page inside the \"namespace\" namespace.

If the specified page does not exist, it creates a new page once the buffer is saved.
"
  (interactive)
  (if (equal *emacs-dokuwiki-xml-rpc-url* "")
      (user-error "Emacs-dokuwiki: Call emacs-dokuwiki-setup() first")
    (let ((page-name (read-string "Enter page name: ")))
      (message "emacs-dokuwiki: page name is \"%s\"" page-name)
      (let ((page-content (xml-rpc-method-call *emacs-dokuwiki-xml-rpc-url* 'wiki.getPage page-name)))
	(if (equal page-content nil)
	    (message "Emacs-dokuwiki: Page not found in wiki. Creating a new buffer with page name \"%s\"" page-name)
	  (message "Emacs-dokuwiki: Page exists. Creating buffer for existing page \"%s\"" page-name))
	(get-buffer-create (concat page-name ".dwiki"))
	(switch-to-buffer (concat page-name ".dwiki"))
	(erase-buffer)
	(when (not (eq page-content nil))
	    (insert page-content))))))

(defun emacs-dokuwiki-save-page()
  "Saves the current buffer as a page in the wiki.

Uses the buffer name as the page name. A buffer of \"wiki-page.dwiki\" is saved as \"wikiurl.com/wiki-page\". On the other hand, a buffer of \"namespace:wiki-page.dwiki\" is saved as \"wikiurl.com/namespace:wiki-page\""
  (interactive)
  (if (eq (string-match-p ".dwiki" (buffer-name)) nil)
      (error "Emacs-dokuwiki: The current buffer is not a .dwiki buffer")
    (let ((page-name (replace-regexp-in-string ".dwiki" "" (buffer-name))))
      (if (y-or-n-p (concat "Do you want to save the page \"" page-name "\"?"))
	  (progn
	    (message "Emacs-dokuwiki: Saving the page \"%s\"" page-name)
	    (let* ((summary (read-string "Summary:"))
		   (minor (y-or-n-p "Is this a minor change?"))
		   (save-success (xml-rpc-method-call *emacs-dokuwiki-xml-rpc-url* 'wiki.putPage page-name (buffer-string) `(("sum" . ,summary) ("minor" . ,minor)))))
	      (if (eq save-success t)
		  (message "Emacs-dokuwiki: Saving successful with summary %s and minor of %s." summary minor)
		(error "Emacs-dokuwiki: Saving unsuccessful!"))))
	(message "Emacs-dokuwiki: Cancelled saving of the page."))
      ))
  )

(defun emacs-dokuwiki-get-wiki-title()
  "Gets the title of the current wiki"
  (interactive)
  (if (equal *emacs-dokuwiki-xml-rpc-url* "")
      (user-error "Emacs-dokuwiki: Call emacs-dokuwiki-setup() first")
    (let ((dokuwiki-title (xml-rpc-method-call *emacs-dokuwiki-xml-rpc-url* 'dokuwiki.getTitle)))
      (message "Emacs-dokuwiki: The title of the wiki is \"%s\"" dokuwiki-title))))

(defun emacs-dokuwiki-login()
  "Connects to the dokuwiki"
  (interactive)
  (if (equal *emacs-dokuwiki-xml-rpc-url* "")
      (user-error "Emacs-dokuwiki: Call emacs-dokuwiki-setup() first")
    (let* ((login-name (read-string "Enter login name: "))
	   (login-password (read-passwd "Enter password: ")))
      (if (eq (xml-rpc-method-call *emacs-dokuwiki-xml-rpc-url* 'dokuwiki.login login-name login-password) t)
	  (message "Emacs-dokuwiki: Login successful!")
	(error "Emacs-dokuwiki: Warning! Login unsuccessful!")))))

(provide 'emacs-dokuwiki)
;;; emacs-dokuwiki.el ends here
