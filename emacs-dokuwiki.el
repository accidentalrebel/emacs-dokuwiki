;;; emacs-dokuwiki.el --- Edit DokuWiki Pages using XML-RPC

;;; Commentary:

;;; Code:

(require 'xml-rpc)

(defvar *emacs-dokuwiki-xml-rpc-url* "")

(defun emacs-dokuwiki-setup()
  "Initial set up for the dokuwiki"
  (interactive)
  (let ((xml-rpc-url (read-string "Enter wiki URL: ")))
    (message "emacs-dokuwiki: Saved the wiki url: %s." xml-rpc-url)
    (setq *emacs-dokuwiki-xml-rpc-url* xml-rpc-url)))

(defun emacs-dokuwiki-open-page()
  "Connects to the dokuwiki"
  (interactive)
  (if (equal *emacs-dokuwiki-xml-rpc-url* "")
      (user-error "Emacs-dokuwiki: Call emacs-dokuwiki-setup() first")
    (let ((page-name (read-string "Enter page name: ")))
      (message "page name is %s" page-name)
      (let ((page-content (xml-rpc-method-call *emacs-dokuwiki-xml-rpc-url* 'wiki.getPage page-name)))
	(if (equal page-content nil)
	    (error "Emacs-dokuwiki: Could not get the page content from page %s" page-name)
	  (message "Emacs-dokuwiki: Creating a new buffer for page %s" page-name)
	  (switch-to-buffer (concat "emacs-dokuwiki:" page-name))
	  (insert page-content))))))

(defun emacs-dokuwiki-get-wiki-title()
  "Gets the title of the current wiki"
  (interactive)
  (if (equal *emacs-dokuwiki-xml-rpc-url* "")
      (user-error "Emacs-dokuwiki: Call emacs-dokuwiki-setup() first")
    (let ((dokuwiki-title (xml-rpc-method-call *emacs-dokuwiki-xml-rpc-url* 'dokuwiki.getTitle)))
      (message "Emacs-dokuwiki: The title of the wiki is %s" dokuwiki-title))))

(defun emacs-dokuwiki-login()
  "Connects to the dokuwiki"
  (interactive)
  (if (equal *emacs-dokuwiki-xml-rpc-url* "")
      (user-error "Emacs-dokuwiki: Call emacs-dokuwiki-setup() first")
    (let* ((login-name (read-string "Enter login name: "))
	   (login-password (read-passwd "Enter password: ")))
      (xml-rpc-method-call *emacs-dokuwiki-xml-rpc-url* 'dokuwiki.login login-name login-password))))

(provide 'emacs-dokuwiki)
;;; emacs-dokuwiki.el ends here
