;;; emacs-dokuwiki.el --- Edit Remote DokuWiki Pages Using XML-RPC

;; Copyright (C) 2017 Juan Karlo Licudine

;; Author: Juan Karlo Licudine <accidentalrebel@gmail.com>
;; URL: http://www.github.com/accidentalrebel/emacs-dokuwiki
;; Version: 0.0.3
;; Keywords: convenience
;; Package-Requires: ((emacs "24.3") (xml-rpc "1.6.8"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides a way to edit a remote Dokuwiki wiki on Emacs.  Uses Dokuwiki's XML-RPC API.

;; Usage:
;; (require 'emacs-dokuwiki) ;; unless installed as a package

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

;;; TODO
;; * Make a variable that would hold the username

;;; Code:

(require 'xml-rpc)

(defvar emacs-dokuwiki-xml-rpc-url nil
  "The url pointing to the \"xmlrpc.php\" file in the wiki to be accessed.")

(defun emacs-dokuwiki-login()
  "Connects to the dokuwiki"
  (interactive)
  (if (equal emacs-dokuwiki-xml-rpc-url nil)
      (emacs-dokuwiki-set-xml-rpc-url))
  (let* ((login-name (read-string "Enter login name: "))
	 (login-password (read-passwd "Enter password: ")))
    (if (eq (xml-rpc-method-call emacs-dokuwiki-xml-rpc-url 'dokuwiki.login login-name login-password) t)
	(message "Emacs-dokuwiki: Login successful!")
      (error "Emacs-dokuwiki: Login unsuccessful! Check if your emacs-dokuwiki-xml-rpc-url or login credentials are correct!"))))

(defun emacs-dokuwiki-set-xml-rpc-url()
  "Asks and sets the dokuwiki xml-rpc url."
  (interactive)
  (let ((xml-rpc-url (read-string "Enter wiki URL: ")))
    (message "Emacs-dokuwiki: Saved the wiki url \"%s\"." xml-rpc-url)
    (setq emacs-dokuwiki-xml-rpc-url xml-rpc-url)))

(defun emacs-dokuwiki-open-page()
  "Opens a page from the wiki.

To open a page in a particular namespace add the namespace name before the page-name. For example, \"namespace:wiki-page\" to open the \"wiki-page\" page inside the \"namespace\" namespace.

If the specified page does not exist, it creates a new page once the buffer is saved."
  (interactive)
  (if (equal emacs-dokuwiki-xml-rpc-url nil)
      (user-error "Emacs-dokuwiki: Call emacs-dokuwiki-setup() first")
    (let ((page-name (read-string "Enter page name: ")))
      (message "emacs-dokuwiki: page name is \"%s\"" page-name)
      (let ((page-content (xml-rpc-method-call emacs-dokuwiki-xml-rpc-url 'wiki.getPage page-name)))
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
		   (minor (y-or-n-p "Is this a minor change? "))
		   (save-success (xml-rpc-method-call emacs-dokuwiki-xml-rpc-url 'wiki.putPage page-name (buffer-string) `(("sum" . ,summary) ("minor" . ,minor)))))
	      (if (eq save-success t)
		  (message "Emacs-dokuwiki: Saving successful with summary %s and minor of %s." summary minor)
		(error "Emacs-dokuwiki: Saving unsuccessful!"))))
	(message "Emacs-dokuwiki: Cancelled saving of the page."))
      ))
  )

(defun emacs-dokuwiki-get-wiki-title()
  "Gets the title of the current wiki"
  (interactive)
  (if (equal emacs-dokuwiki-xml-rpc-url nil)
      (user-error "Emacs-dokuwiki: Call emacs-dokuwiki-setup() first")
    (let ((dokuwiki-title (xml-rpc-method-call emacs-dokuwiki-xml-rpc-url 'dokuwiki.getTitle)))
      (message "Emacs-dokuwiki: The title of the wiki is \"%s\"" dokuwiki-title))))

(provide 'emacs-dokuwiki)
;;; emacs-dokuwiki.el ends here
