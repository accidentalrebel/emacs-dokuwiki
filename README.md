# Dokuwiki for Emacs
[![MELPA](https://melpa.org/packages/dokuwiki-badge.svg)](https://melpa.org/#/dokuwiki)

Edit remote Dokuwiki pages using XML-RPC
 
# Overview #
**Dokuwiki for Emacs** is a package that provides a way to edit a remote Dokuwiki wiki through Emacs. The package uses [Dokuwiki's XML-RPC API](https://www.dokuwiki.org/devel:xmlrpc).

Currently, the package can edit, create, and save pages. There is rudimentary complition provided for inter-linking as well as keybindings to launch a page list menu (jump to page or insert link). 

# Installation #
This package is on melpa as `dokuwiki`! The easiest way to get it is with [`use-package`](https://github.com/jwiegley/use-package). Pair that with <kbd>M-x package-install [`quelpa-use-package`](https://github.com/quelpa/quelpa-use-package)</kbd> to get the most up-to-date (and buggy) version.

``` emacs-lisp
(use-package dokuwiki
  ; quelpa line to fetch versions ahead of what is on melpa. comment out to stick with stable
  :quelpa ((dokuwiki :fetcher github :repo "WillForan/emacs-dokuwiki") :upgrade t)
  :ensure t
  :init
   (defun my-wiki ()
     "Open a page on my wiki"
     (interactive)
     (require 'dokuwiki)
     (dokuwiki-launch "https://www.mysite.com/wiki/lib/exe/xmlrpc.php" "myuser"))
  :config
   (dokuwiki-setup)  ; default key-bindings, link completion at point (dokuwiki-cap)
   (company-mode 1)  ; links as typing if starting with colon like ':foo:bar'
   (flyspell-mode 1)
)
```

Alternatively, you can `git clone https://github.com/accidentalrebel/emacs-dokuwiki.git ~/.emacs.d/dokuwiki` with `(require 'dokuwiki)` in `~/.emacs.d/init.el` and manage configation as you please.

## Dependencies ##
  * The latest version of XML-RPC.el.
  * `dokuwiki-mode` - font-locking/highlighting, outline-mode integration

### Optional
  * `ouline-magic` - more outline functions
  * `company` - for completion at point while typing

# Usage #

## Quick Setup

1. config and evalute use-package call (as detailed in [Installation](#installation))
   * wrap `dokuwiki-launch` with your wiki settings, e.g. `my-wiki`
   * optionally add credentials to `~/.authinfo` (example [below](#auth-source-credentials))
2. <kbd>M-x my-wiki</kbd> as defined above
3. select page, edit, and save
   * <kbd>C-c g </kbd> `dokuwiki-list-pages-cached` - menu to select page to edit (prefix with <kbd>C-u</kbd> to refresh cached page list)
   * <kbd>C-c s </kbd> `dokuwiki-save-page` - write buffer to wiki server via xml-rpc
   * <kbd>C-c o </kbd> `dokuwiki-ffap` - open page when cursor over `[[:foo:bar]]`
   * <kbd>C-c b </kbd> `dokuwiki-in-browser` - open current page in e.g. firefox
   * <kbd>C-c l </kbd> `dokuwiki-insert-link-from-cache` - menu to insert `[[:link:]]`


## Logging In ##
If you prefer to step through each function, the first thing to do is to login using the `dokuwiki-login` function. Once successfully logged in the user can now be able to use the other available functions.

### Customize credentials
To avoid entering the *xml-rpc-url* and the *login-user-name* everytime you login consider setting the `dokuwiki-xml-rpc-url` and `dokuwiki-login-user-name` variables through the [Emacs customization interface](https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html) or by adding the code below when Emacs starts:

``` emacs-lisp
(setq dokuwiki-xml-rpc-url "http://www.url-of-wiki.com/lib/exe/xmlrpc.php")
(setq dokuwiki-login-user-name "username-of-user")
```

### Auth-source credentials
Credentials matching the `dokuwiki-xml-rpc-url` url in `~/.authinfo` (or any file in `auth-sources`) will be used.

```txt
machine https://www.mywiki.com/wiki/lib/exe/xmlrpc.php login myuser password correct-horse-battery-stapler
```

You might need to <kbd>M-x auth-source-forget-all-cached</kbd> for emacs to see changes.

## Opening a page ##
Use the function `dokuwiki-list-pages` to list the wiki pages available. Selecting any element in the list opens it up in a new buffer.

Alternatively, the function `dokuwiki-open-page` will download the contents of the wiki page specified by the user. If the specified page does not exist then the page is created on the remote wiki once the page is saved.

Can accept a full url like *wikiurl.com/wiki-page* or just *wiki-page*.

**Note**
To open a page in a particular namespace add the namespace name before the page-name. For example, *namespace:wiki-page* to open the *wiki-page* page inside the *namespace* namespace.

## Saving a page##
The function `dokuwiki-save-page` will save the contents of the current *.dwiki* buffer to the remote wiki. This function uses the buffer name as the page name to be used when saving. A buffer of *wiki-page.dwiki* is saved as *wikiurl.com/wiki-page*. On the other hand, a buffer of *namespace:wiki-page.dwiki* is saved as *wikiurl.com/namespace:wiki-page*.

**Note**
This function is intended to be called on a buffer created after opening a page. This will still work on any buffer as long as the buffer has a *.dwiki* at the end of it's buffer name.

# Miscellaneous #
  * After opening a page consider using the [emacs-dokuwiki-mode](https://github.com/kai2nenobu/emacs-dokuwiki-mode) or [dokuwiki-mode.el](https://github.com/larsjsol/dokuwiki-mode.el) for easy editing of the buffers.
  
  ```emacs-lisp
(use-package dokuwiki-mode
  :quelpa ((dokuwiki :fetcher github :repo "WillForan/dokuwiki-mode") :upgrade t)
  :ensure t
  :config
   (require outline-magic)
   (flyspell-mode 1))
  ```

# Contributing #
Contributions are always welcome. Feel free to submit a pull request, create an issue, or send suggestions.

# To Do #
  * Add ability to preview page changes
  * Better support multiple wikis

