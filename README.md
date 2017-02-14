# Dokuwiki for Emacs
[![MELPA](https://melpa.org/packages/dokuwiki-badge.svg)](https://melpa.org/#/dokuwiki)

Edit remote Dokuwiki pages using XML-RPC
 
# Overview #
**Dokuwiki for Emacs** is a package that provides a way to edit a remote Dokuwiki wiki through Emacs. The package uses [Dokuwiki's XML-RPC API](https://www.dokuwiki.org/devel:xmlrpc).

Package is still under development. Currently, the package can edit, create, and save pages. Will be adding more convenience functions to make the process easier in the near future.

# Installation #
Download **Dokuwiki for Emacs** from Github.

``` emacs-lisp
git clone https://github.com/accidentalrebel/emacs-dokuwiki.git
```

Add this to your init:

``` emacs-lisp
require 'dokuwiki
```

## Dependencies ##
  * The latest version of XML-RPC.el.

# Usage #

## Logging In ##
The first thing to do is to login using the `dokuwiki-login` function. Once successfully logged in the user can now be able to use the other available functions.

**Note**
To avoid entering the *xml-rpc-url* and the *login-user-name* everytime you login consider setting the `dokuwiki-xml-rpc-url` and `dokuwiki-login-user-name` variables through the [Emacs customization interface](https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html) or by adding the code below when Emacs starts:

``` emacs-lisp
(setq dokuwiki-xml-rpc-url "http://www.url-of-wiki.com/lib/exe/xmlrpc.php")
(setq dokuwiki-login-user-name "username-of-user")
```

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

# Contributing #
Contributions are always welcome. Feel free to submit a pull request, create an issue, or send suggestions.

# To Do #
  * Use auth-source in storing login credentials
  * Add ability to preview page changes
  * Ask to open the page in a browser after saving
