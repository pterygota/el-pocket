el-pocket
=========

el-pocket :: emacs -> getpocket.com
-----------------------------------

# Installation
Put this file in your load-path somewhere and require it.

Or use the [MELPA repository](http://melpa.org/#/el-pocket) to install
it via [package-install](http://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html).

Now do an `M-x el-pocket-authorize RET`. The first time you do this
you will be directed to the oauth/request page, where you can click on
authorize. After authorizing, you may see a message that makes it seem
like it didn't work (e.g. broken images or redirect failures).  This
is because we haven't yet set up proper authorization.

Now, return to emacs and do `M-x el-pocket-authorize RET` again. This
time you should get an access token, and it will be saved to
`~/.el-pocket-auth.json`.

Once this is done you should be able to use `M-x el-pocket-add RET` to
add URLs.

Reading articles still neeed to be added.  Maybe it could be
integrated using the Diffbot's
[Article Extraction API](https://www.diffbot.com/dev/docs/article/)

Now you can add these lines to your init file for future use:

```lisp
(require 'el-pocket)
(el-pocket-load-auth)
```


