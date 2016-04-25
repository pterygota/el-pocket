;;; el-pocket.el --- Read and write to Pocket (getpocket.com) -*- lexical-binding: t -*-
;; Author: Tod Davies <davies.t.o@gmail.com>
;; Created: 4 Aug 2014
;; Last Updated: 28 Jan 2015
;; Version: 0.2
;; Url: http://github.com/pterygota/el-pocket
;; Keywords: emacs, pocket, bookmarks
;; Package-Requires: ((web "0.5.2") (emacs "24"))
;; Installation/Setup/Usage

;;; Commentary:

;; Put this file in your load-path somewhere and require it.

;; Or use the MELPA repository to install it via package-install.

;; Now do an M-x el-pocket-authorize RET. The first time you do this
;; you will be directed to the oauth/request page, where you can click
;; on authorize. After authorizing, you may see a message that makes
;; it seem like it didn't work (e.g. broken images or redirect
;; failures). This is because we haven't yet set up proper
;; authorization.

;; Now, return to emacs and do M-x el-pocket-authorize RET again. This
;; time you should get an access token, and it will be saved to
;; ~/.el-pocket-auth.json.

;; Once this is done you should be able to use M-x el-pocket-add RET to add URLs.

;; Reading articles still neeed to be added. Maybe it could be
;; integrated using the Diffbot's Article Extraction API

;; Now you can add these lines to your init file for future use:

;;  (require 'el-pocket)
;;  (el-pocket-load-auth)

;;; History:

;; Changes from 0.1 to 0.2:
;; * Remove '*' from names.
;; * Create a customization group and add some doc strings.
;; * Use defvar instead of setq'ing undefined variables.
;; * Address other compilation warnings.

;;; Code:

(require 'json)
(require 'web)

;;various mouse-eared items
(defgroup el-pocket nil
  "Pocket"
  :prefix "el-pocket-"
  :group 'external)
(defcustom el-pocket-oauth-request-url "https://getpocket.com/v3/oauth/request"
  "URL to use for OAuth request.")
(defcustom el-pocket-oauth-authorize-url "https://getpocket.com/v3/oauth/authorize"
  "URL to use for OAuth authorization.")
(defvar el-pocket-request-token nil
  "Holds the request token")
(defvar el-pocket-access-token-and-username nil
  "Holds the current access token")

;;no use hiding this I suppose
(defcustom el-pocket-consumer-key "30410-da1b34ce81aec5843a2214f4"
  "API consumer key")

;;access-key and username stored here
(defcustom el-pocket-auth-file (expand-file-name "~/.el-pocket-auth.json")
  "JSON file to store the authorization.")

(defun el-pocket-load-auth ()
  (setq el-pocket-access-token-and-username
        (if (file-readable-p el-pocket-auth-file)
            (json-read-file el-pocket-auth-file))))

(defun el-pocket-save-auth ()
  (if (file-exists-p el-pocket-auth-file)
      (delete-file el-pocket-auth-file))
  (append-to-file (json-encode-alist el-pocket-access-token-and-username) nil el-pocket-auth-file))

(defun el-pocket-clear-auth ()
  (setq el-pocket-request-token)
  (setq el-pocket-access-token-and-username))

;; the authorization dance:
;; TODO - make a nice interface for this
;; TODO - maybe use the oauth or oauth2 package instead?
(defun el-pocket-authorize ()
  (interactive)
  (if el-pocket-access-token-and-username t
    (el-pocket-load-auth))
  (if el-pocket-access-token-and-username t
    (if el-pocket-request-token
        (el-pocket-get-access-token)
      (el-pocket-get-request-token))))

;; once the request token is a-gotten,
;; and you've gone to the oauth/authorize page
;; and and done that, this will then get the
;; all-important access-token, huzzah!
(defun el-pocket-get-access-token ()
  "After authorizing, el-pocket-authorize again to call this and get an access-token."
  (let ((post-data (make-hash-table :test 'equal))
        (extra-headers (make-hash-table :test 'equal)))
    (puthash 'consumer_key el-pocket-consumer-key post-data)
    (puthash 'code el-pocket-request-token post-data)
    (puthash 'Host "getpocket.com" extra-headers)
    (puthash 'Content-type "application/x-www-form-urlencoded; charset=UTF-8" extra-headers)
    (puthash 'X-Accept "application/json" extra-headers)
    (web-http-post
     (lambda (con header data)
       (setq el-pocket-access-token-and-username
             (json-read-from-string data))
       (message "data received is: %s" data)
       (el-pocket-save-auth))
     :url el-pocket-oauth-authorize-url
     :data post-data
     :extra-headers extra-headers))
  (sleep-for 1)
  (display-message-or-buffer
   "access a-gotten!"))

;; we don't have a request token yet, so request
;; one, then send the user to oauth/authorize for
;; to authorize this shiz
(defun el-pocket-get-request-token ()
  "Request a request token, then direct the user to authorization URL"
  (let (url
        (post-data (make-hash-table :test 'equal))
        (extra-headers (make-hash-table :test 'equal)))
    (puthash 'consumer_key el-pocket-consumer-key post-data)
    (puthash 'redirect_uri "http://www.google.com" post-data)
    (puthash 'Host "getpocket.com" extra-headers)
    (puthash 'Content-type "application/x-www-form-urlencoded; charset=UTF-8" extra-headers)
    (puthash 'X-Accept "application/json" extra-headers)
    (web-http-post
     (lambda (con header data)
       (let ((token (cdr (assoc 'code (json-read-from-string data)))))
         (setq el-pocket-request-token token)
         (setq url (concat "https://getpocket.com/auth/authorize?request_token=" el-pocket-request-token))
         (kill-new url)))
     :url el-pocket-oauth-request-url
     :data post-data
     :extra-headers extra-headers)
    (sleep-for 1)
    (display-message-or-buffer
     (concat "authorize el-pocket at " url
             " (copied to clipboard)\n"))))

(defun el-pocket-access-granted-p ()
  "Do we have access yet?"
  (if el-pocket-access-token-and-username t))

(defun el-pocket-access-not-granted ()
  (display-message-or-buffer
   "Do an M-x el-pocket-authorize to get access to pocket."))

;; skeleton function to test getting things from pocket
;; response is printed to *Messages*
;; TODO make this do useful things
(defun el-pocket-get ()
  "Gets things from your pocket."
  (if (el-pocket-access-granted-p)
      (let ((post-data (make-hash-table :test 'equal))
            (extra-headers (make-hash-table :test 'equal)))
        (puthash 'consumer_key el-pocket-consumer-key post-data)
        (puthash 'access_token (cdr (assoc 'access_token el-pocket-access-token-and-username)) post-data)
        (puthash 'count "5" post-data)
        (puthash 'detailType "simple" post-data)
        (puthash 'Host "getpocket.com" extra-headers)
        (puthash 'Content-type "application/x-www-form-urlencoded; charset=UTF-8" extra-headers)
        (puthash 'X-Accept "application/json" extra-headers)
        (web-http-post
         (lambda (con header data)
           (message "data received is: %s" data)
           (json-read-from-string data))
         :url "https://getpocket.com/v3/get"
         :data post-data
         :extra-headers extra-headers))
    (el-pocket-access-not-granted)))

;;oh my gosh
(defun el-pocket-add (url-to-add)
  "Add URL-TO-ADD to your pocket."
  (interactive
   (list
    (read-string "el-pocket url: ")))
  (if (el-pocket-access-granted-p)
      (let ((post-data (make-hash-table :test 'equal))
            (extra-headers (make-hash-table :test 'equal)))
        (puthash 'consumer_key el-pocket-consumer-key post-data)
        (puthash 'access_token (cdr (assoc 'access_token el-pocket-access-token-and-username)) post-data)
        (puthash 'url url-to-add post-data)
        (puthash 'Host "getpocket.com" extra-headers)
        (puthash 'Content-type "application/x-www-form-urlencoded; charset=UTF-8" extra-headers)
        (puthash 'X-Accept "application/json" extra-headers)
        (web-http-post
         (lambda (con header data)
           (message "data received is: %s" data)
           (json-read-from-string data))
         :url "https://getpocket.com/v3/add"
         :data post-data
         :extra-headers extra-headers))
    (el-pocket-access-not-granted)))

(provide 'el-pocket)

;;; el-pocket.el ends here
