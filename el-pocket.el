;;; el-pocket.el --- el-pocket :: emacs -> getpocket.com
;; Author: Tod Davies <davies.t.o@gmail.com>
;; Created: 4 Aug 2014
;; Version: 20140804.2311
;; Url: http://github.com/pterygota/el-pocket
;; Keywords: emacs, pocket, bookmarks

;; Installation/Setup/Usage:
;; Put this file in your load-path somewhere and require it.
;; Now do an M-x el-pocket-authorize. The first time you do this
;; you will be directed to the oauth/request page, where you can 
;; click on authorize. After authorizing, return to emacs and 
;; M-x el-pocket-authorize again. This time you should get an access
;; token, and it will be saved to "~/.el-pocket-auth.js".
;; Once this is done you should be able to el-pocket-add URLs ...
;; Now you can add these lines to your init file for future enuserating:
;;
;; (require 'el-pocket)
;; (el-pocket-load-auth)
;;
;; -*- lexical-binding: t -*-

(require 'web)
(require 'json)

;;various mouse-eared items
(setq *el-pocket-oauth-request-url* "https://getpocket.com/v3/oauth/request")
(setq *el-pocket-oauth-authorize-url* "https://getpocket.com/v3/oauth/authorize")
(setq *request-token*)
(setq *access-token-and-username*)

;;no use hiding this I suppose
(setq *consumer-key* "30410-da1b34ce81aec5843a2214f4")

;;access-key and username stored here
(setq *el-pocket-auth-file* (expand-file-name "~/.el-pocket-auth.js"))

(defun el-pocket-load-auth ()
  (setq *access-token-and-username*
	(if (file-readable-p *el-pocket-auth-file*)
	    (json-read-file *el-pocket-auth-file*))))

(defun el-pocket-save-auth ()
  (progn
    (if (file-exists-p *el-pocket-auth-file*)
	(delete-file *el-pocket-auth-file*))
    (append-to-file (json-encode-alist *access-token-and-username*) nil *el-pocket-auth-file*)))

(defun el-pocket-clear-auth ()
  (progn
    (setq *request-token*)
    (setq *access-token-and-username*)))

;; the authorization dance:
;; TODO - make a nice interface for this 
;; TODO - maybe use the oauth or oauth2 package instead?
(defun el-pocket-authorize ()
  (interactive)
  (progn
    (if *access-token-and-username* t
      (el-pocket-load-auth))
    (if *access-token-and-username* t 
      (if *request-token* 
	  (el-pocket-get-access-token)
	(el-pocket-get-request-token)))))

;; once the request token is a-gotten,
;; and you've gone to the oauth/authorize page
;; and and done that, this will then get the 
;; all-important access-token, huzzah!
(defun el-pocket-get-access-token ()
  (progn
    (let ((post-data (make-hash-table :test 'equal))
	  (extra-headers (make-hash-table :test 'equal)))
      (puthash 'consumer_key *consumer-key* post-data)
      (puthash 'code *request-token* post-data)
      (puthash 'Host "getpocket.com" extra-headers)
      (puthash 'Content-type "application/x-www-form-urlencoded; charset=UTF-8" extra-headers)
      (puthash 'X-Accept "application/json" extra-headers)
      (web-http-post
       (lambda (con header data)
	 (setq *access-token-and-username*
	       (json-read-from-string data))
	 (message "data received is: %s" data)
	 (el-pocket-save-auth))
       :url *el-pocket-oauth-authorize-url*
       :data post-data
       :extra-headers extra-headers))
    (sleep-for 1)
    (display-message-or-buffer
     "access a-gotten!")))

;; we don't have a request token yet, so request 
;; one, then send the user to oauth/authorize for
;; to authorize this shiz
(defun el-pocket-get-request-token ()
  (let ((post-data (make-hash-table :test 'equal))
	(extra-headers (make-hash-table :test 'equal)))
    (puthash 'consumer_key *consumer-key* post-data)
    (puthash 'redirect_uri "http://www.google.com" post-data)
    (puthash 'Host "getpocket.com" extra-headers)
    (puthash 'Content-type "application/x-www-form-urlencoded; charset=UTF-8" extra-headers)
    (puthash 'X-Accept "application/json" extra-headers)
    (web-http-post
     (lambda (con header data)
       (let ((token (cdr (assoc 'code (json-read-from-string data)))))
	 (setq *request-token* token)
	 (setq *url* (concat "https://getpocket.com/auth/authorize?request_token=" *request-token*))
	 (kill-new *url*)))
     :url *el-pocket-oauth-request-url*
     :data post-data
     :extra-headers extra-headers)
    (sleep-for 1)
    (display-message-or-buffer
     (concat "authorize el-pocket at " *url*
	     " (copied to clipboard)\n"))))

;; do we have access?
(defun access-granted-p ()
  (if *access-token-and-username* t))

(defun access-not-granted ()
  (display-message-or-buffer
   "M-x el-pocket-authorize to get access to pocket"))

;; skeleton function to test getting things from pocket
;; response is printed to *Messages*
;; TODO make this do useful things
(defun el-pocket-get ()
  (if (access-granted-p)
      (let ((post-data (make-hash-table :test 'equal))
	    (extra-headers (make-hash-table :test 'equal)))
	(puthash 'consumer_key *consumer-key* post-data)
	(puthash 'access_token (cdr (assoc 'access_token *access-token-and-username*)) post-data)
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
    (access-not-granted)))

;;oh my gosh
(defun el-pocket-add (url-to-add)
  (interactive
   (list
    (read-string "el-pocket url: ")))
  (if (access-granted-p)
      (let ((post-data (make-hash-table :test 'equal))
	    (extra-headers (make-hash-table :test 'equal)))
	(puthash 'consumer_key *consumer-key* post-data)
	(puthash 'access_token (cdr (assoc 'access_token *access-token-and-username*)) post-data)
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
    (access-not-granted)))

(provide 'el-pocket)

;;; el-pocket.el ends here




