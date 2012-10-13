;;; qiita.el --- qiita client for emacs

;;-------------------------------------------------------------------
;;
;; Copyright (C) 2012 Yuuki Arisawa
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;-------------------------------------------------------------------

;; Author: Yuuki Arisawa <yuuki.ari@gmail.com>
;; URL: https://github.com/uk-ar/qiita.el
;; Twitter: http://twitter.com/uk_ar
;; Blog: http://d.hatena.ne.jp/uk-ar
;;
;; Created: Oct 10, 2012
;; Version: 0.1
;; Keywords: web

;;; Commentary:
(require 'json)
(require 'url)

(defcustom qiita-user nil
  "User name for qiita")

(defcustom qiita-password nil
  "Password for qiita")

(defvar qiita-token nil)

(defconst qiita-base-url "https://qiita.com/api/v1")

(defcustom qiita-connection-type 'emacs
  "emacs curl")

(defun qiita-curl-retrieve-synchronously (url)
  (with-current-buffer (get-buffer-create "*qiita*")
    (erase-buffer)
    (let ((args))
      (if url-request-method
          (setq args (append args `("-X" ,url-request-method))))
      (if url-request-extra-headers
          (setq args
                (apply 'append args
                       (mapcar
                        (lambda (elem)
                          (list "-H"
                                (format "%s: %s" (car elem) (cdr elem))))
                        url-request-extra-headers))))
      (if url-request-data
          (setq args (append args `("-d" ,url-request-data))))
      (unless
          (zerop (apply 'call-process "curl" nil (current-buffer) nil url
                        "--trace-ascii" "log";for debug
                        "-s" args))
        (error "connection error at curl")
        )
      ;; for debug
      ;;(apply 'call-process "echo" nil (current-buffer) nil url "-s" args)
      )
    (goto-char (point-min))
    (buffer-string)
    ))

(defun qiita-url-retrieve-synchronously (url)
  (with-current-buffer
      (url-retrieve-synchronously url)
    (goto-char (point-min))
    ;; furl--get-response-body
    (search-forward "\n\n" nil t)
    (narrow-to-region (point) (point-max))
    ;; for debug
    (print (buffer-string))
    (buffer-string)
    ))

(defun qiita-retrieve-json (api &optional no-read-p)
  (let ((url (concat qiita-base-url api))
        (string))
    (setq string
          (cond
           ((eq qiita-connection-type 'emacs)
            (qiita-url-retrieve-synchronously url))
           ((eq qiita-connection-type 'curl)
            (qiita-curl-retrieve-synchronously url))
           (t (error "qiita-connection-type"))))
    (let ((ret-json
           (funcall (if no-read-p 'identity 'json-read-from-string) string)))
      (message "OK")
      (when (assoc-default 'error ret-json)
        (error "Qiita returns error %S" (assoc-default 'error ret-json)))
      ret-json
      )))

(defun qiita-define-shorten-name (symbol)
  (defalias
    (intern
     (replace-regexp-in-string "-post\\|-get" "" (symbol-name symbol)))
    symbol))

(defun qiita-api-post-auth ()
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Content-Type" . "application/x-www-form-urlencoded")
           ("Accept" . "application/json")))
        (url-request-data (format "url_name=%s&password=%s"
                                  qiita-user qiita-password)))
    (qiita-retrieve-json "/auth")
    ))
(qiita-define-shorten-name 'qiita-api-post-auth)

(defun qiita-get-token ()
  (assoc-default 'token
                 (qiita-api-auth)))

(defun qiita-api-get-items (&optional uuid token)
  (qiita-get-method (concat "/items" (when uuid (concat "/" uuid)))
                    (when token `(("token" . ,token)))))

(defun qiita-get-item (uuid &optional token)
  (qiita-api-get-items uuid token))

(defun qiita-get-items (&optional token)
  (qiita-api-get-items nil token))

(defun qiita-api-post-items (json-param)
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("Accept" . "application/json")))
        (url-request-data json-param))
    (unless qiita-token
      (setq qiita-token (qiita-get-token)))
    (qiita-retrieve-json
     (format "/items?token=%s" qiita-token))
    ))

;; input type json?
(defun qiita-api-put-items (uuid json-param)
  (let ((url-request-method "PUT")
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("Accept" . "application/json")))
        (url-request-data json-param))
    (unless qiita-token
      (setq qiita-token (qiita-get-token)))
    (qiita-retrieve-json
     (concat "/items"
             "/" uuid
             "?token=" qiita-token) t)))

(defun qiita-api-delete-items (uuid)
  (let ((url-request-method "DELETE")
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("Accept" . "application/json")))
        )
    (unless qiita-token
      (setq qiita-token (qiita-get-token)))
    (qiita-retrieve-json
     (concat "/items"
             "/" uuid
             "?token=" qiita-token) t)))

(defun qiita-api-get-stocks ()
  (unless qiita-token
    (setq qiita-token (qiita-get-token)))
  (qiita-get-method "/stocks" `(("token" . ,qiita-token))))

;; is this restfull?
(defun qiita-api-put-items-stock (uuid)
  (let ((url-request-method "PUT")
        (url-request-extra-headers
         `(("Accept" . "application/json")
           ("Content-length" . "0"))))
    (unless qiita-token
      (setq qiita-token (qiita-get-token)))
    (qiita-retrieve-json
     (concat "/items"
             "/" uuid
             "/stock"
             "?token=" qiita-token)
     t)))

;; Cannot work?
(defun qiita-api-delete-items-unstock (uuid)
  (let ((url-request-method "DELETE")
        (url-request-extra-headers
         `(("Accept" . "application/json"))))
    (unless qiita-token
      (setq qiita-token (qiita-get-token)))
    (qiita-retrieve-json
     (concat "/items"
             "/" uuid
             "/unstock"
             "?token=" qiita-token)
     t)))

(defcustom qiita-api-per-page 20
  "items per page")

(defcustom qiita-api-page 0
  "page")

(defun qiita-get-method (route &optional params)
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("Accept" . "application/json")))
        (url-request-data nil))
    (unless (eq qiita-api-per-page 20)
      (add-to-list 'params
                   `("per_page" . ,qiita-per-page)))
    (qiita-retrieve-json
     (concat route
             (when params
               (concat "?" (mapconcat
                            (lambda (param)
                              (concat (url-hexify-string (car param))
                                      "="
                                      (url-hexify-string (cdr param))))
                            params
                            "&")))))))

(defun qiita-api-page-token-params (page token)
  (list
   `("page" . ,(number-to-string (or page qiita-api-page)))
   (when token  `("token" . ,token))))

(defun qiita-api-get-rate-limit (&optional token)
  (qiita-get-method "/rate_limit" (when token `(("token" . ,token)))))
(qiita-define-shorten-name 'qiita-api-get-rate-limit)

(defun qiita-api-get-user-items (user &optional page token)
  (qiita-get-method (concat "/users"
                            "/" user
                            "/items")
                    (qiita-api-page-token-params page token)))
(qiita-define-shorten-name 'qiita-api-get-user-items)

(defun qiita-api-get-user-stocks (user &optional page token)
  (qiita-get-method (concat "/users"
                            "/" user
                            "/stocks")
                    (qiita-api-page-token-params page token)))
(qiita-define-shorten-name 'qiita-api-get-user-stocks)

;; todo report user as tag
(defun qiita-api-get-tags-items (tags &optional page token)
  (qiita-get-method (concat "/tags"
                            "/" tags
                            "/items")
                    (qiita-api-page-token-params page token)))
(qiita-define-shorten-name 'qiita-api-get-tags-items)

(defun qiita-api-get-tags (&optional page token)
  (qiita-get-method (concat "/tags")
                    (qiita-api-page-token-params page token)))
(qiita-define-shorten-name 'qiita-api-get-tags)

(defun qiita-api-get-search (query &optional stocked-p page token)
  (qiita-get-method (concat "/search")
                    (append (qiita-api-page-token-params page token)
                            `(("q" . ,query)
                              ,(when stocked-p `("stocked" . ,stocked-p)))
                            )))
(qiita-define-shorten-name 'qiita-api-get-search)

(defvar qiita-enable-tweet nil)
(make-variable-buffer-local 'qiita-enable-tweet)

(defvar qiita-enable-gist nil)
(make-variable-buffer-local 'qiita-enable-gist)

(defvar qiita-enable-private nil)
(make-variable-buffer-local 'qiita-enable-private)

(defvar qiita-file-uuid nil)
(make-variable-buffer-local 'qiita-file-uuid)

(defun qiita-post-buffer ()
  (interactive)
  (let ((params))
    (save-excursion
      (goto-char (point-min))
      (add-to-list
       'params
       `(title . ,(car (split-string (buffer-substring-no-properties
                                      (point-min) (point-at-eol)) "#"))))
      (add-to-list
       'params
       `(tags . ,(vconcat
                  (loop
                   with ret
                   while (re-search-forward " #\\(\\S-*\\)"
                                            (point-at-eol) t)
                   do (push `((name . ,(match-string-no-properties 1))) ret)
                   finally (return (or ret (error "no tags")))
                   ))))
      (end-of-line)
      (add-to-list
       'params
       `(body . ,(buffer-substring-no-properties (1+ (point)) (point-max))))
      (add-to-list 'params `(private . ,(or qiita-enable-private :json-false)))
      (add-to-list 'params `(tweet . ,(or qiita-enable-tweet :json-false)))
      (add-to-list 'params `(gist . ,(or qiita-enable-gist :json-false)))
      )
    (if qiita-file-uuid
        (qiita-api-put-items qiita-file-uuid (json-encode params))
      (setq qiita-file-uuid
            (assoc-default
             'uuid (qiita-api-post-items (json-encode params)))))))

(provide 'qiita)
