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

(defcustom qiita-user nil
  "User name for qiita")

(defcustom qiita-password nil
  "Password for qiita")

(defvar qiita-token nil)

(defconst qiita-base-url "https://qiita.com/api/v1")

(defun qiita-retrieve-json (api)
  (with-current-buffer
      (url-retrieve-synchronously
       (concat qiita-base-url api))
    (goto-char (point-min))
    ;; furl--get-response-body
    (search-forward "\n\n" nil t)
    (narrow-to-region (point) (point-max))
    (json-read)
    ))

(defun qiita-api-rate-limit ()
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("Accept" . "application/json"))))
    (qiita-retrieve-json "/rate_limit")
    ))

(defun qiita-api-auth ()
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Content-Type" . "application/x-www-form-urlencoded")
           ("Accept" . "application/json")))
        (url-request-data (format "url_name=%s&password=%s"
                                  qiita-user qiita-password)))
    (qiita-retrieve-json "/auth")
    ))

(defun qiita-get-token ()
  (assoc-default 'token
                 (qiita-api-auth)))

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

;; (qiita-api-post-items
;;  (json-encode '((tweet . :json-false) (gist . t) (private . t) (tags . [((versions . [1.2 1.3]) (name . "FOOBAR"))]) (body . "foooooooooooooooo") (title . "にほんご"))))

(provide 'qiita)

