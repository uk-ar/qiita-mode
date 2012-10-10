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

(defun qiita-get-json (uri &rest args)
  ;; (with-temp-buffer
  (with-current-buffer (get-buffer-create "*qiita*")
    (erase-buffer)
    (apply 'call-process "curl" nil (current-buffer) nil uri "-s" args)
    (beginning-of-buffer)
    (prog1 (json-read)
      )
    ;; (buffer-string)
    ))

(defun qiita-get-token ()
  (qiita-get-json "https://qiita.com/api/v1/auth"
                  "-d" (format "url_name=%s" qiita-user)
                  "-d" (format "password=%s" qiita-password))
  )
;; (qiita-get-json "https://qiita.com/api/v1/rate_limit")
;; (qiita-get-token)

(provide 'qiita)

