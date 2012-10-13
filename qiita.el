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
  (append
   `(("page" . ,(number-to-string (or page qiita-api-page))))
   (when token `("token" . ,token))))

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
             'uuid (qiita-api-post-items (json-encode params))))
      ;; insert
      (save-excursion
        (goto-char (point-max))
        (insert (format  "\

<!--
Local Variables:
qiita-file-uuid: %S
End:
-->
" qiita-file-uuid))
        )
      )))

(defun qiita-browse-url (&optional uuid)
  (interactive)
  (setq  uuid (or uuid qiita-file-uuid))
  (unless uuid (error "uuid is empty"))
  (browse-url (concat "https://qiita.com/items/" uuid)))

(defun qiita-tags-items (tags)
  (mapcar (lambda (item)
            (message "uuid:%s" (assoc-default 'uuid item))
            (message "title:%s" (assoc-default 'uuid item))
            )
          (append (qiita-api-get-tags-items tags) nil))
  )

(defun qiita-parse-tags (json-params)
  (mapconcat (lambda (tag)
               ;; (message "uuid:%s" (assoc-default 'uuid item))
               ;; (message "title:%s" (assoc-default 'title item))
               (concat
                "#" (url-unhex-string (assoc-default 'url_name tag))
                ;; (assoc-default 'url_name item) " "
                ;; (assoc-default 'title item) "\n"
                ;; (format "%S" (assoc-default 'tags item))))
                ))
             (append json-params nil) ;; (qiita-get-items qiita-token)
             " "
             ))

;; (qiita-tags-items "api")
(defun qiita-my-items ()
  (unless qiita-token
    (setq qiita-token (qiita-get-token)))
  (mapcar (lambda (item)
            ;; (message "uuid:%s" (assoc-default 'uuid item))
            ;; (message "title:%s" (assoc-default 'title item))
            (cons (concat
                   (assoc-default 'url_name item) " "
                   (assoc-default 'title item)
                   (qiita-parse-tags (assoc-default 'tags item))
                   "\n"
                   (let* ((string (assoc-default 'body item))
                          (strings (split-string string "\n")))
                     ;; (when (< (window-width) (length string))
                     ;;   (setq string (substring string 0 (window-width))))
                     (if (< 2 (length strings))
                         (setq string (concat
                                       (nth 0 strings) "\n" (nth 0 strings)))
                         )
                     (replace-regexp-in-string "<.*?>" "" string)
                     ;; string
                     ;; string
                     )
                   ;; (substring "hogehogehogehgoe" 0 100)
                   ;; (message )
                   ;; (substring 0 (assoc-default 'body item) 10)
                   ;; (substring  0 (window-width) (assoc-default 'body item))
                   ;; (format "%s" (assoc-default 'user))
                   ;; (format "%S" (assoc-default 'tags item))
                   )
                  (assoc-default 'uuid item))
            )
          (append test-data nil) ;; (qiita-get-items qiita-token)
          ))
(require 'markdown-mode)
;; (markdown-mode)

;; (qiita-parse-tags [((versions . []) (following . :json-false) (follower_count . 1) (item_count . 13) (icon_url . "http://qiita.com/icons/thumb/missing.png") (url_name . "api") (name . "api"))])

(defvar anything-c-source-qiita
  '((name . "qiita")
    (candidates . (lambda () (qiita-my-items)))
    (action . qiita-browse-url)
    (multiline)
    ))

(defun qiita-anyhing ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-qiita)
   "*qiita-anything*"))

;; (qiita-my-items)
(setq test-data
[((stocked . :json-false) (created_at_as_seconds . 1350099389) (private . :json-false) (gist_url) (url . "http://qiita.com/items/bb2462c263e2e9f95de6") (comment_count . 0) (stock_users . []) (stock_count . 0) (tags . [((versions . []) (following . :json-false) (follower_count . 1) (item_count . 13) (icon_url . "http://qiita.com/icons/thumb/missing.png") (url_name . "api") (name . "api"))]) (updated_at_in_words . "1時間前") (created_at_in_words . "1時間前") (updated_at . "2012-10-13 12:36:29 +0900") (created_at . "2012-10-13 12:36:29 +0900") (body . "<p>も一度新規</p>
") (title . "title2 ") (user (following . :json-false) (profile_image_url . "https://si0.twimg.com/profile_images/1681702121/icon_048_normal.png") (url_name . "uk_ar") (name . "uk")) (uuid . "bb2462c263e2e9f95de6") (id . 9883)) ((stocked . :json-false) (created_at_as_seconds . 1350099265) (private . :json-false) (gist_url) (url . "http://qiita.com/items/25d36a4025c44cece36f") (comment_count . 0) (stock_users . []) (stock_count . 0) (tags . [((versions . []) (following . :json-false) (follower_count . 1) (item_count . 13) (icon_url . "http://qiita.com/icons/thumb/missing.png") (url_name . "api") (name . "api"))]) (updated_at_in_words . "1時間前") (created_at_in_words . "2時間前") (updated_at . "2012-10-13 12:35:37 +0900") (created_at . "2012-10-13 12:34:25 +0900") (body . "<p>新規投稿<br>
修正</p>

<p>&lt;!--<br>
Local Variables:<br>
qiita-file-uuid: &quot;25d36a4025c44cece36f&quot;<br>
End:<br>
--&gt;</p>
") (title . "title ") (user (following . :json-false) (profile_image_url . "https://si0.twimg.com/profile_images/1681702121/icon_048_normal.png") (url_name . "uk_ar") (name . "uk")) (uuid . "25d36a4025c44cece36f") (id . 9882)) ((stocked . :json-false) (created_at_as_seconds . 1350085248) (private . :json-false) (gist_url) (url . "http://qiita.com/items/ebb63c695e194efb6da8") (comment_count . 0) (stock_users . ["katsuren"]) (stock_count . 1) (tags . [((versions . []) (following . :json-false) (follower_count . 191) (item_count . 94) (icon_url . "http://qiita.com/system/tags/icons/000/000/187/thumb/C_Sharp.png?1328867786") (url_name . "c%23") (name . "c#")) ((versions . []) (following . t) (follower_count . 1165) (item_count . 182) (icon_url . "http://qiita.com/system/tags/icons/000/000/007/thumb/emacs.jpeg?1316130821") (url_name . "Emacs") (name . "Emacs")) ((versions . []) (following . :json-false) (follower_count . 1) (item_count . 13) (icon_url . "http://qiita.com/icons/thumb/missing.png") (url_name . "api") (name . "api"))]) (updated_at_in_words . "2時間前") (created_at_in_words . "5時間前") (updated_at . "2012-10-13 12:24:59 +0900") (created_at . "2012-10-13 08:40:48 +0900") (body . "<h1>hoge</h1>

<p>新規<br>
更新<br>
2回目の更新<br>
3回目の更新<br>
4回目の更新<br>
5回目の更新<br>
6回目の更新<br>
ファイルからuuidの読み込み<br>
uuidのコメントアウト<br>
markdownのコメントアウト<br>
OK?dayo</p>

<p>&lt;!--<br>
Local Variables:<br>
qiita-file-uuid: &quot;ebb63c695e194efb6da8&quot;<br>
End:<br>
 --&gt;</p>
") (title . "APIのテスト ") (user (following . :json-false) (profile_image_url . "https://si0.twimg.com/profile_images/1681702121/icon_048_normal.png") (url_name . "uk_ar") (name . "uk")) (uuid . "ebb63c695e194efb6da8") (id . 9877)) ((created_at_as_seconds . 1350085035) (private . t) (gist_url) (url . "http://qiita.com/private/18694bfa0a795bdc39d9") (comment_count . 0) (stock_users . []) (stock_count . 0) (tags . [((versions . ["1.3" "1.2.4"]) (following . :json-false) (follower_count . 0) (item_count . 0) (icon_url . "http://qiita.com/icons/thumb/missing.png") (url_name . "FOOBAR") (name . "FOOBAR"))]) (updated_at_in_words . "5時間前") (created_at_in_words . "5時間前") (updated_at . "2012-10-13 08:37:15 +0900") (created_at . "2012-10-13 08:37:15 +0900") (body . "<p>foooooooooooooooo</p>
") (title . "にほんご") (user (following . :json-false) (profile_image_url . "https://si0.twimg.com/profile_images/1681702121/icon_048_normal.png") (url_name . "uk_ar") (name . "uk")) (uuid . "18694bfa0a795bdc39d9") (id . 9876)) ((stocked . :json-false) (created_at_as_seconds . 1350084207) (private . :json-false) (gist_url) (url . "http://qiita.com/items/4e57a712c71512bc4a2b") (comment_count . 0) (stock_users . ["katsuren"]) (stock_count . 1) (tags . [((versions . []) (following . :json-false) (follower_count . 191) (item_count . 94) (icon_url . "http://qiita.com/system/tags/icons/000/000/187/thumb/C_Sharp.png?1328867786") (url_name . "c%23") (name . "c#")) ((versions . []) (following . t) (follower_count . 1165) (item_count . 182) (icon_url . "http://qiita.com/system/tags/icons/000/000/007/thumb/emacs.jpeg?1316130821") (url_name . "Emacs") (name . "Emacs")) ((versions . []) (following . :json-false) (follower_count . 1) (item_count . 13) (icon_url . "http://qiita.com/icons/thumb/missing.png") (url_name . "api") (name . "api"))]) (updated_at_in_words . "6時間前") (created_at_in_words . "6時間前") (updated_at . "2012-10-13 08:23:27 +0900") (created_at . "2012-10-13 08:23:27 +0900") (body . "<h1>hoge</h1>
") (title . "APIのテスト ") (user (following . :json-false) (profile_image_url . "https://si0.twimg.com/profile_images/1681702121/icon_048_normal.png") (url_name . "uk_ar") (name . "uk")) (uuid . "4e57a712c71512bc4a2b") (id . 9875)) ((stocked . :json-false) (created_at_as_seconds . 1349838675) (private . :json-false) (gist_url) (url . "http://qiita.com/items/1803278df7c84504c633") (comment_count . 0) (stock_users . []) (stock_count . 0) (tags . [((versions . []) (following . :json-false) (follower_count . 1) (item_count . 13) (icon_url . "http://qiita.com/icons/thumb/missing.png") (url_name . "api") (name . "api"))]) (updated_at_in_words . "14時間前") (created_at_in_words . "3日前") (updated_at . "2012-10-13 00:26:17 +0900") (created_at . "2012-10-10 12:11:15 +0900") (body . "<p>apiのテストのための投稿です。</p>
") (title . "English") (user (following . :json-false) (profile_image_url . "https://si0.twimg.com/profile_images/1681702121/icon_048_normal.png") (url_name . "uk_ar") (name . "uk")) (uuid . "1803278df7c84504c633") (id . 9696))])

;; (* 0.5 0.8)

;; (message "%S" (qiita-get-items qiita-token))
(provide 'qiita)
