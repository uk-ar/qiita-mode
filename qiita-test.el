;; (qiita-api-put-items-stock "1803278df7c84504c633")
;; (qiita-api-delete-items-unstock "1803278df7c84504c633")

;; (qiita-api-get-items nil (qiita-get-token));; "2df91279f2f3bed130fc"
;; (qiita-get-items (qiita-get-token))
;; (qiita-api-put-items "1803278df7c84504c633"
;; (json-encode '((title . "English"))))
;; (qiita-api-delete-items "d2aae00d3f7a82eef501");; no-error
;; "4ba49f6af73e9b38e04e"

;; (setq url-debug t)
;; (qiita-api-get-stocks)
;; (qiita-api-get-items)
;; (qiita-api-get-user-items "uk_ar" 1)
;; (qiita-api-get-user-items "uk_ar" 1)
;; (qiita-api-rate-limit)
;; (qiita-api-rate-limit)
;; (qiita-api-get-user-items "uk_ar" 2)
;; (qiita-api-get-user-stocks "uk_ar")
;; (qiita-api-get-tags-items "emacs")
;; (qiita-api-get-tags)
;; (qiita-api-auth)
;; (qiita-get-token)
;; (assoc-default 'uuid
;;                (qiita-api-post-items
;;                 (json-encode '((tweet . :json-false) (gist . t) (private . t) (tags . [((versions . [1.2 1.3]) (name . "FOOBAR"))]) (body . "foooooooooooooooo") (title . "にほんご")))))
