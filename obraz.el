;;; obraz.el --- Emacs assistance for static blogging (Obraz, Jekyll, etc)
;;
;; Copyright (C) 2013 Sergey Astanin
;;
;; License: BSD3
;;


(defvar obraz:post-template
  "---\ntitle: %s\ndate: %s\ntags: %s\nlayout: post\npublished: false\n\n---\n\n"
  "Default template of a new post.")


(defun obraz:insert-post-template (post-title tags-list)
  (interactive)
  (let* ((escaped-title (prin1-to-string post-title))
         (now (format-time-string "%F %T"))
         (tags-str (format "[ %s ]" (mapconcat #'identity tags-list ", ")))
         (header (format obraz:post-template escaped-title now tags-str)))
    (insert header)))


(defun obraz:new-post (blog-path post-title tags)
  "Ask path to blog, post title, tags, then create a new post file."
  (interactive "DBlog location: \nsPost title: \nsTags (white-space separated): ")
  (let* ((file-name (format "_posts/%s-%s.md"
                            (format-time-string "%Y-%m-%d")
                            (downcase (replace-regexp-in-string
                                       "[^A-Za-z0-9]+" "-" post-title))))
         (file-path (expand-file-name file-name blog-path))
         (tags-list (mapcar #'downcase
                            (split-string tags "[^[:alpha:][:digit:]]"))))
    (switch-to-buffer (find-file file-path))
    (if (zerop (length (buffer-string)))
        (obraz:insert-post-template post-title tags-list))))


(provide 'obraz)
