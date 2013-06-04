;;; obraz.el --- Emacs assistance for static blogging (Obraz, Jekyll, etc)
;;
;; Copyright (C) 2013 Sergey Astanin
;;
;; License: BSD3
;;


(require 'button)


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


(defun obraz:open-post (file-path &optional post-title tags-list)
  "Open a new or an existing post file."
  (switch-to-buffer (find-file file-path))
  (if (and post-title
           (zerop (length (buffer-string))))
      (obraz:insert-post-template post-title tags-list)))


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
    (obraz:open-post file-path post-title tags-list)))


(defun obraz:parse-header (header)
  (save-match-data
    (let* ((title (and (string-match "title: \\([^\r\n]+\\)" header)
                       (match-string 1 header)))
           (date  (and (string-match "date: \\([^\r\n]+\\)" header)
                       (match-string 1 header))))
      `((title ,title)
        (date  ,date)))))


(defun obraz:read-header (file-path)
  "Read YAML header, return an assoc-list."
  (let ((header-str
         (if (file-readable-p file-path)
             (with-temp-buffer
               (insert-file-contents file-path)
               (re-search-forward "^---" 1024 'noerror 2)
               (buffer-substring-no-properties (point-min) (line-end-position)))
           "")))
    (cons `(file ,file-path) (obraz:parse-header header-str))))


(defun obraz:fix-date (post)
  "If date is missing, guess it from the file name."
  (if (not (cadr (assoc 'date post)))
      (save-match-data
        (let* ((file-name (file-name-nondirectory (cadr (assoc 'file post))))
               (time-stamp (and (string-match "\\([0-9-]+[0-9]\\).*" file-name)
                                (match-string 1 file-name))))
          (cons `(date ,(concat time-stamp " 00:00:00")) post)))
    post))


(defun obraz:read-posts-meta (blog-path)
  "Read YAML headers of all posts in blog-path."
  (let* ((posts-path (concat blog-path "/_posts/"))
         (files (directory-files posts-path 't "[:digit:]+.*\.md$"))
         (posts (mapcar #'obraz:read-header files)))
    (mapcar #'obraz:fix-date posts)))


(defun obraz:list-posts (blog-path)
  "List existing blog posts in reversed chronological order."
  (interactive "DBlog location: ")
  (cl-flet ((get (k alist) (cadr (assoc k alist)))
            (newest-first (a b) (string< (get 'date b) (get 'date a)))
            (trim (s) (replace-regexp-in-string "[ \t\"]+$" ""
                       (replace-regexp-in-string "^[ \t\"]+" "" s))))
    (let* ((posts (obraz:read-posts-meta blog-path))
           (sorted-posts (sort posts #'newest-first))
           (buf(generate-new-buffer "obraz:posts")))
      (switch-to-buffer buf)
      (dolist (p sorted-posts)
        (let ((label (format "%s  %s\n" (get 'date p) (trim (get 'title p)))))
          (insert-button label
                         'action (lambda (x) (obraz:open-post (button-get x 'file)))
                         'file   (get 'file p)
                         'face   `((:underline nil)))
          (goto-char (point-min))))
      (read-only-mode 't)
      (set-buffer-modified-p nil)
      (if (fboundp 'hl-line-mode)
          (hl-line-mode 't))
      (toggle-truncate-lines 't))))


(provide 'obraz)
