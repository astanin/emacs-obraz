;;; obraz.el --- Managing a statically generated blog (Obraz, Jekyll, etc.)

;; Copyright (C) 2013 Sergey Astanin

;; Author: Sergey Astanin <s.astanin@gmail.com>
;; URL: https://github.com/astanin/obraz.el
;; Version: 1.0-alpha1
;; Keywords: convenience, outlines, wp
;; License: MIT (see below)

;;; Commentary:

;; Obraz.el intends to facilitate use of Obraz static site
;; generator (http://obraz.pirx.ru/), which is mostly compatible
;; with Jekyll.  Hopefully, this package is usable with Jekyll
;; (https://github.com/mojombo/jekyll) too.
;;
;; The package provides several user-visible commands:
;;
;; * obraz-list-posts shows a list of written posts, and allows to
;;   reopen the corresponding files in _posts/ or start writing a new
;;   post.
;;
;;   Default keybindings for the list of posts are: n, p -- next,
;;   previous post, r -- refresh, b -- build, B -- serve locally.
;;
;; * obraz-new-post helps to create a new text file in _posts/ with a
;;   proper file name derived from the current date, and post title.
;;
;; * obraz-build and obraz-serve build and serve current site
;;   respectively.
;;
;; Use `M-x customize-group obraz RET` to set the script location
;; and the post template.
;;

;;; Code:

(require 'button)


(defgroup obraz
  nil
  "Persistent settings of obraz.el package.")


(defcustom obraz:post-template
  "---\ntitle: %s\ndate: %s\ntags: %s\nlayout: post\npublished: false\n\n---\n\n"
  "Default template of a new post."
  :group 'obraz
  :type 'string)


(defcustom obraz:last-blog-location nil
  "Path to the last edited Obraz blog."
  :group 'obraz
  :type 'directory)


(defcustom obraz:obraz-py-path ""
  "/path/to/obraz.py"
  :group 'obraz
  :type 'file)


(define-derived-mode obraz-toc-mode fundamental-mode "obraz-toc"
  "Major mode for displaying a list of posts in an Obraz blog.")


(defun obraz:insert-post-template (post-title tags-list)
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


(defun obraz:save-last-blog-location (blog-path)
  "Update obraz:last-blog-location setting."
  (when (and blog-path (file-accessible-directory-p blog-path))
    (set-variable 'obraz:last-blog-location (expand-file-name blog-path))))


(defun obraz:new-post (blog-path post-title tags)
  "Ask path to blog, post title, tags, then create a new post file."
  (interactive
   (list
    (read-directory-name "Blog location: " obraz:last-blog-location)
    (read-string "Post title: ")
    (read-string "Tags (white-space-separated): ")))
  ;; "DBlog location: \nsPost title: \nsTags (white-space separated): "
  (let* ((file-name (format "_posts/%s-%s.md"
                            (format-time-string "%Y-%m-%d")
                            (downcase (replace-regexp-in-string
                                       "[^A-Za-z0-9]+" "-" post-title))))
         (file-path (expand-file-name file-name blog-path))
         (tags-list (mapcar #'downcase
                            (split-string tags "[^[:alpha:][:digit:]]"))))
    (obraz:save-last-blog-location blog-path)
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


(defun obraz:new-toc-buffer (blog-path)
  "Create a buffer for a new list of posts."
  (let* ((buf (generate-new-buffer (concat "obraz:posts " blog-path))))
    buf))


(defun obraz:find-toc-buffer (blog-path)
  "Find an open buffer with a list of posts."
  (get-buffer (concat "obraz:posts " blog-path)))


(defun obraz:read-list-of-posts (&optional blog-path)
  "(Re)reads the list of posts in reversed chronological order into the current buffer."
  (cl-flet ((get (k alist) (cadr (assoc k alist)))
            (newest-first (a b) (string< (get 'date b) (get 'date a)))
            (trim (s) (replace-regexp-in-string "[ \t\"]+$" ""
                       (replace-regexp-in-string "^[ \t\"]+" "" s))))
    (let* ((blog-path (or blog-path
                          (when (boundp 'obraz:buffer-blog-path) obraz:buffer-blog-path)))
           (posts (obraz:read-posts-meta blog-path))
           (sorted-posts (sort posts #'newest-first)))
      (when blog-path
        (setq buffer-read-only nil)
        (delete-region (point-min) (point-max))
        (dolist (p sorted-posts)
          (let ((label (format "%s  %s\n" (get 'date p) (trim (get 'title p)))))
            (insert-button label
                           'action (lambda (x) (obraz:open-post (button-get x 'file)))
                           'file   (get 'file p)
                           'face   `((:underline nil)))
            (goto-char (point-min))))
        (let ((new-post-label (format "%s  %s\n" "---------- --:--:--" "< Write a new post >")))
          (insert-button new-post-label
                         'action (lambda (x)
                                   (let ((obraz:last-blog-location (button-get x 'blog-path)))
                                     (call-interactively 'obraz:new-post)))
                         'blog-path blog-path
                         'face   `((:underline nil)))
          (goto-char (point-min)))
        (setq buffer-read-only 't)
        (set-buffer-modified-p nil)
        (obraz-toc-mode)))))


(defun obraz:list-posts (blog-path)
  "List existing blog posts in reversed chronological order."
  (interactive
   (list
    (read-directory-name "Blog location: " obraz:last-blog-location)))
  (let* ((buf (or (obraz:find-toc-buffer blog-path)
                  (obraz:new-toc-buffer blog-path)))
         (buf-path (make-local-variable 'obraz:buffer-blog-path)))
    (switch-to-buffer buf)
    (setq obraz:buffer-blog-path blog-path)
    (obraz:read-list-of-posts blog-path)
    (obraz:save-last-blog-location blog-path)))


(defun obraz:build ()
  "Build current blog."
  (interactive)
  (let* ((buffer-path (or (buffer-file-name) obraz:buffer-blog-path))
         (blog-path   (file-name-directory
                       (locate-dominating-file buffer-path "_posts")))
         ;; TODO: quote whitespace in obraz-py-path and blog-path
         (cmd         (mapconcat 'identity
                                 `("python" ,obraz:obraz-py-path
                                   "build"
                                   "-s" ,blog-path
                                   "-d" ,(concat blog-path "/_site/"))
                                 " ")))
    (message cmd)
    (when blog-path
       (compile cmd))))


(defun obraz:serve ()
  "Build and serve current blog."
  (interactive)
  (let* ((buffer-path (or (buffer-file-name) obraz:buffer-blog-path))
         (blog-path   (file-name-directory
                       (locate-dominating-file buffer-path "_posts")))
         ;; TODO: quote whitespace in obraz-py-path and blog-path
         (cmd         (mapconcat 'identity
                                 `("python" ,obraz:obraz-py-path
                                   "serve"
                                   "-w"
                                   "-s" ,blog-path
                                   "-d" ,(concat blog-path "/_site/"))
                                 " ")))
    (message cmd)
    (when blog-path
       (compile cmd))))


(add-hook 'obraz-toc-mode-hook
          (lambda ()
            (define-key obraz-toc-mode-map "n" 'next-line)
            (define-key obraz-toc-mode-map "p" 'previous-line)
            (define-key obraz-toc-mode-map "r" (lambda ()
                                                 (interactive)
                                                 (let ((lines (line-number-at-pos (point))))
                                                   (obraz:read-list-of-posts)
                                                   (forward-line (- lines 1)))))
            (if (fboundp 'hl-line-mode)
                (hl-line-mode 't))
            (toggle-truncate-lines 't)))


(provide 'obraz)

;; Copyright (C) 2013 Sergey Astanin

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; obraz.el ends here
