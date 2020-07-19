;; publish.el --- Publish org-mode project on Gitlab Pages
;; Author: Sachin Patil <iclcoolster@gmail.com, psachin@redhat.com>

;;; Commentary:
;; This script will convert the org-mode files in this directory into
;; html.

;;; Code:
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-refresh-contents)
(package-install 'htmlize)
(package-install 'org-plus-contrib)
(package-install 'ox-reveal)
;; Don't want to invoke insert-shebang locally
(remove-hook 'find-file-hook 'insert-shebang)

(require 'org)
(require 'ox-publish)
;; (require 'htmlize)
;; (require 'ox-html)
;; (require 'ox-rss)
(require 'ox-reveal)

;; setting to nil, avoids "Author: x" at the bottom
(setq org-export-with-section-numbers nil
      org-export-with-smart-quotes t
      org-export-with-toc nil)

(defvar this-date-format "%b %d, %Y")

(setq org-html-divs '((preamble "header" "top")
                      (content "main" "content")
                      (postamble "footer" "postamble"))
      org-html-container-element "section"
      org-html-metadata-timestamp-format this-date-format
      org-html-checkbox-type 'html
      org-html-html5-fancy t
      org-html-validation-link t
      org-html-doctype "html5"
      org-html-htmlize-output-type 'css
      org-src-fontify-natively t)


(defvar me/website-html-head
  "<link rel='icon' type='image/x-icon' href='/images/favicon.jpg'/>
<meta name='viewport' content='width=device-width, initial-scale=1'>
<link rel='stylesheet' href='https://code.cdn.mozilla.net/fonts/fira.css'>
<link rel='stylesheet' href='/css/site.css?v=2' type='text/css'/>
<link rel='stylesheet' href='/css/custom.css' type='text/css'/>
<link rel='stylesheet' href='/css/syntax-coloring.css' type='text/css'/>")

(defun me/website-html-preamble (plist)
  "PLIST: An entry."
  (if (org-export-get-date plist this-date-format)
        (plist-put plist
             :subtitle (format "Published on %s by %s."
                               (org-export-get-date plist this-date-format)
                               (car (plist-get plist :author)))))
  ;; Preamble
  (with-temp-buffer
    (insert-file-contents "../html-templates/preamble.html") (buffer-string)))

(defun me/website-html-postamble (plist)
  "PLIST."
  (concat (format
           (with-temp-buffer
             (insert-file-contents "../html-templates/postamble.html") (buffer-string))
           (format-time-string this-date-format (plist-get plist :time)) (plist-get plist :creator))))


(defvar site-attachments
  (regexp-opt '("jpg" "jpeg" "gif" "png" "svg"
                "ico" "cur" "css" "js" "woff" "html" "pdf"))
  "File types that are published as static files.")


(defun me/org-sitemap-format-entry (entry style project)
  "Format posts with author and published data in the index page.

ENTRY: file-name
STYLE:
PROJECT: `posts in this case."
  (cond ((not (directory-name-p entry))
         (format "*[[file:%s][%s]]*
                 #+HTML: <p class='pubdate'>by %s on %s.</p>"
                 entry
                 (org-publish-find-title entry project)
                 (car (org-publish-find-property entry :author project))
                 (format-time-string this-date-format
                                     (org-publish-find-date entry project))))
        ((eq style 'tree) (file-name-nondirectory (directory-file-name entry)))
        (t entry)))


(defun me/org-reveal-publish-to-html (plist filename pub-dir)
  "Publish an org file to reveal.js HTML Presentation.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory. Returns output file name."
  (let ((org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"))
    (org-publish-org-to 'reveal filename ".html" plist pub-dir)))

(setq org-publish-project-alist
      `(("posts"
         :base-directory "posts"
         :base-extension "org"
         :recursive t
         :publishing-function org-html-publish-to-html
         :publishing-directory "./public"
         :exclude ,(regexp-opt '("README.org" "draft" "404.org"))
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Blog Index"
         :sitemap-format-entry me/org-sitemap-format-entry
         :sitemap-style list
         :sitemap-sort-files anti-chronologically
         :html-link-home "/"
         :html-link-up "/"
         :html-head-include-scripts t
         :html-head-include-default-style nil
         :html-head ,me/website-html-head
         :html-preamble me/website-html-preamble
         :html-postamble me/website-html-postamble)
        ("about"
         :base-directory "about"
         :base-extension "org"
         :exclude ,(regexp-opt '("README.org" "draft"))
         :index-filename "index.org"
         :recursive nil
         :publishing-function org-html-publish-to-html
         :publishing-directory "./public/about"
         :html-link-home "/"
         :html-link-up "/"
         :html-head-include-scripts t
         :html-head-include-default-style nil
         :html-head ,me/website-html-head
         :html-preamble me/website-html-preamble
         :html-postamble me/website-html-postamble)
        ("gureSagardoa"
         :base-directory "gureSagardoa"
         :base-extension "org"
         :exclude ,(regexp-opt '("README.org" "draft"))
         :sitemap-filename "index.org"
         :recursive t
         :auto-sitemap nil
         :publishing-function org-html-publish-to-html
         :publishing-directory "./public/gureSagardoa"
         :html-link-home "/"
         :html-link-up "/"
         :html-head-include-scripts t
         :html-head-include-default-style nil
         :html-head ,me/website-html-head
         :html-preamble me/website-html-preamble
         :html-postamble me/website-html-postamble)
        ("photography"
         :base-directory "photography"
         :base-extension "org"
         :exclude ,(regexp-opt '("README.org" "draft"))
         :index-filename "index.org"
         :recursive nil
         :publishing-function org-html-publish-to-html
         :publishing-directory "./public/photography"
         :html-link-home "/"
         :html-link-up "/"
         :html-head-include-scripts t
         :html-head-include-default-style nil
         :html-head ,me/website-html-head
         :html-preamble me/website-html-preamble
         :html-postamble me/website-html-postamble)
        ("horology"
         :base-directory "horology"
         :base-extension "org"
         :exclude ,(regexp-opt '("README.org" "draft"))
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Horology"
         :sitemap-format-entry me/org-sitemap-format-entry
         :sitemap-sort-files anti-chronologically
         :recursive nil
         :publishing-function org-html-publish-to-html
         :publishing-directory "./public/horology"
         :html-link-home "/"
         :html-link-up "/horology"
         :html-head-include-scripts t
         :html-head-include-default-style nil
         :html-head ,me/website-html-head
         :html-preamble me/website-html-preamble
         :html-postamble me/website-html-postamble)
        ("slides"
         :base-directory "slides"
         :base-extension "org"
         :exclude ,(regexp-opt '("README.org" "draft"))
         :index-filename "index.org"
         :recursive t
         :publishing-function me/org-reveal-publish-to-html
         :publishing-directory "./public/slides/")
        ("css"
         :base-directory "./css"
         :base-extension "css"
         :publishing-directory "./public/css"
         :publishing-function org-publish-attachment
         :recursive t)
        ("images"
         :base-directory "./images"
         :base-extension ,site-attachments
         :publishing-directory "./public/images"
         :publishing-function org-publish-attachment
         :recursive t)
        ("assets"
         :base-directory "./assets"
         :base-extension ,site-attachments
         :publishing-directory "./public/assets"
         :publishing-function org-publish-attachment
         :recursive t)
        ("rss"
         :base-directory "posts"
         :base-extension "org"
         :html-link-home "https://psachin.gitlab.io/"
         :rss-link-home "https://psachin.gitlab.io/"
         :html-link-use-abs-url t
         :rss-extension "xml"
         :publishing-directory "./public"
         :publishing-function (org-rss-publish-to-rss)
         :section-number nil
         :exclude ".*"
         :include ("index.org")
         :table-of-contents nil)
        ("all" :components ("posts" "about" "slides" "gureSagardoa" "horology" "css" "images" "assets" "rss"))))

(provide 'publish)
;;; publish.el ends here
