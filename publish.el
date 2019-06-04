;; publish.el --- Publish org-mode project on Gitlab Pages
;; Author: Sachin

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

(defvar psachin-date-format "%b %d, %Y")

(setq org-html-divs '((preamble "header" "top")
                      (content "main" "content")
                      (postamble "footer" "postamble"))
      org-html-container-element "section"
      org-html-metadata-timestamp-format psachin-date-format
      org-html-checkbox-type 'html
      org-html-html5-fancy t
      org-html-validation-link t
      org-html-doctype "html5"
      org-html-htmlize-output-type 'css
      org-src-fontify-natively t)


(defvar psachin-website-html-head
  "<link rel='icon' type='image/x-icon' href='/images/favicon.jpg'/>
<meta name='viewport' content='width=device-width, initial-scale=1'>
<link rel='stylesheet' href='https://code.cdn.mozilla.net/fonts/fira.css'>
<link rel='stylesheet' href='/css/site.css?v=2' type='text/css'/>
<link rel='stylesheet' href='/css/custom.css' type='text/css'/>
<link rel='stylesheet' href='/css/syntax-coloring.css' type='text/css'/>")

(defun psachin-website-html-preamble (plist)
  "PLIST: An entry."
  ;; Skip adding subtitle to the post if :KEYWORDS don't have 'post' has a
  ;; keyword
  (when (string-match-p "post" (format "%s" (plist-get plist :keywords)))
    (plist-put plist
	       :subtitle (format "Published on %s by %s."
				 (org-export-get-date plist psachin-date-format)
				 (car (plist-get plist :author)))))

  ;; Below content will be added anyways
"<div class='intro'>
<img src='/images/about/profile.png' alt='Sachin Patil' class='no-border'/>
<h1>Sachin</h1>
<p>Free Software developer & Emacser</p>
</div>

<div class='nav'>
<ul>
<li><a href='/'>Blog</a>.</li>
<li><a href='http://gitlab.com/psachin'>GitLab</a>.</li>
<li><a href='http://github.com/psachin'>GitHub</a>.</li>
<li><a href='https://www.reddit.com/user/psachin'>Reddit</a>.</li>
<li><a href='https://youtube.com/user/iclcoolsterU'>YouTube</a>.</li>
<li><a href='/index.xml'>RSS</a>.</li>
<li><a href='/about/'>About</a></li>
</ul>
</div>")

(defvar psachin-website-html-postamble
  "<div class='footer'>
Copyright © 2012-2019 <a href='mailto:iclcoolster@gmail.com'>Sachin Patil</a> | <a href='https://gitlab.com/psachin/psachin.gitlab.io'>Source</a><br>
GnuPG fingerprint: <a href='http://pgp.mit.edu/pks/lookup?op=get&search=0xE5F9CE4862AA06E2'>28C5 A1F3 221B 949D B651 FC47 E5F9 CE48 62AA 06E2</a> <br>
Adapted from <a href='https://nicolas.petton.fr'>https://nicolas.petton.fr</a> <br>
Last updated on %C using %c
</div>")

(defvar site-attachments
  (regexp-opt '("jpg" "jpeg" "gif" "png" "svg"
                "ico" "cur" "css" "js" "woff" "html" "pdf"))
  "File types that are published as static files.")

(defun psachin-org-sitemap-format-entry (entry style project)
  "Format posts with author and published data in the index page.

ENTRY: file-name
STYLE:
PROJECT: `posts in this case."
  (cond ((not (directory-name-p entry))
         (format "*[[file:%s][%s]]*
                  #+HTML: <p class='pubdate'>by %s on %s</p>"
		 entry
		 (org-publish-find-title entry project)
		 (car (org-publish-find-property entry :author project))
		 (format-time-string psachin-date-format
				     (org-publish-find-date entry project))))
        ((eq style 'tree) (file-name-nondirectory (directory-file-name entry)))
        (t entry)))


(defun psachin-org-reveal-publish-to-html (plist filename pub-dir)
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
         :exclude ,(regexp-opt '("README.org" "draft"))
         :auto-sitemap t
         :sitemap-filename "index.org"
	 :sitemap-title "Posts"
	 :sitemap-format-entry psachin-org-sitemap-format-entry
         :sitemap-style list
         :sitemap-sort-files anti-chronologically
	 :html-link-home "/"
	 :html-link-up "/"
	 :html-head-include-scripts t
	 :html-head-include-default-style nil
	 :html-head ,psachin-website-html-head
	 :html-preamble psachin-website-html-preamble
	 :html-postamble ,psachin-website-html-postamble)
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
	 :html-head ,psachin-website-html-head
	 :html-preamble psachin-website-html-preamble
	 :html-postamble ,psachin-website-html-postamble)
	("todo"
         :base-directory "todo"
         :base-extension "org"
	 :exclude ,(regexp-opt '("README.org" "draft"))
	 :index-filename "index.org"
         :recursive nil
         :publishing-function org-html-publish-to-html
         :publishing-directory "./public/todo"
	 :html-link-home "/"
	 :html-link-up "/"
	 :html-head-include-scripts t
	 :html-head-include-default-style nil
	 :html-head ,psachin-website-html-head
	 :html-preamble psachin-website-html-preamble
	 :html-postamble ,psachin-website-html-postamble)
	("slides"
         :base-directory "slides"
         :base-extension "org"
	 :exclude ,(regexp-opt '("README.org" "draft"))
	 :index-filename "index.org"
         :recursive t
         :publishing-function psachin-org-reveal-publish-to-html
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
	("all" :components ("posts" "about" "slides" "todo" "css" "images" "assets" "rss"))))

(provide 'publish)
;;; publish.el ends here
