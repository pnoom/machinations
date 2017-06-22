;;;; machinations.asd

(asdf:defsystem #:machinations
  :description "Factorio-related tools."
  :author "Andy Page"
  :license "MIT"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
	       (:file "recipes")
               (:file "machinations")))

