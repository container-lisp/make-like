;;;; make-like.asd

(asdf:defsystem #:make-like
  :description "A web application generator"
  :author "Anthony Green <green@moxielogic.com>"
  :license "Apache"
  :depends-on (#:cl-template
               #:log4cl
               #:cl-fad
	       #:unix-opts
               #:str
	       #:alexandria
	       #:chipz
	       #:archive
	       #:flexi-streams
	       #:inferior-shell
	       #:uiop)
  :serial t
  :components ((:file "package")
               (:file "make-like")))
