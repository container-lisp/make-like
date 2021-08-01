
(in-package #:make-like)

(defvar *template*
  (with-open-file (stream "template.tar.gz" :element-type '(unsigned-byte 8))
    (let ((seq (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence seq stream)
      seq)))

(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help"))

(defun usage ()
  (opts:describe
   :prefix "make-like - copyright (C) 2021 Anthony Green <green@moxielogic.com>"
   :suffix "Distributed under the terms of MIT License"
   :usage-of "make-like"
   :args     "likefile"))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
 (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun fatal-error (&rest format-string-and-parameters)
  (format t "Error: ~A~%"
          (apply #'format (cons nil format-string-and-parameters)))
  (finish-output)
  (sb-ext:exit :abort t))

(defun make-application (&key (app-name "fixme")
                           (author "Fix Me <fixme@example.com>")
                           (description "FIXME")
                           (source-header ";;; FIXME source-header")
                           (github-account "fixme-github-account")
                           (container-registry "quay.io/fixme"))
  (when (fad:directory-exists-p app-name)
    (fatal-error "Directory '~A' already exists" app-name))
  (let ((app-dir (pathname (str:concat app-name "/"))))
    (fad:delete-directory-and-files "_template" :if-does-not-exist :IGNORE)
    (archive::extract-files-from-archive
     (archive:open-archive 'archive:tar-archive
			     (chipz:make-decompressing-stream 'chipz:gzip
							      (flexi-streams:make-in-memory-input-stream *template*))
			     :direction :input))
    (rename-file "_template" app-name)
    (uiop:with-current-directory (app-dir)
      (cl-fad:walk-directory
       #p"." (lambda (filepath)
               (let* ((template-filename (namestring filepath))
                      (filename (subseq template-filename 0 (- (length template-filename) 4)))
                      (template (alexandria:read-file-into-string filepath)))
	         (with-open-file (stream filename
				         :direction :output
				         :if-exists :supersede
				         :if-does-not-exist :create)
	           (princ (funcall (cl-template:compile-template template)
                                   (list :app-name app-name
                                         :author author
                                         :description description
                                         :source-header source-header
                                         :github-account github-account
                                         :container-registry container-registry))
                          stream)))
               (delete-file filepath))
       :follow-symlinks nil
       :test (lambda (filename)
	       (str:ends-with? ".clt" (file-namestring filename))))
      (rename-file "src/app.asd" (str:concat app-name ".asd"))
      (rename-file "src/app.lisp" (str:concat app-name ".lisp")))))

(defun main (args)
  (multiple-value-bind (options free-args)
		       (handler-case
			   (handler-bind ((opts:unknown-option #'unknown-option))
			     (opts:get-opts))
			 (opts:missing-arg (condition)
			   (format t "fatal: option ~s needs an argument!~%"
				   (opts:option condition)))
			 (opts:arg-parser-failed (condition)
			   (format t "fatal: cannot parse ~s as argument of ~s~%"
				   (opts:raw-arg condition)
				   (opts:option condition))))
    (when-option (options :help)
		 (usage))
    (if (not (eq 1 (length free-args)))
	(usage)
	(load (car free-args)))))
