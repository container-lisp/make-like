;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: MAKE-LIKE; Base: 10 -*-"

;;; -----------------------------------------------------------------------
;;; make-like
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; ``Software''), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; -----------------------------------------------------------------------

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
   :suffix "Distributed under the terms of MIT License.  See the source code for details."
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
