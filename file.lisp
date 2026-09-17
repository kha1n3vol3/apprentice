;;;; file.lisp

(in-package :apprentice)


(defstruct file
  path
  content)


;;;; File Handling


(defun dot-name-p (name)
  "Determines if NAME begins with a dot."
  (and (stringp name) (plusp (length name)) (char= (char name 0) #\.)))

(defun hidden-file-p (path)
  "Determines if PATH is hidden."
  (dot-name-p (file-namestring path)))

(defun read-file-string-safe (path)
  (handler-case
      (uiop:read-file-string path :external-format :utf-8)
    (error nil)))

(defun create-file (path)
  "Creates file struct from PATH."
  (let ((content (read-file-string-safe path)))
    (when content
      (make-file
       :path path
       :content content))))

(defparameter *ignored-dirs*
  '("target" "build" "dist" "out" "obj" "bin"
    "node_modules" "vendor" "deps" "_build" "elm-stuff"
    "__pycache__" "venv" "env" "site-packages"
    "coverage" "logs" "tmp" "temp"))

(defparameter *ignored-extensions*
  '("fasl" "o" "a" "so" "dylib" "dll" "exe" "class" "pyc" "pyo" "rlib"
    "png" "jpg" "jpeg" "gif" "bmp" "ico" "pdf" "eps"
    "zip" "gz" "tar" "tgz" "bz2" "xz" "jar" "rlib"
    "mp3" "mp4" "mov" "wav" "ttf" "otf" "woff" "woff2"))

(defparameter *max-walk-depth* 16)

(defun ignored-dir-p (dir)
  (let ((name (car (last (pathname-directory dir)))))
    (or (dot-name-p name)
	(and (stringp name)
	     (member name *ignored-dirs* :test #'string-equal)))))

(defun ignored-file-p (path)
  (let ((type (pathname-type path)))
    (or (hidden-file-p path)
	(and type (member type *ignored-extensions* :test #'string-equal)))))

(defun collect-files (dir &optional (depth 0))
  "Collect valid files we want to consider in DIR."
  (when (< depth *max-walk-depth*)
    (let ((files (remove-if #'ignored-file-p
			    (ignore-errors (uiop:directory-files dir)))))
      (dolist (sub (ignore-errors (uiop:subdirectories dir)) files)
	(unless (ignored-dir-p sub)
	  (setf files (append files (collect-files sub (1+ depth)))))))))

(defun create-files-from-dir (dir)
  (remove nil (mapcar #'create-file
		      (mapcar #'uiop:native-namestring
			      (collect-files dir)))))
