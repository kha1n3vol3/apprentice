;; apprentice.asd

(asdf:defsystem #:apprentice
  :description "Library for fusing frontier and local coding agents."
  :author "Sai Karnati"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json #:bordeaux-threads)
  :components ((:file "package")
	       (:file "state")
	       (:file "helpers")
	       (:file "file")
	       (:file "chunk")
	       (:file "embedding")
	       (:file "anchor")
	       (:file "tool")
	       (:file "model")
	       (:file "loop")
	       (:file "apprentice")))
