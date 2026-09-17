;;;; subagent.lisp

(in-package :apprentice)


(defparameter *subagent-loop* :little-coder)

(defparameter *subagent-max-turns* 8)

(defparameter *subagent-calls* 0)

(defvar *subagent-count-lock* (bt:make-lock "apprentice-subagent-count"))

(defun note-subagent-call ()
  (bt:with-lock-held (*subagent-count-lock*) (incf *subagent-calls*)))


(defparameter *subagent-report-limit* 6000
  "Characters of a subagent's report the SUBAGENT tool passes back.")

(defparameter *subagent-brief-limit* 1200
  "The same, for SUBAGENT-BRIEF. Small on purpose: a loop that delegates
   constantly keeps every report it gets back in context for the rest of
   the run.")

(defparameter *subagent-tool-names* '("subagent" "subagent-brief"))

(defparameter *max-parallel-subagents* 3)

(defun run-subagent-1 (task limit turns)
  "Runs one subagent with TASK, limited to TURNS, response truncated to LIMIT."
  (let ((tools (remove-if (lambda (tl)
			    (member (tool-name tl) *subagent-tool-names*
				    :test #'string=))
			  *subagent-tools*)))
    (note-subagent-call)
    (bt:with-lock-held (*trace-lock*)
      (format t "~&⇢ subagent (~a, ~(~a~) loop, ~a turns): ~a~%"
	      (model-name *subagent-model*) *subagent-loop*
	      (or turns *subagent-max-turns*) task)
      (finish-output))
    (destructuring-bind (content msgs)
	(funcall (resolve-loop *subagent-loop*) task
		 :model *subagent-model*
		 :system-prompt *subagent-prompt*
		 :system *subagent-prompt*
		 :tools tools
		 :max-turns (or turns *subagent-max-turns*))
      (declare (ignore msgs))
      (truncate-output (if (and (stringp content) (string/= content ""))
			   content
			   "(the subagent returned no report)")
		       limit))))

(defun run-subagents (tasks limit turns)
  "Run multiple TASKS on subagents, *MAX-PARALLEL-SUBAGENTS* at a time."
  (let* ((tasks (if (stringp tasks)
		    (list tasks)
		    (remove-if-not #'stringp tasks)))
	 (n (length tasks)))
    (cond
      ((zerop n) "No tasks were given. Send tasks as a list of strings.")
      ((= n 1) (run-subagent-1 (first tasks) limit turns))
      (t
       (let ((vec     (coerce tasks 'vector))
	     (results (make-array n :initial-element nil))
	     (out     *standard-output*)
	     (dirs    *allowed-dirs*)
	     (anchor  *anchor-dir*)
	     (model   *subagent-model*)
	     (kit     *subagent-tools*)
	     (lp      *subagent-loop*)
	     (prompt  *subagent-prompt*)
	     (turns*  *subagent-max-turns*))
	 (loop for start from 0 below n by *max-parallel-subagents*
	       do (mapc
		   #'bt:join-thread
		   (loop for i from start
			   below (min n (+ start *max-parallel-subagents*))
			 collect
			 (let ((i i))
			   (bt:make-thread
			    (lambda ()
			      (let ((*standard-output*    out)
				    (*allowed-dirs*       dirs)
				    (*anchor-dir*         anchor)
				    (*subagent-model*     model)
				    (*subagent-tools*     kit)
				    (*subagent-loop*      lp)
				    (*subagent-prompt*    prompt)
				    (*subagent-max-turns* turns*))
				(setf (aref results i)
				      (handler-case
					  (run-subagent-1 (aref vec i) limit turns)
					(error (e)
					  (format nil "Subagent failed: ~a" e))))))
			    :name "apprentice-subagent")))))
	 (format nil "~{~a~^~%~%~}"
		 (loop for i below n
		       collect (format nil "--- task ~a of ~a ---~%~a"
				       (1+ i) n (aref results i)))))))))
