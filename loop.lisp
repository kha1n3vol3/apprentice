;;;; loop.lisp

(in-package :apprentice)


(defun dispatch-tool (name args tools)
  "Runs tool with NAME with ARGS."
  (let ((tl (find name tools :key #'tool-name :test #'string=)))
    (cond
      ((null tl) (format nil "Unknown tool: ~a" name))
      ((eq args :malformed)
       (format nil "Arguments to ~a were not valid JSON. Send them again ~
                    as a JSON object." name))
      (t
       (handler-case
	   (let ((failures (remove nil (run-tool-checks tl args))))
	     (if failures
		 (format nil "~{~a~^~%~}" failures)
		 (run-tool tl args)))
	 (error (e) (format nil "Tool ~a failed: ~a" name e)))))))

(defun tool-output (name args tools)
  "String wrapping around DISPATCH-TOOL"
  (let ((result (dispatch-tool name args tools)))
    (if (or (null result)
	    (and (stringp result) (string= result "")))
	"(no output)"
	result)))

(defvar *trace-lock* (bt:make-lock "apprentice-trace")
  "Held while one call's trace is printed, so parallel calls do not
   interleave mid-line.")

(defparameter *max-parallel-calls* 1)

(defun call-triple (call tools out)
  "One call run and traced, as its (ID NAME OUTPUT) triple."
  (let* ((name   (tool-call-name call))
	 (args   (tool-call-args call))
	 (result (handler-case (tool-output name args tools)
		   (error (e) (format nil "Tool ~a failed: ~a" name e)))))
    (bt:with-lock-held (*trace-lock*)
      (format out "~&→ ~a ~s~%~a~%" name args result)
      (finish-output out))
    (list (tool-call-id call) name result)))

(defun run-calls (calls tools)
  "List of (ID NAME OUTPUT) per call, in the order CALLS were given even
   when they run at once."
  (if (or (<= *max-parallel-calls* 1) (null (rest calls)))
      (mapcar (lambda (c) (call-triple c tools *standard-output*)) calls)
      (let* ((vec     (coerce calls 'vector))
	     (n       (length vec))
	     (results (make-array n :initial-element nil))
	     (out     *standard-output*)
	     (dirs    *allowed-dirs*)
	     (anchor  *anchor-dir*)
	     (anchors *anchors*)
	     (model   *subagent-model*)
	     (kit     *subagent-tools*))
	(loop for start from 0 below n by *max-parallel-calls*
	      do (mapc
		  #'bt:join-thread
		  (loop for i from start
			  below (min n (+ start *max-parallel-calls*))
			collect
			(let ((i i))
			  (bt:make-thread
			   (lambda ()
			     (let ((*allowed-dirs*   dirs)
				   (*anchor-dir*     anchor)
				   (*anchors*        anchors)
				   (*subagent-model* model)
				   (*subagent-tools* kit))
			       (setf (aref results i)
				     (call-triple (aref vec i) tools out))))
			   :name "apprentice-tool-call")))))
	(coerce results 'list))))

(defparameter *loop-keys*
  '(:model :system-prompt :system :tools :max-turns :history
    :max-parallel-calls :escalate-after)
  "Keys the loops consume themselves. CHECK-OPTIONS rejects any option
   the model does not declare, so these must not reach it.")

(defun model-options (options &rest also)
  "OPTIONS with the loops' own keys, and any in ALSO, removed."
  (loop for (k v) on options by #'cddr
	unless (or (member k *loop-keys*) (member k also))
	  append (list k v)))

(defun seed-messages (system prompt history)
  (if history
      (append history (list (make-turn :role :user :text prompt)))
      (list (make-turn :role :system :text system)
	    (make-turn :role :user   :text prompt))))

(defun force-final-answer (model msgs max-turns &rest options)
  "One last call with the tools taken away, so the model has to answer in
   prose rather than reach for another one. Returns the (CONTENT MSGS)
   pair a loop returns."
  (let* ((nudge (make-turn
		 :role :user
		 :text (format nil "You have used all ~a of your turns and cannot call any more tools. Answer now with what you already have: report what you found or changed so far." max-turns)))
	 (msgs  (append msgs (list nudge)))
	 (turn  (apply #'run-model model msgs nil options)))
    (list (or (turn-text turn)
	      (format nil "[stopped: hit max-turns (~a)]" max-turns))
	  (append msgs (list turn)))))


;;;; Standard Agent Loop


(defun standard-loop (prompt &rest options
		      &key (model *model*)
			(system-prompt *standard-prompt*)
			(tools *standard-tools*)
			(max-turns 50)
			(history nil)
		      &allow-other-keys)
  (let ((opts (model-options options))
	(msgs (seed-messages system-prompt prompt history)))
    (loop repeat max-turns do
      (let ((turn (apply #'run-model model msgs tools opts)))
	(when (eq (turn-stop turn) :error)
	  (return (list (turn-text turn) msgs)))
	(setf msgs (append msgs (list turn)))
	(if (turn-calls turn)
	    (setf msgs (append msgs (list (make-turn
					   :role :tool-results
					   :results (run-calls (turn-calls turn) tools)))))
	    (return (list (turn-text turn) msgs))))
	  finally (return (apply #'force-final-answer
				 model msgs max-turns opts)))))


;;;; Little Coder Agent Loop
;;;;
;;;; NOTE: Implementation based on https://github.com/itayinbarr/little-coder



(defun little-coder-loop (prompt &rest options
			  &key (model *model*)
			    (system *little-coder-prompt*)
			    (tools *little-coder-tools*)
			    (max-turns 20)
			    (history nil)
			  &allow-other-keys)
  (let ((opts (model-options options :thinking))
	(msgs (seed-messages system prompt history)))
    (loop repeat max-turns do
      (let ((turn (apply #'run-model model msgs tools :thinking t opts)))
	(when (eq (turn-stop turn) :error)
	  (return (list (turn-text turn) msgs)))
	(setf msgs (append msgs (list turn)))
	(cond
	  ;; Retry with thinking off, keeping the partial trace in
	  ;; context: the model keeps its work but must now commit.
	  ((eq (turn-stop turn) :overflow)
	   (format t "~&⋯ deliberation overflowed, retrying with thinking disabled~%")
	   (let ((retry (apply #'run-model model msgs tools :thinking nil opts)))
	     (when (eq (turn-stop retry) :error)
	       (return (list (turn-text retry) msgs)))
	     (setf msgs (append msgs (list retry)))
	     (if (turn-calls retry)
		 (setf msgs (append msgs (list (make-turn
						:role :tool-results
						:results (run-calls (turn-calls retry) tools)))))
		 (return (list (turn-text retry) msgs)))))
	  ((turn-calls turn)
	   (setf msgs (append msgs (list (make-turn
					  :role :tool-results
					  :results (run-calls (turn-calls turn) tools))))))
	  (t (return (list (turn-text turn) msgs)))))
	  finally (return (apply #'force-final-answer
				 model msgs max-turns :thinking nil opts)))))


;;;; Apprentice Agent Loop
;;;;
;;;; The primary model cannot read or write files. It locates things with
;;;; search tools and delegates investigation and every change to
;;;; subagents, relying on their reports.


(defparameter *escalate-after* 10)

(defun apprentice-loop (prompt &rest options
			&key (model *model*)
			  (system-prompt *apprentice-prompt*)
			  (tools *apprentice-tools*)
			  (max-turns 50)
			  (history nil)
			  (max-parallel-calls 3)
			  (escalate-after *escalate-after*)
			&allow-other-keys)
  "The standard loop with one addition: once ESCALATE-AFTER subagent
   tasks have run, the primary is handed the standard tool kit."
  (let ((*max-parallel-calls* max-parallel-calls)
	(opts      (model-options options))
	(msgs      (seed-messages system-prompt prompt history))
	(escalated nil))
    (setf *subagent-calls* 0)
    (loop repeat max-turns do
      (let* ((escalate (>= *subagent-calls* escalate-after))
	     (kit      (if escalate *standard-tools* tools)))
	(when (and escalate (not escalated))
	  (setf escalated t)
	  (format t "~&⇧ ~a subagent tasks run: the standard tool kit is now available~%"
		  *subagent-calls*))
	(let ((turn (apply #'run-model model msgs kit opts)))
	  (when (eq (turn-stop turn) :error)
	    (return (list (turn-text turn) msgs)))
	  (setf msgs (append msgs (list turn)))
	  (if (turn-calls turn)
	      (setf msgs (append msgs (list (make-turn
					     :role :tool-results
					     :results (run-calls (turn-calls turn) kit)))))
	      (return (list (turn-text turn) msgs)))))
	  finally (return (apply #'force-final-answer
				 model msgs max-turns opts)))))
