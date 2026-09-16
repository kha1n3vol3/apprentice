;;;; loop.lisp

(in-package :apprentice)


(defun dispatch-tool (name args tools)
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
  "DISPATCH-TOOL's string, never empty. Finding nothing is a real answer,
   but providers reject an empty text block, so it has to be said out
   loud rather than sent as \"\"."
  (let ((result (dispatch-tool name args tools)))
    (if (or (null result)
	    (and (stringp result) (string= result "")))
	"(no output)"
	result)))

(defvar *trace-lock* (bt:make-lock "apprentice-trace")
  "Held while one call's trace is printed, so parallel calls do not
   interleave mid-line.")

(defparameter *max-parallel-calls* 1
  "Tool calls from one turn allowed to run at once. One by default:
   concurrent edit or bash calls touching the same file corrupt it, so
   only a loop that knows its tools are safe should raise this.")

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
   when they run at once. One turn's results travel together: some
   providers require them batched into a single message, and some need
   the function name alongside the id."
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

(defun current-tools (tools)
  "TOOLS, or what calling it returns when a loop was handed a function,
   so a tool set can change as a run goes on."
  (if (functionp tools) (funcall tools) tools))

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


(defparameter *standard-prompt*
  "You are a coding agent. Use tools to inspect files before answering. Always use absolute paths.")

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
      (let* ((kit  (current-tools tools))
	     (turn (apply #'run-model model msgs kit opts)))
	(when (eq (turn-stop turn) :error)
	  (return (list (turn-text turn) msgs)))
	(setf msgs (append msgs (list turn)))
	(if (turn-calls turn)
	    (setf msgs (append msgs (list (make-turn
					   :role :tool-results
					   :results (run-calls (turn-calls turn) kit)))))
	    (return (list (turn-text turn) msgs))))
	  finally (return (apply #'force-final-answer
				 model msgs max-turns opts)))))


;;;; Little Coder Agent Loop
;;;;
;;;; NOTE: Implementation based on https://github.com/itayinbarr/little-coder


(defparameter *little-coder-prompt*
  *standard-prompt*)

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


(defparameter *apprentice-prompt*
  "You are the lead agent on a coding task. You cannot read or modify files yourself: you have no read, write, edit or shell tools. You have file-tree to see the structure of the directory, grep to find where text and identifiers appear, dense-vector-search to find passages by meaning when you do not know the exact wording, web-search for information outside the codebase, and subagent-brief to delegate work to a subagent that can read, write and edit files and run shell commands.

Start with file-tree to get oriented, then locate things with grep and dense-vector-search, since they are fast and hand you the text itself. A subagent's reply is cut off after roughly a thousand characters, so it is the wrong way to read a file: never ask one to send you a file's contents or a long passage, because the end will simply be missing. Use grep for that, with a pattern narrow enough to show the lines you need. Delegate when something must be traced, judged or changed rather than merely quoted, and delegate every change to a file.

A subagent starts with no memory of this conversation, and nothing carries over between subagent calls. Make every task self-contained: absolute file paths, exactly what to find or change, and any context it needs. Never refer back to a file or function from an earlier call; name it again in full.

Tell every subagent to answer briefly, findings and evidence only with no narration, since anything past the limit is lost. When you delegate a change, ask for the file path, the line numbers, and only the lines that changed, quoted. When you delegate an investigation, ask for the specific answer with file paths and line numbers, quoting only the lines that carry it. If a reply comes back marked as truncated, do not ask for it again in full; ask a narrower question instead.

Give each subagent one focused task, and split larger work into several. Send independent tasks together in one call so they run at the same time, but never give two of them the same file to edit.

Pass turns with every call and size it to the task: three or four for a lookup or a question about one file, eight to twelve for a change that spans several. A subagent that runs out of turns still reports what it has, so a tight budget costs you a partial answer rather than nothing, and it brings the work back to you sooner. Prefer several small tasks over one long one.

When the work is done, answer the user with a summary of what changed, citing the evidence the subagents reported.")

(defparameter *escalate-after* 10
  "Subagent tasks after which the primary is handed the standard tool
   kit. A run that is not converging through delegation can then work
   directly instead of grinding on.")

(defun apprentice-loop (prompt &rest options
			&key (system-prompt *apprentice-prompt*)
			  (tools *apprentice-tools*)
			  (max-parallel-calls 3)
			  (escalate-after *escalate-after*)
			&allow-other-keys)
  (let ((*max-parallel-calls* max-parallel-calls)
	(announced nil))
    (setf *subagent-calls* 0)
    (apply #'standard-loop prompt
	   :system-prompt system-prompt
	   :tools (lambda ()
		    (if (< *subagent-calls* escalate-after)
			tools
			(progn
			  (unless announced
			    (setf announced t)
			    (format t "~&⇧ ~a subagent tasks run: the standard tool kit is now available~%"
				    *subagent-calls*))
			  *standard-tools*)))
	   options)))
