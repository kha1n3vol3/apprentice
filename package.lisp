;;;; package.lisp


(defpackage #:apprentice
  (:use #:cl)
  (:export ;; Conversation
   #:chat
   #:clear
   #:drop-turns
   #:show-turns
   ;; Models
   #:available-models
   #:model
   #:set-model
   ;; Anchors
   #:anchors
   #:available-anchors
   #:add-anchor
   #:clear-anchors
   #:set-anchor-dir
   ;; Permissions
   #:allowed-dirs
   #:add-allowed-dir
   #:clear-allowed-dirs
   ;; Loops
   #:available-loops
   #:current-loop
   #:set-loop
   #:resolve-loop
   ;; Options
   #:options
   #:add-option
   #:clear-options))
