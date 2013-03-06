#|
  This file is a part of caramel project.
  Copyright (c) 2013 Masato Sogame (poketo7878@gmail.com)
|#

(in-package :cl-user)

(defpackage caramel
  (:use :cl :alexandria :css :buildnode :iterate)
  (:shadow :substitute :append :prepend)
  (:export
    ;; resource
    #:html-resource
    ;; selector
    #:select
    ;; translator
    #:content
    #:html-content
    #:before
    #:after
    #:append
    #:prepend
    #:substitute
    #:set-attr
    #:remove-attr
    #:add-class
    #:remove-class
    #:move
    #:clone-for
    #:do->
    ;; templating
    #:defsnippet
    #:deftemplate
    ;; scraping
    #:get-attrs
    #:get-attr
    #:get-content
    ))
(in-package :caramel)

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility
;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-clone-node (var node &body body)
  `(let ((,var (dom:clone-node ,node t)))
     ,@body))

(defun treat-node-list (doc node-list)
  (loop for node in node-list
        if (stringp node)
        collect (dom:create-text-node doc node)
        else
        collect (progn 
                 (setf (slot-value node 'cxml-dom::owner) doc)
                 node)))
        
(defun group (list)
  (labels ((%group (acc list)
             (if (null list)
               acc
               (%group 
                 (cons (cons (car list) (cadr list)) acc)
                 (cddr list)))))
    (%group '() list)))

(defun dom-to-html-string (dom)
  (let ((*html-compatibility-mode* t))
    (document-to-string dom)))

(defmethod replace-node-with ((old-node dom:node) node-or-nodes)
  (cond ((listp node-or-nodes)
         (setf node-or-nodes (treat-node-list (buildnode::document-of old-node) (flatten node-or-nodes)))
         (loop for nnode in node-or-nodes
               do
               (when (eq nnode old-node)
                 (setf nnode (dom:clone-node nnode t)))
               (dom:insert-before (dom:parent-node old-node) 
                                  nnode
                                  old-node))
         (dom:remove-child (dom:parent-node old-node) old-node))
        (t
         (when (eq node-or-nodes old-node)
           (setf node-or-nodes (dom:clone-node nnode t)))
         (dom:replace-child (dom:parent-node old-node) node-or-nodes old-node))))

(defmacro apply-select-trans (node select trans)
  (let ((n (gensym)))
    `(progn
       (loop for ,n in (select ,select ,node)
	    do
	    (replace-node-with ,n (funcall,trans ,n)))
       ,node)))

(defun flatmap (fn node-or-nodes)
  (flatten (mapcar fn (ensure-list node-or-nodes))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Resource
;;;;;;;;;;;;;;;;;;;;;;;;
(defun html-resource (input)
  (chtml:parse input (cxml-dom:make-dom-builder)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Selector
;;;;;;;;;;;;;;;;;;;;;;;;
(defun select (selector node)
  (query selector node))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Translator
;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-attr (&rest atters)
  (lambda (node)
    (with-clone-node node node 
		     (if (zerop (mod (length atters) 2))
			 (loop for pair in (group atters)
			    for key = (car pair)
			    for val = (cdr pair)
			    do
			      (set-attribute node key val)
			    finally (return node))
			 (error "malformed atters")))))


(defun remove-attr (&rest atters)
  (lambda (node)
    (with-clone-node node node
     (loop for att in atters
        do
          (remove-attr node att))
     node)))

(defun add-class (&rest class)
  (lambda (node)
    (with-clone-node node node
		     (loop for cls in class
			do
			  (add-css-class node cls))
		     node)))

(defun remove-class (&rest class)
  (lambda (node)
    (with-clone-node node node
		     (loop for cls in class
			do
			  (remove-css-class node cls))
		     node)))

(defun content (&rest value)
  (lambda (node)
    (with-clone-node node node
		     (setf (slot-value node 'cxml-dom::children) (cxml-dom::make-node-list))
		     ;;Insert nodes
		     (loop for val in (flatten value)
			  do
			  (insert-children node 0 val))
		     node)))

(defun html-content (html-str)
  (lambda (node)
    (with-clone-node node node
		     (setf (slot-value node 'cxml-dom::children) (cxml-dom::make-node-list))
		     (let ((*document* (buildnode::document-of node)))
		       (loop for nnode across (dom:child-nodes (inner-html html-str))
			  do
			    (add-children node nnode)))
		     node)))

(defun wrap (tag)
  (lambda (node)
    (with-clone-node node node
		     (let* ((*document* (buildnode::document-of node))
			    (nnode (dom:create-element *document* tag)))
		       (add-children nnode node)
		       nnode))))

(defun unwrap ()
  (lambda (node)
    (loop for n across (dom:child-nodes node)
	 collect n)))

(defun do-> (&rest fns)
  (lambda (node-or-nodes)
    (reduce (lambda (nodes f) (flatmap f nodes)) fns
	    :initial-value (ensure-list node-or-nodes))))


(defun before (&rest nodes)
  (lambda (node-or-nodes)
    (flatten (concatenate 'list nodes 
			  (ensure-list node-or-nodes)))))

(defun after (&rest nodes)
  (lambda (node-or-nodes)
    (flatten (concatenate 'list 
			  (ensure-list node-or-nodes)
			  nodes))))

(defun substitute (&rest nodes)
  (lambda (node)
    nodes))

(defun prepend (&rest nodes)
  (lambda (node)
    (with-clone-node node node
      (loop for nnode in (treat-node-list
                           (buildnode::document-of node) nodes)
            do
            (insert-children node 0 nnode))
      node)))

(defun append (&rest nodes)
  (lambda (node)
    (with-clone-node node node
        (loop for nnode in (treat-node-list 
                             (buildnode::document-of node) nodes)
              do
              (add-children node nnode))
        node)))

(defun move (src-selector dst-selector)
  (lambda (node)
    (with-clone-node node node
        (let ((snodes (select src-selector node))
              (dnodes (select dst-selector node)))
          (loop for dnode in dnodes
                do
                (replace-node-with dnode 
                   (flatmap (lambda (x) (dom:clone-node x t)) snodes)))
          (loop for snode in snodes
                do
                (dom:remove-child (dom:parent-node snode) snode)))
        node)))

(defmacro clone-for (var lst &rest trans)
  (let ((node (gensym)))
    (cond 
      ((= 1 (length trans)) 
       `(lambda (,node)
	  (iter (for ,var in ,lst)
		(collect (funcall ,@trans (dom:clone-node ,node t))))))
      (t
       (let ((nn (gensym)))
	 `(lambda (,node)
	    (loop for ,var in ,lst
	       for ,nn = (dom:clone-node ,node t)
	       do
		 (progn
		   ,@(loop for stp in (group trans)
			for select = (car stp)
			for tr = (cdr stp)
			collect
			  `(apply-select-trans ,nn ,select ,tr)))
	       collect ,nn)))))))
                      
         
;;;;;;;;;;;;;;;;;;;;;;;;
;;; Templating
;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro deftemplate (name file-path args &rest select-body-pair)
  (let ((dom (gensym))
        (st (gensym)))
    `(defun ,name ,args
       (let ((,dom (html-resource ,file-path)))
         ,@(loop for sbp in (group select-body-pair)
                 for selector = (car sbp)
                 for code = (cdr sbp)
                 collect
                 `(loop for node in (select ,selector ,dom)
                        do
		       (replace-node-with node (funcall  ,code node))))
         (dom-to-html-string ,dom)))))

(defmacro defsnippet (name file-path selector args &rest select-trans-pair)
  (let ((dom (gensym))
        (st (gensym))
        (n (gensym))
        (ns (gensym)))
    `(defun ,name ,args
       (let* ((,dom (html-resource ,file-path))
              (,st (select ,selector ,dom)))
         (loop for ,n in ,st
                do
                ,@(loop for sbp in (group select-trans-pair)
                        for selector = (car sbp)
                        for code = (cdr sbp)
                        collect
                        `(loop for ,ns in (select ,selector ,n)
                               do
                               (replace-node-with ,ns (funcall ,code ,ns))))
                collect ,n)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scraping
;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-attrs (node)
  (loop for attr-node in (slot-value (dom:attributes node)
                                 'cxml-dom::items)
        collect
        (cons
          (dom:node-name attr-node)
          (dom:value attr-node))))

(defun get-attr (node name)
  (get-attribute node name))

(defun get-content (node)
  (etypecase node
   (rune-dom::text 
     (dom:node-value node))
   (rune-dom::element
     (funcall (unwrap) node))
   (rune-dom::document
     (funcall (unwrap) node))))
     
