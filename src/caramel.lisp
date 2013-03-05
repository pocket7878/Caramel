#|
  This file is a part of caramel project.
  Copyright (c) 2013 Masato Sogame (poketo7878@gmail.com)
|#

(in-package :cl-user)

(defpackage caramel
  (:use :cl :alexandria :css :buildnode :iterate)
  (:export
    #:->
    #:do->
    #:clone-for
    #:content
    #:html-content
    #:set-attr
    #:remove-attr
    #:add-class
    #:remove-class
    #:html-resource
    #:select
    #:deftemplate))
(in-package :caramel)

(defun html-resource (input)
  (chtml:parse input (cxml-dom:make-dom-builder)))

(defun select (selector node)
  (query selector node))

(defmacro -> (exp &rest rest)
  (if rest
    (let ((fst (car rest))
          (rest (cdr rest)))
      (typecase fst
        (symbol  `(-> (,fst ,exp) ,@rest))
        (atom  `(-> (,fst ,exp) ,@rest))
        (list `(-> (,(car fst) ,exp ,@(cdr fst)) ,@rest))))
    exp))

(defmacro with-clone-node (var node &body body)
  `(let ((,var (dom:clone-node ,node t)))
     ,@body))

(defun treat-node-list (doc node-list)
  (loop for node in node-list
        if (stringp node)
        collect (dom:create-text-node doc node)
        else
        collect node))
        
(defun group (list)
  (labels ((%group (acc list)
             (if (null list)
               acc
               (%group 
                 (cons (cons (car list) (cadr list)) acc)
                 (cddr list)))))
    (%group '() list)))

(defun set-attr (node &rest atters)
  (with-clone-node node node 
    (if (zerop (mod (length atters) 2))
      (loop for pair in (group atters)
            for key = (car pair)
            for val = (cdr pair)
            do
            (set-attribute node key val)
            finally (return node))
      (error "malformed atters"))))

(defun remove-atter (node &rest atters)
  (with-clone-node node node
   (loop for att in atters
         do
         (remove-atter node att))
     node))

(defun add-class (node &rest class)
  (with-clone-node node node
    (loop for cls in class
          do
          (add-css-class node cls))
    node))

(defun remove-class (node &rest class)
  (with-clone-node node node
    (loop for cls in class
          do
          (remove-css-class node cls))
    node))

(defun list->array (list)
  (apply #'vector list))

(defun content (node &rest value)
  (with-clone-node node node
   (setf (slot-value node 'cxml-dom::children) (cxml-dom::make-node-list))
   ;;Insert nodes
   (apply (lambda (x) (insert-children node 0 x)) value)))

(defun html-content (node html-str)
  (with-clone-node node node
   (setf (slot-value node 'cxml-dom::children) (cxml-dom::make-node-list))
   (let ((*document* (buildnode::document-of node)))
     (loop for nnode across (dom:child-nodes (inner-html html-str))
           do
           (add-children node nnode)))
   node))

(defun wrap (node &optional (tag "div"))
  (with-clone-node node node
    (let* ((*document* (buildnode::document-of node))
           (nnode (dom:create-element *document* tag)))
      (add-children nnode node)
      nnode)))

(defun unwrap (node)
  (dom:child-nodes node))

(defmacro do-> (node &rest trans)
  `(progn
     ,@(loop for tra in trans
             collect
             (typecase tra
               (symbol  `(,tra ,node))
               (atom    `(,tra ,node))
               (list    `(,(car tra) ,node ,@(cdr tra)))))))

(defmacro apply-select-trans (node select trans)
  (let ((n (gensym)))
    `(progn
       (loop for ,n in (select ,select ,node)
             collect
             (do-> ,n ,trans))
       ,node)))

(defun before (node-or-nodes &rest nodes)
  (flatten (concatenate 'list nodes 
                        (ensure-list node-or-nodes))))

(defun after (node-or-nodes &rest nodes)
  (flatten (concatenate 'list 
                        (ensure-list node-or-nodes)
                        nodes)))

(defun subsititute (node &rest nodes)
  nodes)

(defmacro clone-for (node var lst &rest trans)
  (cond 
    ((= 1 (length trans)) 
     `(iter (for ,var in ,lst)
            (collect (-> ,node (dom:clone-node t) ,@trans))))
    (t
     `(loop for ,var in ,lst
            for nn = (dom:clone-node ,node t)
            do
            (progn
              ,@(loop for stp in (group trans)
                      for select = (car stp)
                      for tr = (cdr stp)
                      collect
                      `(apply-select-trans nn ,select ,tr)))
            collect nn))))
                      
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
         
(defun dom-to-html-string (dom)
  (let ((*html-compatibility-mode* t))
    (document-to-string dom)))

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
                        (replace-node-with node (do-> node ,code))))
         (dom-to-html-string ,dom)))))
