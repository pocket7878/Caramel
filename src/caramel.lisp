#|
  This file is a part of caramel project.
  Copyright (c) 2013 Masato Sogame (poketo7878@gmail.com)
|#

(in-package :cl-user)

(defpackage caramel
  (:use :cl :css :buildnode)
  (:export
    #:->
    #:do->
    #:content
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

(defun group (list)
  (labels ((%group (acc list)
             (if (null list)
               acc
               (%group 
                 (cons (cons (car list) (cadr list)) acc)
                 (cddr list)))))
    (%group '() list)))

(defun set-attr (node &rest atters)
    (if (zerop (mod (length atters) 2))
      (loop for pair in (group atters)
            for key = (car pair)
            for val = (cdr pair)
            do
            (set-attribute node key val)
            finally (return node))
      (error "malformed atters")))

(defun remove-atter (node &rest atters)
    (loop for att in atters
          do
          (remove-atters node att))
    node)

(defun add-class (node &rest class)
    (loop for cls in class
          do
          (add-css-class node cls))
    node)

(defun remove-class (node &rest class)
    (loop for cls in class
          do
          (remove-css-class node cls))
    node)

(defun list->array (list)
  (apply #'vector list))

(defun content (node &rest value)
  (setf (slot-value node 'rune-dom::children) (rune-dom::make-node-list))
  ;;Insert nodes
  (apply (lambda (x) (insert-children node 0 x)) value))

(defun clone-for (node-list fn)
  (loop for n in node-list
        collect (funcall fn n)))

(defmacro do-> (node &rest trans)
  `(progn
     ,@(loop for tra in trans
             collect
             (typecase tra
               (symbol  `(,tra ,node))
               (atom    `(,tra ,node))
               (list    `(,(car tra) ,node ,@(cdr tra)))))))

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
                        (do-> node ,code)))
         (let ((*html-compatibility-mode* t))
             (document-to-string ,dom))))))

(deftemplate hoge #p"/home/masato/Desktop/test.html" (moge)
    "#hoge" (do-> (content "fuge") (set-attr "color" "pi"))
    "h1" (content "Yahoo!!"))
