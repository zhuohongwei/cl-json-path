(defpackage #:cl-json-path
  (:use 
    #:alexandria
    #:cl
    #:cl-ppcre)
  (:export 
    #:json-value-with-path-list
    #:make-path-list
    #:json-value-with-path))
(in-package #:cl-json-path)

(defun property-p (property)
  (consp property))

(defun property-name (property)
  (car property))

(defun property-value (property)
  (cdr property))

(defun json-object-p (object)
  (and (listp object) (every #'property-p object)))

(defun json-array-p (arr)
  (and (listp arr) (every #'json-object-p arr)))

(defun index-component-p (path-component)
  (alexandria:non-negative-integer-p path-component))

(defun name-component-p (path-component)
  (keywordp path-component))

(defun path-component-p (path-component &key (name-component-p-fn #'name-component-p))
  (or (index-component-p path-component) (funcall name-component-p-fn path-component)))

(defun path-list-p (path-list &key (name-component-p-fn #'name-component-p))
  (and (listp path-list) 
       (every #'(lambda (c) (path-component-p c :name-component-p-fn name-component-p-fn)) path-list)))

(defun find-property-value (property-name json-object)
  (property-value (find-if #'(lambda (p) (eq property-name (property-name p))) json-object)))

(defun json-value-with-path-list (path-list json &key
                                    (json-array-p-fn #'json-array-p)
                                    (name-component-p-fn #'name-component-p)
                                    (json-object-p-fn #'json-object-p)
                                    (find-property-value-fn #'find-property-value))
  (cond 
    ((not (path-list-p path-list :name-component-p-fn name-component-p-fn)) nil)
    ((null path-list) json)
    ((index-component-p (car path-list))
      (if (funcall json-array-p-fn json)
        (json-value-with-path-list (cdr path-list) (nth (car path-list) json))))
    ((funcall name-component-p-fn (car path-list))
      (if (funcall json-object-p-fn json)
        (json-value-with-path-list 
          (cdr path-list)
          (funcall find-property-value-fn (car path-list) json))))
    (t nil)))

(defun make-path-list (path)
  (if (stringp path)
    (map 'list 
      #'(lambda (s) 
          (let ((i (parse-integer s :junk-allowed t))) 
              (if i i (intern (string-upcase s) :keyword))))  
      (split "/" (string-trim '(#\/) path)))))

(defmacro json-value-with-path (path json)
  `(json-value-with-path-list (make-path-list ,path) ,json))