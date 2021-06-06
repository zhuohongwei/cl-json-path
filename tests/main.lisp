(defpackage cl-json-path/tests/main
  (:use #:cl
        #:cl-json-path
        #:rove)
  (:import-from 
        #:cl-json-path
        #:property-p
        #:property-name
        #:property-value
        #:json-object-p
        #:json-array-p
        #:path-component-p
        #:path-list-p
        #:json-value-with-path-list
        #:make-path-list
        #:json-value-with-path))
(in-package #:cl-json-path/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-json-path)' in your Lisp.

(defvar foobar (cons :foo "bar"))
(defvar barbaz (cons :bar "baz"))
(defvar a-json-object (list foobar barbaz))
(defvar a-nested-json-object (list (cons :qux a-json-object)))

(deftest test-property-p
  (testing "when a property is passed"
    (ok (property-p foobar)))
  (testing "when a non-property is passed"
    (ng (property-p "baz"))))

(deftest test-property-name
  (testing "extracting property name"
    (ok (eq (property-name foobar) :foo))))

(deftest test-property-value
  (testing "extracting property value"
    (ok (string-equal (property-value foobar) "bar"))))

(deftest test-json-object-p
  (testing "when a json object is passed"
    (ok (json-object-p a-json-object)))
  (testing "when a non-json object is passed"
    (ng (json-object-p (list foobar "baz")))))

(deftest test-json-array-p
  (testing "when a json array is passed"
    (ok (json-array-p (list a-json-object a-json-object))))
  (testing "when a non-json array is passed"
    (ng (json-array-p (list a-json-object "baz")))))

(deftest test-path-component-p
  (testing "when invalid path component"
    (ng (path-component-p 'invalid-component))
    (ng (path-component-p '()))
    (ng (path-component-p -1))
    (ng (path-component-p "bar")))
  (testing "when valid path component"
    (ok (path-component-p :foo))
    (ok (path-component-p 0))
    (ok (path-component-p 1))))

(deftest test-path-list-p
  (testing "when invalid path"
    (ng (path-list-p 'invalid-path))
    (ng (path-list-p (list :foo nil)))
    (ng (path-list-p (list :foo "bar"))))
  (testing "when valid path"
    (ok (path-list-p '()))
    (ok (path-list-p (list :foo 1)))
    (ok (path-list-p (list 1 :foo)))))

(deftest test-json-value-with-path-list
  (testing "when parameters are invalid"
    (testing "when path-list is invalid"
      (ok (eq (json-value-with-path-list 'invalid-path a-json-object) nil)))
    (testing "when json is invalid"
      (ok (eq (json-value-with-path-list '(:foo) 'invalid-json) nil))))
  (testing "when parameters are valid"
    (testing "when path list is empty"
      (ok (eq (json-value-with-path-list nil a-json-object) a-json-object)))
    (testing "when path list contains one component"
      (testing "when component is an index"
        (testing "when json is a json array"
          (testing "when index is within bounds"
            (ok (eq (json-value-with-path-list '(0) (list a-json-object)) a-json-object)))
          (testing "when index is out of bounds"
            (ok (eq (json-value-with-path-list '(1) (list a-json-object)) nil))))
        (testing "when json is not a json array"
          (ok (eq (json-value-with-path-list '(0) a-json-object) nil))))
      (testing "when component is a keyword"
        (testing "when json is a json object"
          (testing "when keyword matches a property name"
            (ok (string-equal (json-value-with-path-list '(:foo) a-json-object) "bar")))
          (testing "when keyword does not match any property name"
            (ok (eq (json-value-with-path-list '(:unknown) a-json-object) nil))))
        (testing "when json is not a json object"
          (ok (eq (json-value-with-path-list '(:foo) (list a-json-object)) nil)))))
    (testing "when path list contains multiple components"
      (testing "when first component is an index"
        (testing "when json is a json array"
          (testing "when index is within bounds"
            (ok (eq (json-value-with-path-list '(0 :foo) (list a-json-object)) (json-value-with-path-list '(:foo) a-json-object))))
          (testing "when index is out of bounds"
            (ok (eq (json-value-with-path-list '(1 :foo) (list a-json-object)) nil))))
        (testing "when json is not a json array"
          (ok (eq (json-value-with-path-list '(0 :foo) a-json-object) nil))))
      (testing "when first component is a keyword"
        (testing "when json is a json object"
          (testing "when keyword matches a property name"
            (ok (string-equal 
              (json-value-with-path-list '(:qux :foo) a-nested-json-object) 
              (json-value-with-path-list '(:foo) a-json-object))))
          (testing "when keyword does not match any property name"
            (ok (eq (json-value-with-path-list '(:unknown :foo) a-nested-json-object) nil))))
        (testing "when json is not a json object"
          (ok (eq (json-value-with-path-list '(:qux :foo) (list a-nested-json-object)) nil)))))))

(deftest test-make-path-list 
  (testing "when path is valid"
    (ok (equal (make-path-list "foo") '(:foo)))
    (ok (equal (make-path-list "foo/") '(:foo)))
    (ok (equal (make-path-list "/foo") '(:foo)))
    (ok (equal (make-path-list "foo/bar") '(:foo :bar)))
    (ok (equal (make-path-list "foo/bar/0") '(:foo :bar 0)))
    (ok (equal (make-path-list "0") '(0)))
    (ok (equal (make-path-list "01") '(1)))
    (ok (equal (make-path-list "") nil))
    (ok (equal (make-path-list "//") nil))
    (ok (equal (make-path-list "0/foo") '(0 :foo)))
    (ok (equal (make-path-list "0/foo/bar") '(0 :foo :bar))))
  (testing "when path is invalid"
    (ok (equal (make-path-list :foo) nil))
    (ok (equal (make-path-list 'foo) nil))))

(deftest test-json-value-with-path 
  (testing "convenience helper with path"
    (ok (expands '(json-value-with-path "0/foo" (list a-json-object))
                 `(json-value-with-path-list (make-path-list "0/foo") (list a-json-object))))))