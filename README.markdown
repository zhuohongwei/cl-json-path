# cl-json-path

A small utility to make extracting values from nested JSON easier. Originally wrote this to work with cl-json but it can be customizable to work for other representations too.

## Usage

```lisp 
;;; using cl-json
(ql:quickload :cl-json)

(with-input-from-string 
    (s (uiop:run-program "curl -v -X GET \"http://api.open-notify.org/astros.json\"" :output :string))
    (cl-json-path:json-value-with-path "" (cl-json:decode-json s)))

;;; output
((:NUMBER . 7) (:MESSAGE . "success")
 (:PEOPLE ((:NAME . "Mark Vande Hei") (:CRAFT . "ISS"))
  ((:NAME . "Oleg Novitskiy") (:CRAFT . "ISS"))
  ((:NAME . "Pyotr Dubrov") (:CRAFT . "ISS"))
  ((:NAME . "Thomas Pesquet") (:CRAFT . "ISS"))
  ((:NAME . "Megan McArthur") (:CRAFT . "ISS"))
  ((:NAME . "Shane Kimbrough") (:CRAFT . "ISS"))
  ((:NAME . "Akihiko Hoshide") (:CRAFT . "ISS"))))

(with-input-from-string 
    (s (uiop:run-program "curl -v -X GET \"http://api.open-notify.org/astros.json\"" :output :string))
    ;;; getting the first item in the list of people
    (cl-json-path:json-value-with-path "people/0" (cl-json:decode-json s)))

;;;output
((:NAME . "Mark Vande Hei") (:CRAFT . "ISS"))
```

## Customization

To customize the utility to work with other json libraries, use
`(cl-json-path:json-value-with-path-list ...)` to setup the relevant predicate functions

It has the following signature:
```
(defun json-value-with-path-list (path-list json &key
                                    (json-array-p-fn #'json-array-p)
                                    (name-component-p-fn #'name-component-p)
                                    (json-object-p-fn #'json-object-p)
                                    (find-property-value-fn #'find-property-value))
...)
```
- path-list is a list of `path component`s which can either be a name or index. cl-json uses keywords for property names, so the example in the Usage section above would translate a path-list of `'(:people 0)`

- json refers to the parsed json representation from the json library of choice.

- json-array-p-fn is a predicate function that tests if the single argument is a json array. The default predicate `json-array-p` checks that the argument is a list of json objects (as tested with `json-object-p`) 

- name-component-p-fn is a predicate function that tests if the single argument is a valid property name. For example, for cl-json's parsed output, property names are keywords (e.g. :name) so the default `name-component-p` predicate tests for keywords.

- json-object-p-fn is a predicate function that tests if the single argument is a json object. The default predicate 
`json-object-p` checks that the argument is a list of pairs.  

- find-property-value-fn is a function that retrives the value of a property (e.g. name) from a json object (person object). It takes in 2 arguments, respectively name and a json object. The default function searches a json object represented as a list of (property-name property-value) pairs by comparing with `eq`. 

## Dependencies
- alexandria
- cl-ppcre

