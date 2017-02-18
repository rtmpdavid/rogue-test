;;;; rogue-test.asd

(asdf:defsystem #:rogue-test
  :description "Describe rogue-test here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (:swank
	       :cl-ncurses
	       :trivial-signal)
  :components ((:file "package")
               (:file "rogue-test")))

