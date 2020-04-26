(asdf:defsystem #:random-jsf
  :description "Jenkins Small Fast 32-bit pseudo random number generator"
  :author "Jeremy Mates <jeremy.mates@gmail.com>"
  :license "ISC"
  :version "0.0.1"
  :serial t
  :components ((:static-file "README")
               (:static-file "LICENSE")
               (:file "jsf")))
