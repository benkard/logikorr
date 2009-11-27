#! /bin/bash

DIR=`dirname "$0"`
sbcl <<EOF
  (require :asdf)
  (pushnew "$DIR/" asdf:*central-registry*)
  (require :logikorr)
  (in-package logikorr-ht)
  (start-logikorr)
EOF

