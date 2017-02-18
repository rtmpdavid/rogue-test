#!/bin/bash
sbcl --eval "(ql:quickload :rogue-test :silent t :prompt nil)" \
     --eval "(rogue-test::start-with-swank)"
     --non-interactive


