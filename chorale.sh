#!/bin/bash

mkdir output
cd output && runhaskell ../chorale.hs ../progression.txt > temp.ly && cp ../template1.ly test.ly && cat temp.ly >> test.ly && cat ../template2.ly >> test.ly && rm temp.ly && lilypond test.ly 