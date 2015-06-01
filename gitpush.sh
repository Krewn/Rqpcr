#!/bin/bash
for f in *.R~; do rm $f; done
rm - fr .git
git init
git add .
git status
git commit -m $1
git remote add origin https://github.com/Krewn/Rqpcr.git
git remote -v
git push -f origin master

