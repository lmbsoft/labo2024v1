#!/bin/bash
git checkout main
git pull

git fetch upstream
git checkout main
git merge -Xtheirs upstream/main -m "Z manda"

git checkout main
git add .
git commit -m "otro commit"
git push
