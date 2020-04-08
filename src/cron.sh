#!/usr/bin/env bash

cd "${0%/*}"
cd ..
git pull
Rscript -e "drake::r_make()"
git add --all
git commit -m "Cron auto commit."
git push
