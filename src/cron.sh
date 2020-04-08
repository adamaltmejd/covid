#!/usr/bin/env bash

cd "${0%/*}"
cd ..
Rscript -e "drake::r_make()"
git add --all
git commit -m "Cron auto commit."
git push
