#!/usr/bin/env bash

cd "${0%/*}"
cd ..
/usr/local/bin/Rscript -e "drake::r_make()"
git add --all
git commit -m "Cron auto commit."
git push
