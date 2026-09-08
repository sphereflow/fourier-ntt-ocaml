#!/bin/sh
rsync -av --filter=':- .gitignore' --exclude='.git/' . ~/rool/projects/fourier-ntt-ocaml/
