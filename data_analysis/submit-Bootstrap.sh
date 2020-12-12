#!/bin/bash

cd /users/jjin/covid/
for b in {1..1000}
do
qsub -cwd -l mem_free=20G,h_vmem=20G,h_fsize=200g sh/Bootstrap.sh $b
done