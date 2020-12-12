#!/bin/bash

cd /users/jjin/covid/
for b in {1..1000}
do
qsub -cwd -l mem_free=10G,h_vmem=10G,h_fsize=200g sh/Bootstrap-state.sh $b
done