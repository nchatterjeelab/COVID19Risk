#!/bin/bash

cd /users/jjin/covid/
  for b in 11
do
qsub -cwd -l mem_free=20G,h_vmem=20G,h_fsize=200g sh/Bootstrap-medicare.sh $b
done