#$ -cwd

module load R/3.6.1
#$ -o /users/jjin/covid/logfile
#$ -e /users/jjin/covid/logfile
cd  /users/jjin/covid/
  
Rscript code/Bootstrap-state.R $1 > out/Bootstrap-state.$1.Rout &
  
wait