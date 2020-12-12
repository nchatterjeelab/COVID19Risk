#$ -cwd

module load R/3.6.1
#$ -o /users/jjin/covid/logfile
#$ -e /users/jjin/covid/logfile
cd  /users/jjin/covid/
  
Rscript code/bootstrap-medicare.R $1 > out/Bootstrap-medicare.$1.Rout &
  
wait