# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions
alias q='squeue -u $USER --long'
alias errors='egrep -iR "error" --include=*.out'
alias errors_useful='egrep -iR "Error in" --include=*.out'
alias pulisci='scancel -t PENDING -u $USER'
#alias cleaning2='chmod +x /home/p274829/mbd_like/cleaning.bash' #execute cleaning.bash
#alias chiuditutto='scancel -u $USER'
alias chiuditutto='scancel --user=$USER --partition=nodes && scancel --user=$USER --partition=regular'
alias distruggi='rm -r mbd_like/*' #rimuove tutto dalla cartella mbd_like: usare con cautela!
alias Rrun='module load R/3.3.1-foss-2016a && Rscript'
#alias openR='module load R/3.3.1-foss-2016a && R'
alias openR='module load R/3.4.4-foss-2018a-X11-20180131 && R'
#alias OpenR='module load R/3.3.1-foss-2016a && R'
alias OpenR='module load R/3.4.4-foss-2018a-X11-20180131 && R'
alias slurmcheck='ls | find . -name "slurm*"' #/home/p274829/mbd_like
alias slurmclean='ls | find . -name "slurm*" | xargs rm' #/home/p274829/mbd_like
alias jobclean='ls | find . -name "mbd_ML_job_a*" | xargs rm' #/home/p274829/mbd_like
alias cleaning='slurmclean && jobclean'
alias sbatchqui='chmod +x ' #execute on local node
#alias startmbdML='sbatch /home/$USER/mbd_like/start_mbd_ML.bash' 
#alias startmbdML2='sbatch /home/$USER/mbd_like2/start_mbd_ML2.bash' 
alias check_user_status='sshare -u $USER'