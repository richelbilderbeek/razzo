#!/bin/bash

rm Rsetup
echo "home_dir = substring(getwd(),1,13)" > Rsetup.R
echo "libs_dir = paste(home_dir,'/mbd_like/libs',sep = '')" >> Rsetup.R
echo "lib_files = list.files(pattern=paste('[.]tar',sep = ''),path=libs_dir, full.names=TRUE)" >> Rsetup.R
echo "suppressWarnings(install.packages(lib_files, repos = NULL))" >> Rsetup.R

rm Rsetup2
echo "#!/bin/bash" > Rsetup2
echo "#SBATCH --time=00:59:00" >> Rsetup2
echo "module load R/3.3.1-foss-2016a" >> Rsetup2
echo "Rscript Rsetup.R" >> Rsetup2
echo "rm Rsetup.R" >> Rsetup2
echo "rm Rsetup2" >> Rsetup2
sbatch Rsetup2


