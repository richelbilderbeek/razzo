#!/bin/bash

echo "home_dir = substring(getwd(),1,13)" > Rsetup.R
echo "libs_dir = paste0(home_dir,'/mbd_like/libs')" >> Rsetup.R
echo "lib_files = list.files(pattern=paste0('[.]tar'),path=libs_dir, full.names=TRUE)" >> Rsetup.R
echo "mylibrary = paste0(home_dir,'/R/x86_64-pc-linux-gnu-library/3.3/')" >> Rsetup.R
#echo "suppressWarnings( install.packages(lib_files, repos = NULL, lib = mylibrary) )" >> Rsetup.R
#echo "install.packages('expoRkit', lib = mylibrary,repos='http://cran-mirror.cs.uu.nl/')" >> Rsetup.R
#echo "install.packages('testit', lib = mylibrary,repos='http://cran-mirror.cs.uu.nl/')" >> Rsetup.R
#echo "install.packages('DDD', lib = mylibrary,repos='http://cran-mirror.cs.uu.nl/')" >> Rsetup.R
echo "install.packages(lib_files, repos = NULL, lib = mylibrary, dependencies = T)" >> Rsetup.R
#echo "install.packages('mbd', contriburl=paste('file:///',pkg_path2,sep = ''),dependencies = TRUE) >> Rsetup.R"

echo "#!/bin/bash" > Rsetup2
echo "#SBATCH --time=00:59:00" >> Rsetup2
#echo "#SBATCH --output=testinst.out" >> Rsetup2
echo "module load R/3.3.1-foss-2016a" >> Rsetup2
echo "Rscript Rsetup.R" >> Rsetup2
echo "rm Rsetup.R" >> Rsetup2
echo "rm Rsetup2" >> Rsetup2

sbatch Rsetup2


