# Set up the cores.
list.of.packages <- c("parallel", "snow")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)){install.packages(new.packages)}
library("snow"); library("parallel")
# Set path to mpi include files and library files
if(.Platform$OS.type == "unix")
{
  mpipath = dirname(dirname(Sys.which('mpirun')))
  mpiinc = paste('--with-Rmpi-include=',mpipath,'/include',sep='')
  mpilib = paste('--with-Rmpi-libpath=',mpipath,'/lib',sep='')
  if(!require("Rmpi"))install.packages("Rmpi", repos="http://cran.rstudio.com/", configure.args=c(mpiinc, mpilib, "--with-Rmpi-type=OPENMPI"))
  library("Rmpi")
}

# Set up my package
if (.Platform$OS.type == "windows")
{
  libs_dir <- dirname(getwd())
  results_dir <- paste0(dirname(getwd()),"/results")
}
if(.Platform$OS.type == "unix")
{
  home_dir <- substring(getwd(),1,13)
  libs_dir <- paste0(home_dir,'/mbd_like/libs')
  results_dir <- paste0(dirname(getwd()),"/sims")
}
lib_files <- list.files(pattern=paste0('[.]tar'),path=libs_dir, full.names=TRUE)
mylibrary <- paste0(home_dir,'/R/x86_64-pc-linux-gnu-library/3.3/')
install.packages(lib_files, repos = NULL, lib = mylibrary, dependencies = T)

# Counts the amount of CPUs minus the master CPU.
if (.Platform$OS.type == "unix")
{
  cpu <- (mpi.universe.size() -1)
  cl  <- makeMPIcluster(cpu)
}
if (.Platform$OS.type == "windows")
{
  cpu <- detectCores() # Number of cores requested.
  hosts <- rep("localhost",cpu)
  cl <- makeCluster(hosts, type = "SOCK")
}

##### ACTUAL SCRIPT

# Load data
simpath  <- getwd()
datapath <- paste0(simpath,"/data")
load(file = paste0(datapath,"/general_settings"))
load(file = paste0(datapath,"/sim_data"))

# Set up ML
initparsopt = c(0.5, 0.1, 1.7, 0.15)
parnames <- c("lambda","mu","nu","q"); Npars <- length(parnames)
idparsopt <- 1:Npars; parsfix <- NULL;

#remove this at the end. just useful for testing on windows local machine
sim_data2 <- sim_data[1:2]
for (i in 1:length(sim_data2))
{
  sim_data2[[i]] <- sim_data2[[i]][1:5]
}

# Do the thing
res <- parLapply(cl = cl, #cluster
                 X = sim_data2, #dataset
                 fun = mbd::mbd_ML, #function you want to apply
                 initparsopt = initparsopt,
                 idparsopt = idparsopt,
                 idparsfix = (1:Npars)[-idparsopt],
                 parsfix = parsfix,
                 missnumspec = 0,
                 cond = cond,
                 soc = soc,
                 tips_interval = tips_interval,
                 tol = c(1E-3, 1E-4, 1E-6),
                 maxiter = 1000 * round((1.25)^length(idparsopt)),
                 changeloglikifnoconv = FALSE,
                 optimmethod = 'simplex',
                 minimum_multiple_births = minimum_multiple_births)

# Save Output
test.string <- toString(sim_pars)
test.string2 <- gsub(", ", "-", test.string)
for (i in 1:4) {test.string2 <- sub("[.]", "_", test.string2)}
results_file_name <- paste0(test.string2,".RData")
save(res, file = paste0(results_dir, results_file_name))

# Close Stuff
gc() #garbage collector
stopCluster(cl)
mpi.exit()
