#MD_DATA_ANALYSIS
#setup
rm(list=ls());
mbd_data_analysis <- function(sim_pars, max_sims = 1000, account_name = "274829"){
  # results = mbd:::mbd_import_data()
  # sim_pars = c(0.2,0.15, 2,0.05); max_sims = 1000; account_name = "cyrus"
  Npars = length(sim_pars);
  system.time( data <- mbd:::get_all_data_from_cluster(sim_pars = sim_pars, max_sims = max_sims, account_name = account_name))
  results = data$results
  path = data$path
  working_account = data$working_account
  #OUTPUTS
  #all data
  all_data_title = paste("\n CORRELATION ANALYSIS (All trees)\n", N ," trees (",max_sims-N," failed)",sep = '')
  all_data_pdfname = "correlation_analysis_all_data"
  quantiles <- mbd:::percentiles_function(results = results,sim_pars = sim_pars)
  mbd:::correlation_analysis(results = results,sim_pars = sim_pars,titolo=all_data_title,pdfname=all_data_pdfname,path=path,openit = 1)

  #multiple births vs single births analysis
  # sbd_res=results[results[,(Npars+2)]==0,];print(how_many_single_births<-dim(sbd_res)[1])
  # mbd_res=results[results[,(Npars+2)]!=0,];print(how_many_multiple_births<-dim(mbd_res)[1])
  # mbd:::percentiles_function(results = sbd_res,sim_pars = sim_pars)
  # mbd:::correlation_analysis(results = sbd_res,path=path,titolo = "Single Birth Only",pdfname = "correlation_analysis_single_birth_only",sim_pars = sim_pars)
  # mbd:::percentiles_function(results = mbd_res,sim_pars = sim_pars)
  # mbd:::correlation_analysis(results = mbd_res,path=path,titolo = "At least one multiple birth",pdfname = "correlation_analysis_at_least_one_multiple_birth",sim_pars = sim_pars)
  address = paste(path, "/account_name", sep = '')
  save(working_account, file= "account_name")
  return(list(results = results, quantiles = quantiles))
}
mbd_results_overview = function(){

  lambdas = c(0.2)
  mus = c(0,0.15)
  nus = c(1, 2, 2.5)
  qs = c(0.05,0.1,0.2)

  sim_settings <- matrix(NA, ncol = 5);
  sim_settings = cbind(
    aaaa<-expand.grid(lambdas,mus,nus,qs), # parameters
    rep(1000,dim(aaaa)[1]),                # total number of sims
    rep(0,dim(aaaa)[1]),                   # number of collected results
    rep(0,dim(aaaa)[1]),                   # are they enough?
    rep(0,dim(aaaa)[1])                    # account name: 1 is giovanni, 2 is cyrus
    );
  colnames(sim_settings) <- c("lambda","mu","nu","q","#sims","collected","completed","account");

  home_dir = substring(getwd(), 1, 21)
  data_folder = paste(home_dir,"/Progress/RQ1 - Multiple Births/Results + Reports/4_parameters/",sep = '')
  # data_folder = "F:/Dropbox/University/Progress/RQ1 - Multiple Births/Results + Reports/4_parameters/"
  folder_list = dir(path = data_folder )

  for (y in 1:dim(sim_settings)[1]) {
    folder_name <- NULL
    for (i in 1:length(sim_settings[y, 1:4])) {
      vec <- unname(unlist( sim_settings[y, 1:4] ))
      folder_name <- paste(folder_name,toString(vec[i]),sep = '')
      if (i != length(vec)) {
        folder_name = paste(folder_name,"-",sep = '')
      }
    }
    present <- max(grepl(folder_name, folder_list)) == 1
    collected = 0
    if (present != 0) {
      right_folder <- folder_list[
        max( which(grepl(folder_name, folder_list) == 1)) 
      ]
      collected = length(
        list.files(
          path = paste(data_folder,right_folder,sep = ''),
          pattern = ".*.txt"
        ) 
      )
      account_file <- paste(data_folder,right_folder,"/account_name",sep = '')
    }
    sim_settings[y,6] <- collected
    sim_settings[y,7] <- (collected >= (0.9*sim_settings[y,5]))

    if (exists("account_file")) {
      if (file.exists(account_file)) {
          load(file = account_file)
      } else {
        working_account = "noone"
      }
    }

    sim_settings[y,8] <- working_account
  }

  return( sim_settings )
}

#execution
mbd_results_overview()
sim_pars = c(0.2,0, 2.5,0.05); max_sims = 1000; account_name = "cyrus"#account_name = "cyrus";
system( results<-mbd_data_analysis(sim_pars = sim_pars, max_sims = max_sims, account_name = account_name )$results )

