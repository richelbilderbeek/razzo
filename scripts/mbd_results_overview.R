#overview

mbd_results_overview = function(){

  lambdas = c(0.2)
  mus = c(0,0.15)
  nus = c(1,2,2.5)
  qs = c(0.05,0.1,0.2)

  (sim_settings <- matrix(NA, ncol = 5));
  sim_settings = cbind(aaaa<-expand.grid(lambdas,mus,nus,qs),rep(1000,dim(aaaa)[1]),rep(0,dim(aaaa)[1]),rep(0,dim(aaaa)[1]));
  colnames(sim_settings) <- c("lambda","mu","nu","q","#sims","collected","completed");

  home_dir = substring(getwd(),1,21)
  data_folder = paste(home_dir,"/Progress/RQ1 - Multiple Births/Results + Reports/4_parameters/",sep = '')
  # substring(data_folder,1,21)
  # data_folder = "F:/Dropbox/University/Progress/RQ1 - Multiple Births/Results + Reports/4_parameters/"
  folder_list = dir(path = data_folder )

  for (y in 1:dim(sim_settings)[1]){
    folder_name =  NULL
    for (i in 1:length(sim_settings[y,1:4])){
      vec=unname(unlist( sim_settings[y,1:4] ))
      folder_name = paste(folder_name,toString(vec[i]),sep = '')
      if (i!=length(vec)){folder_name = paste(folder_name,"-",sep = '')}
    }
    present <- max(grepl(folder_name, folder_list))==1
    collected = 0
    if (present!=0){
      right_folder = folder_list[ max( which(grepl(folder_name, folder_list)==1) ) ]
      collected = length( list.files(path = paste(data_folder,right_folder,sep = ''),pattern = ".*.txt") )
    }
    sim_settings[y,6] <- collected
    sim_settings[y,7] <- ( collected>=(0.9*sim_settings[y,5]) )
  };

  return( sim_settings )
}
