rm(list = ls())

analyze_data1 <- function(max_sims = 1000, quantiles_choice = c(.25, .50, .75), results_folder = "new_results"){

  home_dir = substring(getwd(),1,21)
  folder_name = paste0(home_dir,"/Progress/RQ1-multiple-birth-death/results/",results_folder,"/")
  datasets <- list.files(folder_name, pattern = "^[0]")
  # datasets <- list.files( folder_name <- "F://Dropbox//University//Progress//RQ1 - Multiple Births//Results + Reports//final results2",pattern = "^[0]")

  dataset_pars <- vector("list",Nd <- length(datasets))
  for (d in 1:Nd)
  {
    dataset_pars[[d]] <- as.numeric(  unlist( strsplit(x = datasets[d],split = "-") )  )
  }

  # single data_set analysis
  quantiles = vector("list",Nd); Nsims = rep(0,Nd)
  for (d in 1:Nd){
    print(d)
    sim_pars = dataset_pars[[d]]; Npars = length(sim_pars); parnames = c("lambda","mu","nu","q")
    local_path <- paste(folder_name,"//",datasets[d],sep = '');
    res_files <- list.files(pattern=paste('[.]txt',sep = ''),path=local_path, full.names=TRUE); #length(res_files)

    if(length(res_files)!=0)
    {
      for (s in 1:length(res_files))
      {
        fileData <- utils::read.table(file=res_files[s], header = FALSE, sep=",")
        ifelse(exists("targetTable"),targetTable <- rbind(targetTable, fileData), targetTable <- fileData)
      }

      results0 <- targetTable; rm(targetTable); names(results0) = (c(parnames,"LL","multiple_born","number_of_tips","percentage_multiple_species","tree_id"))
      results  <- results0[rowSums(results0[,1:(Npars+1)]==rep(-1,(Npars+1)))!=(Npars+1),];

      Nsims[d] <- dim(results)[1]
      if ( max(results[,1:Npars]==-1) ){print("You are considering results that are = -1. Be careful!")}

      quantiles[[d]] <- mbd:::percentiles_function(results = results,sim_pars = sim_pars, printit = 0, quantiles_choice = quantiles_choice)
      titolo <- paste("Correlation analysis with ", Nsims[d],"/",max_sims," trees.",sep = '')
      pdfname <- paste("Correlation ", datasets[d], sep = '')
      mbd:::correlation_analysis(results = results,sim_pars = sim_pars,titolo = titolo,
                                 pdfname = pdfname,path = local_path,openit = 0,mother_folder = folder_name)
    }
}

  # for (d in 1:Nd){print(quantiles[[d]]); print(dataset_pars[[d]])}
  result.table = matrix(NA,nrow = Nd,ncol = prod(dim(quantiles[[1]])) + Npars )
  quantiles_names <- format(round(quantiles_choice,2),nsmall = 2)
  lambda_quantiles_names <- paste0("lambda",quantiles_names)
  mu_quantiles_names <- paste0("mu",quantiles_names)
  nu_quantiles_names <- paste0("nu",quantiles_names)
  q_quantiles_names <- paste0("q",quantiles_names)
  for (d in 1:(Nd))
  {
    result.table[d,1:Npars] = dataset_pars[[d]]
    if(Nsims[d]!=0)
    {
      result.table[d,(Npars+1):(Npars+prod(dim(quantiles[[1]])))] = t(matrix(t(quantiles[[d]]),nrow = prod(dim(quantiles[[1]])),byrow = F));
    }
  }
  colnames(result.table) = c("sim.lambda","sim.mu","sim.nu","sim.q",
                             lambda_quantiles_names,
                             mu_quantiles_names,
                             nu_quantiles_names,
                             q_quantiles_names); #result.table
  result.table2 <- format(round(result.table,2),nsmall = 2)
  result.table2 <- cbind(result.table2,Nsims); result.table2
  # install.packages("xlsx");
  library("xlsx")
  xlsname0 = "results_table"; xlsname = xlsname0; xlscount = 2;
  while (file.exists(paste0(folder_name,"//",xlsname,".xlsx"))){xlsname = paste0(xlsname0,xlscount); xlscount = xlscount + 1}
  write.xlsx(x = result.table2, file = paste(folder_name,"//",xlsname,".xlsx",sep = ''), sheetName = "age=10; #sims=1000", row.names = FALSE)
  return(result.table)
}

analyze_data2 <- function(result.table, error_bars = 0){

  library(ggplot2); library(RColorBrewer)

  res <- result.table[apply(!is.na(result.table), 1, prod)!=0,]
  res2 <- res[res[,4]!=0.05,]
  pippo <- as.data.frame(res2)
  # str(pippo)
  # colnames(pippo) = c("sim.lambda","sim.mu","sim.nu","sim.q",
  #                     "lambda.25","lambda.50","lambda.75",
  #                     "mu.25","mu.50","mu.75",
  #                     "nu.25","nu.50","nu.75",
  #                     "q.25","q.50","q.75"); #result.table

  colnames(pippo) <- colnames(result.table)

  pippo2 <- cbind(pippo, row(pippo)[,1]); colnames(pippo2) <- c(colnames(pippo),"id"); pippo2

  ###
  folder_name <- "F://Dropbox//University//Progress//RQ1 - Multiple Births//Results + Reports//final results"
  setwd(folder_name)

  pippo3 <- pippo2[pippo2[,12]>0.5 & pippo2[,15]>0.01,]
  # col_palette <- rainbow(n = dim(pippo3)[1], s = 0.5)

  pippo_draw <- function(sub_res_table, error_bars = error_bars){
    library(ggplot2)
    palette <- rainbow(n = dim(sub_res_table)[1], s = 0.5)
    bb <- ggplot2:::ggplot(data = sub_res_table, aes(x = sub_res_table$sim.q, y = sub_res_table$sim.nu)) +
      labs( x = "q", y = "nu") +

      geom_point(aes(x = sub_res_table$q0.50 , y = sub_res_table$nu0.50) , col = palette[1:dim(sub_res_table)[1]], size = 10, shape = 13)

      # if(error_bars == 1)
      # {
      #   # bb <- bb +  geom_errorbarh(aes(xmin = sub_res_table$q.50, xmax = sub_res_table$q.50)) +
      #   #             geom_linerange(aes(ymin = sub_res_table$nu.50, ymax = sub_res_table$nu.50))
      #   # bb <- bb +  geom_errorbarh(aes(xmin = sub_res_table[,14], xmax = sub_res_table[,16])) +
      #               # geom_errorbar(aes(ymin = sub_res_table[,11], ymax = sub_res_table[,13]))
      #   bb <- bb + stat_ellipse()
      # }

      bb <- bb +geom_point(aes(x = sub_res_table$sim.q, y = sub_res_table$sim.nu), col = palette[1:dim(sub_res_table)[1]], size = 5) +
      theme_dark() +
      theme(axis.text=element_text(size=12), axis.title = element_text(size=14, face="bold"))

    # if(error_bars == 1)
    # {
    #  # bb <- bb +  geom_errorbarh(aes(xmin = sub_res_table$q.50, xmax = sub_res_table$q.50)) +
    #  #             geom_linerange(aes(ymin = sub_res_table$nu.50, ymax = sub_res_table$nu.50))
    #   bb <- bb +  geom_errorbarh(aes(xmin = sub_res_table[,14], xmax = sub_res_table[,16])) +
    #               geom_linerange(aes(ymin = sub_res_table[,11], ymax = sub_res_table[,13]))
    # }

    return(bb)
  }

  pippo4 <- pippo3[pippo3[,2]==0   ,]; pippo_draw(pippo4)
  pippo5 <- pippo3[pippo3[,2]==0.15,]; pippo_draw(pippo5)

  grDevices::png(filename = "results_mu=0.png")
  pippo_draw(pippo4)
  grDevices::dev.off()
  grDevices::png(filename = "results_mu=0_15.png")
  pippo_draw(pippo5)
  grDevices::dev.off()
}

result.table <- analyze_data1(quantiles_choice = c(0.40, 0.50, 0.60))
analyze_data2(result.table)
