#
# Check ESSes versus number of taxa #326
#
library(ggplot2)

df <- read.csv(
  "~/GitHubs/razzo_project/overview.md",
  sep = "|",
  stringsAsFactors = FALSE
)
df$mean_ess <- as.numeric(df$mean_ess)
df$mean_n_taxa <- as.numeric(df$mean_n_taxa)
df <- df[-1,]
names(df)
ggplot(df, aes(x = mean_n_taxa, y = mean_ess)) + geom_point() + geom_smooth(method = "lm")

