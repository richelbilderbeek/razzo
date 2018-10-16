## ----setup, results = "hide"---------------------------------------------
if (!require(mbd)) {devtools::install_github("Giappo/mbd")}
if (!require(TESS)) {
  install.packages("TESS", repo = "https://lib.ugent.be/CRAN/")
}
if (!require(pirouette)) {
  devtools::install_github("richelbilderbeek/babette")
  devtools::install_github("richelbilderbeek/pirouette")
}

## ------------------------------------------------------------------------
library(razzo)

## ------------------------------------------------------------------------
folder_name <- tempdir()

