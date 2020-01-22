#Which version?
paste0(R.Version()[c("major","minor")], collapse = ".")

#UpdateR
#installing/loading the latest installr package:
#install.packages("installr"); library(installr) # install+load installr
#updateR() # updating R.

#keras
install.packages("keras")
library(keras)
install_keras()
install_keras(tensorflow = "gpu")

#or

install_keras(method = c("auto", "virtualenv", "conda"),
              conda = "auto", version = "default", tensorflow = "default",
              extra_packages = c("tensorflow-hub"))

install_keras(tensorflow = "gpu")

#working directory
getwd()
setwd("C:/Users/Vijaya Shree/Documents/R/datasets")