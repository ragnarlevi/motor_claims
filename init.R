install.packages("CASdatasets", repos = "http://dutangc.free.fr/pub/RRepos/", type="source")
library(tidyverse)

require(MASS)
library(CASdatasets)
?CASdatasets

data(freMTPL2freq)
str(freMTPL2freq)
