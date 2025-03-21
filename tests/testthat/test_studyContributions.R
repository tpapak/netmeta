#
library(devtools)
load_all()

data1 <- read.csv("tests/NMA_data_binary_FE.csv")

p1 <- pairwise(treat = T, event = R, n = N,
   studlab = Study, data = data1, sm = "OR")
 
net1 <- netmeta(p1)
cm <- netcontrib(net1)


rcm <- netcontrib(net1, method = "r")

