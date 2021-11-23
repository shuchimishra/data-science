
install.packages("FSelector")
library(FSelector)
idata = read.csv("info-gain.csv")
information.gain(Default~., idata)

