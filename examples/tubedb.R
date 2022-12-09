library(myClim)

tubedb <- TubeDB(url="https://tubedb.ibot.cas.cz", user="", password="")
data <- mc_read_tubedb(tubedb, region="ckras", plot=c("HOSEK-606", "HOSEK-618"))
data_all <- mc_read_tubedb(tubedb, region="rokle")
