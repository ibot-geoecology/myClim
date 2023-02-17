library(myClim)

tubedb <- TubeDB(url="https://tubedb.ibot.cas.cz", user="", password="")
data <- mc_read_tubedb(tubedb, region="ckras", plot=c("TP_KAR_19", "TP_KODA_61"))
data_all <- mc_read_tubedb(tubedb, region="rokle")
