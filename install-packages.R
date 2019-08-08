try(unCheckpoint(), silent = TRUE)

if (!("Rserve" %in% rownames(installed.packages())))
    install.packages("Rserve", repos = "http://www.rforge.net/")

if (!("checkpoint" %in% rownames(installed.packages())))
    install.packages("checkpoint")

library(checkpoint)

checkpoint(snapshotDate = "2019-04-15") # default for MRO 3.5.3

if (packageVersion("waiter") < '0.0.4')
    devtools::install_github("JohnCoene/waiter", ref = "32ea390845")
