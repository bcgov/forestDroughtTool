# treeRiskRatings_raw

# describe process to create data from treeRiskRatins.csv

treeRisk <- readr::read_csv("data-raw/treeRiskRatings.csv")

save(treeRisk,file="./data/treeRisk.rda")
