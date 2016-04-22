# This is a sample main file for a module
library(faosws)

if(CheckDebug()){

  library(faoswsModules)
  SETTINGS <- ReadSettings("sws.yml")

  R_SWS_SHARE_PATH <- SETTINGS[["share"]]

  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])

}

raw_data <- GetData(swsContext.datasets[[1]])

processed_data <- raw_data[, Value := Value * 2]

stats <- SaveData("agriculture", "aproduction", processed_data)

print(sprintf("%s objects inserted
              %s objects discarded
              There were %s warnings",
      stats[["inserted"]],
      stats[["discarded"]],
      length(stats[["warnings"]]))
      )
