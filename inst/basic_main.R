# This is a sample main file for a module
library(faosws)

# If we're on the server, there's a global variable that defines with share
# folder
R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")

# This return FALSE if on the Statistical Working System
if(CheckDebug()){

  library(faoswsModules)
  SETTINGS <- ReadSettings("sws.yml")

  # If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH <- SETTINGS[["share"]]

  # Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])

  # Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])

}

if(is.null(swsContext.computationParams$numberInput)){
  swsContext.computationParams$numberInput <- 2
}

# Read in data from session key
raw_data <- GetData(swsContext.datasets[[1]])

processed_data <- raw_data[, Value := multiplyValue(Value, swsContext.computationParams$numberInput)]

# Save data back to a SWS dataset
stats <- SaveData("agriculture", "aproduction", processed_data)

# Output the stats
print(sprintf("%s objects inserted
              %s objects discarded
              There were %s warnings",
      stats[["inserted"]],
      stats[["discarded"]],
      length(stats[["warnings"]]))
      )
