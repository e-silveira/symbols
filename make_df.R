library(dplyr)
library(magrittr)
library(data.table)

dengue <- read.csv("data/dengue.csv")

df <- select(
    dengue,
    Epiweek_Sunday = data_iniSE,
    Cases_Notified = casos,
    Temperature_Minimum = tempmin,
    Temperature_Mean = tempmed,
    Temperature_Maximum = tempmax,
    Humidity_Minimum = umidmin,
    Humidity_Mean = umidmed,
    Humidity_Maximum = umidmax,
    Receptive_Climate = receptivo
)

fwrite(df, "./data/santa_maria_dengue.csv")
