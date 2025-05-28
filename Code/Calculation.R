
current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

library(tidyquant)
library(tidyverse)
library(tsibble)



getSymbols("SOFR", src="FRED")
SOFR |> fortify.zoo(melt = F) |> as_tsibble()


