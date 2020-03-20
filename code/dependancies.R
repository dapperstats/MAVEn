depend <- c("tidyverse", "lubridate", "rmarkdown", "pander", "knitr")
ndepend <- length(depend)
present <- installed.packages()[ , "Package"]
needed <- depend[!(depend %in% present)] 
n_needed <- length(needed)

if(n_needed > 0){
  install.packages(needed)
}
for(i in 1:ndepend){
  suppressMessages(eval(bquote(library(.(depend[i])))))
}
