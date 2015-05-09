if(!require("devtools")) install.packages("devtools")
if(!require("roxygen2")) install.packages("roxygen2")

create("./lClass")
devtools::document()
devtools::install()
