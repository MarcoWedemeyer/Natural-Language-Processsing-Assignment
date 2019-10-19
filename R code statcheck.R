# Load packages
packages <- c("wosr","statcheck", "rcrossref","crminer","xml2","stringr","margins","DT","rvest","RSelenium")
lapply(packages,require,character.only=TRUE)

# Set directory
directory <- "C:\\Users\\marco\\Downloads\\Learning R\\Pvalueproject\\Articles\\" #read in html

# Check all files in the directory
results <- checkdir(directory, subdir = TRUE)
source <- results$Source 
error <- results$Error

df <- data.frame(source,error)
write.csv(df,"C:\\Users\\marco\\Documents\\1) MSc Data Science and Society\\Natural Language Processing\\Statcheck results.csv", row.names = FALSE)
