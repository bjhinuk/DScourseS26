# PS4a_Banerji.R
# JSON data exercise

# Part (a): Download the JSON file using wget
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"')

# Part (b): Print the raw file to console
system('cat dates.json')

# Part (c): Convert JSON to a data frame
library(jsonlite)
library(tidyverse)

mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])

# Part (d): Check object types
cat("\n--- Class of mydf ---\n")
print(class(mydf))

cat("\n--- Class of mydf$date ---\n")
print(class(mydf$date))

# Part (e): Print first 6 rows
cat("\n--- First 6 rows of mydf ---\n")
print(head(mydf))
