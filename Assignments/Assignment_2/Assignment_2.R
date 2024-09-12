
list.files(path= 'Data/', pattern = "\\.csv$") # code for number 4

csv_files <- list.files(path= 'Data/', pattern = "\\.csv$") #code for number 4

length(csv_files) #code for number 5

df <- read.csv("Data/wingspan_vs_mass.csv") #code for number 6

head(df, 5) # code for number 7

b_files <- list.files(path = "Data/", pattern = "^b", full.names = TRUE, recursive = TRUE) # code for number 8 

for (i in b_files) {
  top_line <- readLines (i, n =1)
print(top_line)
  } # code for number 9 

csv_files_1 <- list.files(path = "Data/", full.names = TRUE, pattern = "\\.csv$",recursive = TRUE)

for (i in csv_files_1) {
  top_line <- readLines (i, n = 1)
print(top_line)
  } #code for number 10
