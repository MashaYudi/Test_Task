library("xlsx")
library("tidyverse")
library("gridExtra")

#set the level of significance for t-test
alpha_t <- 0.1

#set the path to data files
#path <- readline(prompt="Enter source path: ")

## FULL PATH MAY BE NEEDED HERE
path <- "/Sources/"
filename1 <- "T_NOR_all.xlsx"
filename2 <- "180223_T_aSyn3_animal_data.xlsx"

#load the data
my_data <- read.xlsx2(paste0(path, filename1), 1)
my_data_strains <- read.xlsx2(paste0(path, filename2), 1)

#select important columns
# -- about novel oblect
ball_data <- my_data %>% select(1,34:44)
# ---- setting the right column names
colnames(ball_data) <- as.character(unlist(ball_data[1,]))
ball_data = ball_data[-1, ]

# -- about block
block_data <- my_data %>% select(1,23:33)
# ---- setting the right colugetwd()mn names
colnames(block_data) <- as.character(unlist(block_data[1,]))
block_data = block_data[-1, ]

#some magic with animal numbers(for merging)
my_data_strains$animal.ID..Ear..<-gsub("^.{6}","",my_data_strains$animal.ID..Ear..)
my_data_strains$animal.ID..Ear..<-gsub(".{1}$","",my_data_strains$animal.ID..Ear..)

# temporal tables with the corresponding strains
temp_ball_data <- merge(ball_data, my_data_strains[, c("animal.ID..Ear..", "strain.")]
                        , by.x=c("No."), by.y=c("animal.ID..Ear.."))

temp_block_data <- merge(block_data, my_data_strains[, c("animal.ID..Ear..", "strain.")]
                        , by.x=c("No."), by.y=c("animal.ID..Ear.."))

# prep.renaming for correct merging
temp_ball_data = plyr::rename(temp_ball_data,c("Duration (s)"  = "Duration_ball",
                                         "Visits" = "Visits_ball"))

temp_block_data = plyr::rename(temp_block_data,c("Duration (s)"  = "Duration_block",
                                               "Visits" = "Visits_block"))

#final table for t-test
t_test_table <- merge(temp_ball_data[, c("No.", "Duration_ball","Visits_ball")], 
                      temp_block_data[, c("No.","Duration_block","Visits_block", 
                                          "strain.")], by = "No.")

#some R magic for difference calc.
t_test_table$Duration_ball <- as.numeric(as.character(t_test_table$Duration_ball))
t_test_table$Duration_block <- as.numeric(as.character(t_test_table$Duration_block))
t_test_table$Visits_ball <- as.numeric(as.character(t_test_table$Visits_ball))
t_test_table$Visits_block <- as.numeric(as.character(t_test_table$Visits_block))

#adding two difference-columns to t-test_table
t_test_table$duration_diff <- t_test_table$Duration_ball - t_test_table$Duration_block
t_test_table$visits_diff <- t_test_table$Visits_ball - t_test_table$Visits_block

#checking if there's only two strains to test
#only that case was considered
if(length(unique(t_test_table$strain.)) == 2){
  
  #histograms for 2 new columns
  # -- duration
  p1 <- ggplot(t_test_table, aes(duration_diff)) +
    geom_histogram(fill = "white", color = "grey30") +
    facet_wrap(~ strain.)
  # -- number of visits
  p2 <- ggplot(t_test_table, aes(visits_diff)) +
    geom_histogram(fill = "white", color = "grey30") +
    facet_wrap(~ strain.) 
  
  grid.arrange(p1, p2, nrow = 2)
  
  #t-tests for duration and visits
  visits_res <- t.test(visits_diff ~ strain., t_test_table)
  duration_res <- t.test(duration_diff ~ strain., t_test_table)
  
  print("T-test for number of visits")
  print(visits_res)
  
  print("T-test for duration of visits")
  print(duration_res)
  
  if(visits_res$p.value<alpha_t) {
    print("Number of visits: Difference is significant")
    print(paste0("alpha = ", alpha_t, ", p-value = ",  visits_res$p.value))
  } else {
    print("Number of visits: Difference is not significant")
    print(paste0("alpha = ", alpha_t, ", p-value = ",  visits_res$p.value))}
  
  if(duration_res$p.value<alpha_t) {
    print("Duration of visits: Difference is significant")
    print(paste0("alpha = ", alpha_t, ", p-value = ",  duration_res$p.value))
  } else {
    print("Duration of visits: Difference is not significant")
    print(paste0("alpha = ", alpha_t, ", p-value = ",  duration_res$p.value))
  }
}

