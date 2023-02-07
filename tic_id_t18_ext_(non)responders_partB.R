#TIC - Creating a list of people who have not yet responded to their (extended-deadline) t18 survey
#Ania Jaroszewicz
#Last updated: 7 Feb 2023

#Purpose: We are inviting TIC participants who (a) have not responded for the last year; and (b) have not completed their t18 survey during the regular survey window or thereafter; to complete their t18 survey (specifically, the "short" 15 min version) within an extended time period. This file identifies the most recent list of people who meet the (a) and (b) criteria above so we know who to nudge to complete the survey. It does this by starting with the calling_priorities.csv spreadsheet, slimming it down to focus on the people who have not completed any surveys *on time* since t0, t3, or t6, and then REMOVES anyone who has completed the short t18 survey outside the standard window. Finally, it outputs a file of everyone who meets this criteria (across all the waves). These are the people who should be prioritized in the data acquisition efforts (calling them, SMS/email, sending postcards). 
#The code runs as a construction of 2 files. This is Part B, and is called in the Part A (and Part AC) code.

#This file uses 3 datasets called in part A: 
#1: the calling_priorities.csv spreadsheet, which was created in tic_calc_pastsurveyscompleted_[date].R. The calling_priorities.csv spreadsheet changes each time a survey window closes. It needs to be updated after each survey window closes but won't change during the time that no survey is active. [this is the complete list of all participants with a column for the last survey they completed on time]
#2: the Qualtrics survey data from the short (15 min) t18 survey [so we can pull out the people who do NOT need to be bugged again]
#3: the list of links for the survey [so we know what link to send participants who need to be nudged]



####IDENTIFY WHO STILL NEEDS TO BE NUDGED TO COMPLETE THEIR SHORT T18 SURVEY ----

#Slim down: only keep people who are eligible to get the short t18 survey: people who have not completed a survey since t6 or earlier. 
callingpriorities2 <- callingpriorities %>%
  filter(lastcompletedsurvey<=6) 

#Slim down the survey responses to make it easier to work with
survey_responses_short <- raw_survey_responses_short %>%
  dplyr::select(entity_uuid, 
                t,  
                finished = sawlastpage,
                recorded_datetime = RecordedDate)

#Join the calling priorities and short survey completion lists and pull out people who have completed the short survey recently
callingpriorities2_xrecresponders <- left_join(callingpriorities2, survey_responses_short, by="entity_uuid")
callingpriorities2_xrecresponders <- callingpriorities2_xrecresponders %>%
  mutate(finished=ifelse(is.na(finished), 0, finished)) %>%
  filter(finished!=1) 
callingpriorities2_xrecresponders <- callingpriorities2_xrecresponders %>%
  dplyr::select(-finished, -recorded_datetime, -t)

#Clean up links list
links2 <- links %>%
  filter(t==18) %>%
  dplyr::select(-wave)

#Add survey links to list of people who need to be nudged to complete their survey 
ppltonudge_links <- left_join(callingpriorities2_xrecresponders, links2, by="entity_uuid")
  

####CLEAN UP & OUTPUT THE LIST OF PEOPLE WHO NEED TO BE NUDGED TO COMPLETE THEIR SHORT T18 SURVEY ---- 
ppltonudge_links <- ppltonudge_links %>%
  arrange(wave, lastcompletedsurvey, entity_uuid) 

#Print some summary messages for easy data checking: 
print(paste0("Number of wave1 people who need a reminder: ", sum(ppltonudge_links$wave == 1)))
print(paste0("Number of wave2 people who need a reminder: ", sum(ppltonudge_links$wave == 2)))
print(paste0("Number of wave3 people who need a reminder: ", sum(ppltonudge_links$wave == 3)))
print(paste0("Number of wave4 people who need a reminder: ", sum(ppltonudge_links$wave == 4)))
print(paste0("Number of wave5 people who need a reminder: ", sum(ppltonudge_links$wave == 5)))
print(paste0("Number of wave6 people who need a reminder: ", sum(ppltonudge_links$wave == 6)))
print(paste0("Total number of people who need a reminder: ", nrow(ppltonudge_links)))
print("You're done! Now just check your folder for the file.")

todaysdate <- today()
filename <- paste0("t18_nonresponders_as_of_", todaysdate, ".csv")
write_csv(ppltonudge_links, filename)
