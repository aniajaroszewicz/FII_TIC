#UpTogether TIC: Pulling data from Qualtrics to identify who needs reminders and who needs to be paid for survey completion in the Trust & Invest Collaborative (TIC) study
#Ania Jaroszewicz (ajaroszewicz@hbs.edu) 
#Last updated: 20 December 2022


#Overview: This file should be run in conjunction with TIC_calc_survey_reminders_payments_partA_[date].R (sent to the UpTogether survey admin team). 
#PartA code (run before this file is called):
#(1) Pulls survey data from Qualtrics (the survey platform on which we are collecting data for TIC), a list of dates on which surveys were sent, and a list of links that reflect who 'should have' gotten the survey and what link they should have completed
#Part B code (this):
#(2) Processes that data to identify all the people who have not yet taken a survey that was recently sent out but "should have", and thus need a reminder. This is based on *when* you run the script, and will only say that a person needs a reminder if the relevant survey was sent out 14 days or less from the day that you run the script 
#(3) It also identifies all the people who have taken a survey, and thus need payment 
#(4) It outputs 2 CSV spreadsheets that show who needs a reminder (and, if they need a reminder, what survey link to use) and who needs a payment (and, if they need payment, how much). One of these shows all of the responses to date (with at most one entry per entity_uuid - t combination). The other one shows just the wave and t number that the user specified in partA.


# CLEAN RAW QUALTRICS DATA: DELETE INVALID RESPONSES, UNWANTED VARIABLES  ----
#Take the raw Qualtrics data that was run in Part A. Keep only observations that are not preview and test responses, those that are not missing an entity_uuid or t. Note that those who said they got the link from someone other than TIC/UT get routed in Qualtrics, and are pulled out below because they never "sawlastpage." 
survey_responses <- raw_survey_responses %>% 
  filter(Status!="Survey Preview" & entity_uuid!="NA" & t!="NA")

#Select the variables of interest, put them in the desired order, and rename them
survey_responses <- survey_responses %>%
  dplyr::select(entity_uuid,
         t,
         finished = sawlastpage,
         recorded_datetime = RecordedDate) %>%
  mutate(finished = ifelse(is.na(finished), 0, 1)) 

#Remove people who never saw the last page, either because they started and didn't finish, or because they said they got the link from someone other than UT and then got kicked out (note that this latter group has a recorded date, but they didn't take the full survey)
survey_responses <- survey_responses %>%
  filter(finished==1)

#Pull out just the date from the recorded datetime variable; arrange
survey_responses <- survey_responses %>% 
  mutate(recorded_date=as_date(recorded_datetime)) %>%
  arrange(entity_uuid, t, recorded_date)


# IDENTIFY THE VALID UUIDS [ESP FOR THIS T/WAVE], PREPARE DATAFRAME SKELETON FOR INPUTTING DATA, JOIN WITH SURVEY RESPONSES ----


#Identify everyone who 'should' be responding to a survey this round (and/or has previously been randomized so we have a comprehensive view of who is where in the trial). If it's a t0 survey in question, the links list will only include people who are on that list (who have been approved but not yet enrolled), and thus we need to supplement the list with everyone who has previously been randomized so we can have that comprehensive view. If it's a t3+ survey in question, it already includes everyone who has previously been randomized (and, as far as we know, are still enrolled in the trial), so there's no need to supplement further. 
if(tnum==0){
  valid_uuids <- bind_rows(links, all_randomized_to_date)
} else {
  valid_uuids <- links
}

valid_uuids <- valid_uuids %>%
  group_by(entity_uuid) %>%
  distinct(entity_uuid, .keep_all=TRUE) %>%
  ungroup() %>%
  dplyr::select(entity_uuid, wave)

#For each uuid, create 7 rows, 1 for each t (0,3,6,9,12,15,18). This will later help us have a complete picture of who has data for each time period, and who does not. 
valid_uuids <- uncount(valid_uuids, 7) #copies each row 7x
valid_uuids <- valid_uuids %>% 
  group_by(entity_uuid) %>% 
  mutate(temprownum=row_number()) #creates a temporary variable called 'temprownum' that just captures which rownumber it is
valid_uuids$t <- case_when(
                      valid_uuids$temprownum==1 ~ 0,
                      valid_uuids$temprownum==2 ~ 3,
                      valid_uuids$temprownum==3 ~ 6,
                      valid_uuids$temprownum==4 ~ 9,
                      valid_uuids$temprownum==5 ~ 12,
                      valid_uuids$temprownum==6 ~ 15,
                      valid_uuids$temprownum==7 ~ 18
) #replaces the temprownum variable with the t variable values we'll ultimately want

# Join the datasets. If survey_responses has more than 1 response for a given entity_uuid-t combo, then ALL rows are kept. If there are no responses for a given entity_uuid-t combo, then the recordeddate is just NA.
responses_all_uuids <- full_join(survey_responses, valid_uuids, by = c("entity_uuid", "t"))

#Someone might have a survey response but not actually be in the study if there was a test or a typo. Delete anyone who took the survey who is not on the valid_uuids list; this is equivalent to saying: delete anyone who does not have a wave value associated with them. 
responses_all_uuids <- responses_all_uuids %>%
  filter(!is.na(wave))

#Arrange responses and reorder variables
responses_all_uuids <- responses_all_uuids %>%
  arrange(wave, entity_uuid, t) %>% # order of rows
  dplyr::select(entity_uuid, wave, t, everything()) # order of columns


# GET THE SENT DATES ----

#Force the date formats
sent_dates <- sent_dates %>%
  mutate(sent_date = as.Date(sent_date, format="%m/%d/%y"))

#Join with the list of sent_dates to identify when a given wavenum got its tnum survey. Sent date will be missing if we have not yet sent a survey (or Ania messed up and didn't put it into the gsheet).
responses_all_uuids_sentdates <- left_join(responses_all_uuids, sent_dates, by=c("wave", "t"))

#Calculate how long it took someone to complete a survey. Completion time will be missing if recorded_date is missing or if sent_date is missing. 
responses_all_uuids_sentdates <- responses_all_uuids_sentdates %>%
  mutate(completion_time = recorded_date - sent_date)


# CALCULATE WHO NEEDS REMINDERS, WHO NEEDS PAYMENTS ----

#Create variables identifying the number of days it's been since a survey was sent (time_since_sent) and how many days the survey window was open for. Typically it'll be open for 14 days, but for logistical reasons occasionally it'll be shorter (e.g. wave1 t0 had a 13 day window; wave5 t0 and wave2 t3 both had 12 day windows)
today() #Look at what "today" is; make sure is right
need_reminder_payment <- responses_all_uuids_sentdates %>%
  mutate(time_since_sent = as.numeric(today() - sent_date), #evaluates to NA if sent_date is missing (eg bc not yet sent)
         finished = ifelse(is.na(finished), 0, finished), #turn NA's into 0 again [needed bc of merge above]
         window_length = case_when(
                         wave==1 & t==0 ~ 13,
                         wave==2 & t==3 ~ 12, 
                         wave==3 & t==3 ~ 13,
                         wave==5 & t==0 ~ 12,
                         wave==6 & t==0 ~ 13,
                         TRUE ~ 14))

#Calculate who needs a reminder (if they haven't finished and the survey window is still open) and who needs payment (if they have finished in the survey window and now that survey window is closed)
need_reminder_payment <- need_reminder_payment %>%
  mutate(needs_reminder = as.numeric(case_when(
                          time_since_sent>=-1 & time_since_sent<=window_length & finished==0 ~ 1,
                          TRUE ~ 0)), #for all other cases: needs_reminder=0
        needs_payment = as.numeric(case_when(
                        completion_time>=-1 & completion_time<=window_length & time_since_sent>window_length & finished==1 ~ 1,
                        TRUE ~ 0))) #for all other cases: needs_payment=0

#Copy over the most conservative value of needreminder and needpayment, erring on the side of paying and not reminding. Take the SMALLEST value of 'needs_reminder' (NO<YES) and the LARGEST value of 'needs_payment' (NO<YES) and replace all rows for that entity_uuid-t combo with the corresponding value.
need_reminder_payment <- need_reminder_payment %>% 
  group_by(entity_uuid, t) %>% 
  mutate(needs_payment = max(needs_payment),
         needs_reminder = min(needs_reminder)) %>%
  ungroup() 

#Deduplicate so that we keep no more than 1 response per entity_uuid-t combo (this is to ensure that the survey admin team doesn't accidentally send >1 reminder email or >1 payment)
need_reminder_payment <- need_reminder_payment %>% 
  group_by(entity_uuid) %>%
  distinct(t, .keep_all=TRUE) %>%
  arrange(entity_uuid, t) %>%
  ungroup()

#Do some tests. Note that with the second one, the observations are all people who had sawlastpage=1, so the fact that they have a recorded date indicates that they must have actually finished.
stopifnot((is.na(need_reminder_payment$sent_date)==FALSE & need_reminder_payment$time_since_sent<=need_reminder_payment$window_length & need_reminder_payment$finished==0 & need_reminder_payment$needs_reminder==1) |
            (is.na(need_reminder_payment$sent_date)==FALSE & need_reminder_payment$time_since_sent<=need_reminder_payment$window_length & need_reminder_payment$finished==1 & need_reminder_payment$needs_reminder==0) | 
            (is.na(need_reminder_payment$sent_date)==FALSE & need_reminder_payment$time_since_sent>need_reminder_payment$window_length & need_reminder_payment$needs_reminder==0) |
            is.na(need_reminder_payment$sent_date)==TRUE)
stopifnot((is.na(need_reminder_payment$sent_date)==FALSE & need_reminder_payment$time_since_sent<=need_reminder_payment$window_length & is.na(need_reminder_payment$recorded_date)==TRUE & need_reminder_payment$needs_reminder==1) |
            (is.na(need_reminder_payment$sent_date)==FALSE & need_reminder_payment$time_since_sent<=need_reminder_payment$window_length & is.na(need_reminder_payment$recorded_date)==FALSE & need_reminder_payment$needs_reminder==0) | 
            (is.na(need_reminder_payment$sent_date)==FALSE & need_reminder_payment$time_since_sent>need_reminder_payment$window_length & need_reminder_payment$needs_reminder==0) |
            is.na(need_reminder_payment$sent_date)==TRUE)
stopifnot((is.na(need_reminder_payment$sent_date)==FALSE & need_reminder_payment$finished==1 & need_reminder_payment$completion_time>=-1 & need_reminder_payment$completion_time<=need_reminder_payment$window_length & need_reminder_payment$time_since_sent>need_reminder_payment$window_length & need_reminder_payment$needs_payment==1) |
            need_reminder_payment$needs_payment==0)
  

# ADD IN THE SURVEY LINK FOR ANYONE WHO NEEDS A REMINDER ----

#Remove wave number from the links df since it's not needed
links <- links %>%
  dplyr::select(-wave)

#Join the list of people who need reminders and payments with the list of links
need_reminderlink_payment <- left_join(need_reminder_payment, links, by=c("entity_uuid", "t"))

#For clarity, only keep the links in if the person needs a reminder
need_reminderlink_payment <- need_reminderlink_payment %>%
  mutate(unique_link=ifelse(needs_reminder==0, NA, unique_link))

#Do a test to make sure there is consistency in reminder and link columns for the wave and t at hand
stopifnot((need_reminderlink_payment$needs_reminder==0 & is.na(need_reminderlink_payment$unique_link)==TRUE & need_reminderlink_payment$wave==wavenum & need_reminderlink_payment$t==tnum) |
            (need_reminderlink_payment$needs_reminder==1 & is.na(need_reminderlink_payment$unique_link)==FALSE & need_reminderlink_payment$wave==wavenum & need_reminderlink_payment$t==tnum) |
            need_reminderlink_payment$wave!=wavenum |
            need_reminderlink_payment$t!=tnum)

  
# EXPORT THE DATA ----

#Turn numeric variables to more user-friendly "yes" and "no" values
need_reminderlink_payment <- need_reminderlink_payment %>% 
  mutate(finished=case_when(
           finished==0 ~ "No",
           finished==1 ~ "Yes"),
        needs_reminder=case_when(
          needs_reminder==0 ~ "No",
          needs_reminder==1 ~ "Yes"),
         needs_payment=case_when(
           needs_payment==0 ~ "No",
           needs_payment==1 ~ "Yes")) 

#Specify payment amount
need_reminderlink_payment <- need_reminderlink_payment %>% 
  mutate(payment_amount=case_when(
    needs_payment=="Yes" & t==0 ~ 60,
    needs_payment=="Yes" & t>0 & t<=6 ~ 40,
    needs_payment=="Yes" & t==9 ~ 45,
    needs_payment=="Yes" & t==12 ~ 50,
    needs_payment=="Yes" & t==15 ~ 55,
    needs_payment=="Yes" & t==18 & wavenum==1 ~ 160,
    needs_payment=="Yes" & t==18 & wavenum>1 ~ 150))

#Clean up and add in the prior forfeitures (people who did not complete the t0 survey) to make it easy for Support to see everyone's data in one place
forfeitures <- forfeitures %>%
  mutate(payment_amount=NA,
         sent_date=as.Date(sent_date),
         recorded_date=as.Date(recorded_date))

need_reminderlink_payment_forfeitures <- bind_rows(need_reminderlink_payment, forfeitures)


#Order the rows, select and order the columns, and export the whole file with today's date (yyyy-mm-dd)
final_output <- need_reminderlink_payment_forfeitures %>%
  arrange(wave, entity_uuid, t) %>% 
  dplyr::select(entity_uuid, wave, t, sent_date, finished, recorded_date, needs_reminder, unique_link, needs_payment, payment_amount) 
todaysdate <- today()
filenameall <- paste0("TIC_survey_reminders_payments_all_", todaysdate, ".csv")
write_csv(final_output, filenameall)

#Filter to keep only the desired wave and t (chosen in partA code), and export the filtered file with today's date (yyyy-mm-dd)
final_output_filtered <- final_output %>%
  filter(wave==wavenum & t==tnum) 
filenamefiltered <- paste0("TIC_survey_reminders_payments_wave", wavenum, "_t", tnum, "_", todaysdate, ".csv")
write_csv(final_output_filtered, filenamefiltered)

#Print some summary messages for easy data checking: 
print(paste0("For wave", wavenum, " and t", tnum, ":"))
print(paste0("Number of people getting the survey: ", n_distinct(final_output_filtered$entity_uuid)))
print(paste0("First survey completion: ", min(final_output_filtered$recorded_date, na.rm=TRUE))) 
print(paste0("Last survey completion: ", max(final_output_filtered$recorded_date, na.rm=TRUE)))
print(paste0("Number of people who do not need a reminder: ", sum(final_output_filtered$needs_reminder == "No")))
print(paste0("Number of people who need a reminder: ", sum(final_output_filtered$needs_reminder == "Yes")))
print(paste0("Percent of people who need a reminder: ", (round(100*sum(final_output_filtered$needs_reminder == "Yes")/n_distinct(final_output_filtered$entity_uuid))), "%"))
print(paste0("Number of people who do not need payment: ", sum(final_output_filtered$needs_payment == "No")))
print(paste0("Number of people who need payment: ", sum(final_output_filtered$needs_payment == "Yes")))
print(paste0("Percent of people who need payment: ", (round(100*sum(final_output_filtered$needs_payment == "Yes")/n_distinct(final_output_filtered$entity_uuid))), "%"))
print("You're done! Now just check your folder for the files.")

