#FII/UpTogether TIC: Pulling data from Qualtrics to identify who needs reminders and who needs to be paid for survey completion in the Trust & Invest Collaborative (TIC) study
#Ania Jaroszewicz (ajaroszewicz@hbs.edu) 
#Last updated: 5 May 2021


#Overview: This file should be run in conjunction with TIC_calc_survey_reminders_payments_[date]_partA.R (sent to the FII survey admin team). 
#PartA code (run before this file is called):
#(1) Pulls data from Qualtrics, the survey platform on which we are collecting data for TIC
#Part B code (this):
#(2) Processes that data to identify all the people who have not yet taken a survey that was recently sent out but "should have", and thus need a reminder. This is based on *when* you run the script, and will only say that a person needs a reminder if the relevant survey was sent out 14 days or less from the day that you run the script 
#(3) It also identifies all the people who have taken a survey, and thus need payment 
#(4) It outputs 2 CSV spreadsheets that show who needs a reminder and who needs a payment (and, if they need payment, how much). One of these shows all of the responses to date (with at most one entry per entity_uuid - t combination). The other one shows just the wave and t number that the user specified in partA.



# CLEAN RAW QUALTRICS DATA: DELETE INVALID RESPONSES, UNWANTED VARIABLES  ----
#Take the raw Qualtrics data that was run in Part A. Keep only observations that are not preview and test responses, those that are not missing an entity_uuid, and those that did not say that they got the link from someone other than FII. Later add to this all the test responses(entity_uuid=ania1234 or test, birthyear=1990, etc.)
survey_responses <- raw_survey_responses %>% 
  filter(Status!="Survey Preview" & entity_uuid!="NA" & LINKSOURCE!="From someone else")

#Select the variables of interest, put them in the desired order, and rename them
survey_responses <- survey_responses %>%
  select(entity_uuid,
         t,
         finished = sawlastpage,
         recorded_datetime = RecordedDate)

#Expand the definition of completion: also allow finished=1 if there is a recordeddate. Replace NA's with 0's.
survey_responses <- survey_responses %>%
  mutate(finished = case_when(
    !is.na(survey_responses$recorded_datetime) ~ 1,
    is.na(finished) ~ 0)) #later check this; right now there are no responses where there's a sawlastpage=1 but no recorded date; this will happen after like a month of the survey being open.

#Pull out just the date from the recorded datetime variable
survey_responses <- survey_responses %>% 
  mutate(recorded_date=as_date(recorded_datetime))

#Sort/arrange the variables
survey_responses <- survey_responses %>%
  arrange(entity_uuid, t, recorded_date)



# MERGE WITH LIST OF UUID'S ----

#Load uuids file from GitHub
  #Typically, you'll want to use the full list of active enrollees
  #But if the t survey that we want reminders/payments for is t0, those people won't yet be on the list of active enrollees. So, you need to use the list of people who are approved by not yet enrolled for that wave (sent to eval team from engineering team) and posted by Ania onto her github page. Note that the code should break if she hasn't posted the file.
  if(tnum==0) {
    approved_not_yet_enrolled_filename <- paste0("https://raw.githubusercontent.com/aniajaroszewicz/FII_TIC/main/wave", wavenum, "_approved_uuids.csv")
    full_uuids <- read.csv(approved_not_yet_enrolled_filename, header = TRUE, sep=",")
   } else {
     full_uuids <- read.csv("https://raw.githubusercontent.com/aniajaroszewicz/FII_TIC/main/actively_enrolled_entity_uuids_fake.csv", header = TRUE, sep=",") #This will later be updated with a complete list of all ACTIVE uuids and their wave # (ie, adding new waves as they come in and dropping people who have left the trial). When doing so, use the 'raw' form of the file
   } 

#Rename variable names to address fact that sometimes-- but not always-- they read in funny
newvarnames <- c("entity_uuid", "wave")
cbind(names(full_uuids), newvarnames) #compare old variable names with new names to make sure you're mapping it correctly
names(full_uuids) <- newvarnames

#For each uuid, create 7 rows, 1 for each t (0,3,6,9,12,15,18). This will later help us have a complete picture of who has data for each time period, and who does not. 
full_uuids <- uncount(full_uuids, 7) #copies each row 7x
full_uuids <- full_uuids %>% 
  group_by(entity_uuid) %>% 
  mutate(temprownum=row_number()) #creates a temporary variable called 'temprownum' that just captures which rownumber it is
full_uuids$t <- case_when(
                full_uuids$temprownum==1 ~ 0,
                full_uuids$temprownum==2 ~ 3,
                full_uuids$temprownum==3 ~ 6,
                full_uuids$temprownum==4 ~ 9,
                full_uuids$temprownum==5 ~ 12,
                full_uuids$temprownum==6 ~ 15,
                full_uuids$temprownum==7 ~ 18
) #replaces the temprownum variable with the t variable values we'll ultimately want

# Join the datasets. If survey_responses has more than 1 response for a given entity_uuid-t combo, then ALL rows are kept. If there are no responses for a given entity_uuid-t combo, then the recordeddate is just NA.
responses_all_uuids <- full_join(survey_responses, full_uuids, by = c("entity_uuid", "t"))

#We will later want to double check that the only people who are flagged as 'needs_reminder' or 'needs_payment' are actually on the formal full_uuids list. Someone might have a survey response but not actually be in the study if there was a test or a typo. Delete anyone who took the survey who is not on the full_uuid list; this is equivalent to saying: delete anyone who does not have a wave value associated with them. 
responses_all_uuids <- responses_all_uuids %>%
  filter(!is.na(wave))

#Arrange responses and reorder variables
responses_all_uuids <- responses_all_uuids %>%
  arrange(wave, entity_uuid, t) %>% # order of rows
  select(entity_uuid, wave, t, everything()) # order of columns

#Add in the date the survey was sent (for the given wave-t) (based on Ania's records) and calculate how long it took someone to complete the survey (if it was sent and they completed it at all). Sent date will be missing if we have not yet sent a survey. Completion time will be missing if recorded_date is missing. Ania will add lines to this each time a wave gets a new t survey (and replace these fake values with real ones).
responses_all_uuids <- responses_all_uuids %>%
  mutate(sent_date = case_when(
                      wave == 99 & t == 0 ~ ymd(20210404),
                      wave == 99 & t == 3 ~ ymd(20210410),
                      wave == 2 & t == 0 ~ ymd(20210325),
                      wave == 2 & t == 3 ~ ymd(20210326),
                      wave == 2 & t == 6 ~ ymd(20210327),
                      wave == 2 & t == 9 ~ ymd(20210328),
                      wave == 4 & t == 15 ~ ymd(20210402)),
         completion_time = recorded_date - sent_date)


# CALCULATE WHO NEEDS REMINDERS, WHO NEEDS PAYMENTS ----

#Create a flag for if someone needs a reminder. If they didn't finish (meaning: there is no recorded date and they did not get to the last page), and today is <=14 days from the surveysentdate, then needs_reminder should be =Yes/1. Otherwise, it's =No/0.
today() #Look at what "today" is; make sure is right
responses_all_uuids <- responses_all_uuids %>%
  mutate(time_since_sent = as.numeric(today() - sent_date), #evaluates to 0 if sent_date is missing (eg bc not yet sent)
         finished = ifelse(is.na(finished), 0, finished), #turn NA's into 0 again [needed bc of merge above]
         needs_reminder = as.numeric(ifelse(between(time_since_sent, 0, 14) == TRUE & finished == 0, 1, 0))) %>%
  mutate(needs_reminder = ifelse(is.na(needs_reminder) == TRUE, 0, needs_reminder)) #turn any NA's into 0 for future min/max functions

#Create a flag for if someone needs payment. If the date on which the survey was completed was <= 14 days since it was sent, and if today is 15+ days since the wave-t email has been sent, needpayment=Yes/1. Otherwise, it's =No/0.  
responses_all_uuids <- responses_all_uuids %>%
  mutate(needs_payment = as.numeric(ifelse(completion_time>-1 & completion_time<=14 & finished==1 & time_since_sent>=15, 1, 0))) %>%
  mutate(needs_payment = ifelse(is.na(needs_payment) == TRUE, 0, needs_payment)) #turn any NA's into 0 for future min/max functions

#Copy over the most conservative value of needreminder and needpayment, erring on the side of paying and not reminding. Take the SMALLEST value of 'needs_reminder' (NO<YES) and the LARGEST value of 'needs_payment' (NO<YES) and replace all rows for that entity_uuid-t combo with the corresponding value.
responses_all_uuids <- responses_all_uuids %>% 
  group_by(entity_uuid, t) %>% 
  mutate(needs_payment = max(needs_payment),
         needs_reminder = min(needs_reminder)) 
  
#Arrange responses
responses_all_uuids <- responses_all_uuids %>%
  arrange(wave, t, entity_uuid) # order of rows

#Deduplicate so that we keep no more than 1 response per entity_uuid-t combo (this is to ensure that the survey admin team doesn't accidentally send >1 reminder email or >1 payment)
responses_all_uuids <- responses_all_uuids %>% 
  group_by(entity_uuid) %>%
  distinct(t, .keep_all=TRUE) %>%
  arrange(entity_uuid, t) 


# EXPORT THE DATA ----

#Turn numeric needs_reminder and needs_payment variables to be more user-friendly "yes" and "no" values
responses_all_uuids <- responses_all_uuids %>% 
  mutate(needs_reminder=case_when(
          needs_reminder==0 ~ "No",
          needs_reminder==1 ~ "Yes"),
         needs_payment=case_when(
           needs_payment==0 ~ "No",
           needs_payment==1 ~ "Yes")) 

#Specify payment amount
responses_all_uuids <- responses_all_uuids %>% 
  mutate(payment_amount=case_when(
    needs_payment=="Yes" & t==0 ~ "$60",
    needs_payment=="Yes" & t>0 & t<18 ~ "$40",
    needs_payment=="Yes" & t==18 ~ "$100"))

#Order the rows, select and order the columns, and export the whole file with today's date (yyyy-mm-dd)
final_output <- responses_all_uuids %>%
  arrange(wave, t, entity_uuid) %>% # order of rows
  select(entity_uuid, wave, t, sent_date, finished, recorded_date, needs_reminder, needs_payment, payment_amount) # order of columns
todaysdate <- today()
filenameall <- paste0("TIC_survey_reminders_payments_all_", todaysdate, ".csv")
write_csv(final_output, filenameall)

#Filter to keep only the desired wave and t (chosen in partA code), and export the filtered file with today's date (yyyy-mm-dd)
final_output_filtered <- final_output %>%
  filter(wave==wavenum & t==tnum) 
filenamefiltered <- paste0("TIC_survey_reminders_payments_wave", wavenum, "_t", tnum, "_", todaysdate, ".csv")
write_csv(final_output_filtered, filenamefiltered)


#THINGS ANIA WILL DO AFTER LAUNCH ----
# -Delete all test responses (drop everything that happened before a certain date, all the fake uuid's, all the fp=1 values)
# -Delete the fake 'sent date' values and replace with real ones
# -Replace the fake_uuid spreadsheet with the real list. Put this on github and call it directly from the URL
