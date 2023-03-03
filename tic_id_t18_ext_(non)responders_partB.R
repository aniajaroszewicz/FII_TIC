#TIC - Creating a list of people who have not yet responded to their (extended-deadline) t18 survey
#Ania Jaroszewicz
#Last updated: 3 March 2023

#Purpose: We are inviting TIC participants who have not responded for the last year to complete a shortened (15 min) t18 survey. These folks are invited to do so during the regular survey window, but if they don't complete it then, they get an extended window of another 5ish weeks. This file identifies (1) the people who successfully completed the short t18 survey outside the standard window (and thus needs to be paid on a special schedule); and (2) the people who have not [yet] completed the survey (across all the waves) and thus need to be prioritized in the data acquisition efforts (calling them, SMS/email, sending postcards). The final output is a spreadsheet of all participants across all waves who were invited to do the short t18 survey in the extended time period
#The code runs as a construction of 2 files. This is Part B, and is called in the Part A code.

#This file uses 6 datasets called in part A: 
#1: the calling_priorities.csv spreadsheet, which was created in tic_calc_pastsurveyscompleted_[date].R. The calling_priorities.csv spreadsheet changes each time a survey window closes. It needs to be updated after each survey window closes but won't change during the time that no survey is active. [this is the complete list of all participants with a column for the last survey they completed on time]
#2: the list of people who were ever randomized [so we can get wave number]
#3: the Qualtrics survey data from the short (15 min) t18 survey [so we can identify who needs to be paid and who does NOT need to be bugged again]
#4: the list of links for the survey [so we know what link to send participants who need to be nudged]
#5: the list of dates on which regular t18 survey was sent [so we know if it's too early to send a reminder for some people]
#6: the list of people who have already been paid for completing their t18 survey (regular length or short) during the regular window [so we don't double pay people]


####1ST PASS AT IDENTIFYING WHO DID THE SHORT T18 SURVEY & WHO STILL NEEDS TO BE NUDGED ----

#Slim down calling priorities list. Only keep people who are eligible to get the short t18 survey (this is by definition the same group of people who are being invited to complete the survey, or a subset of them): people who have not completed a survey since t6 or earlier. 
callingpriorities2 <- callingpriorities %>%
  filter(lastcompletedsurvey<=6) %>%
  dplyr::select(-wave)

#Slim down the survey responses to make it easier to work with
survey_responses_short <- raw_survey_responses_short %>%
  dplyr::select(entity_uuid, 
                t,  
                finished = sawlastpage,
                recorded_datetime = RecordedDate)

#Join the calling priorities and short survey completion lists to identify people who have completed the short survey recently
callingpriorities2_survey <- full_join(callingpriorities2, survey_responses_short, by="entity_uuid")

#Merge the callingpriorities+survey list with the list of randomized participants so we can get wave number for everyone
callingpriorities2_survey2 <- left_join(callingpriorities2_survey, all_randomized_to_date, by="entity_uuid")

#Remove people who were never randomized (Ania's test responses) and slim down
callingpriorities2_survey2 <- callingpriorities2_survey2 %>%
  filter(!is.na(treatment)) %>%
  dplyr::select(-treatment, -app_id, -peergroupid)

#Clean up. Make initial judgments about whether people need reminders or payments (this will be further cleaned up below)
callingpriorities2_survey2 <- callingpriorities2_survey2 %>%
  mutate(finished=ifelse(is.na(finished), 0, finished),
         t="t18 extension",
         recorded_date=as_date(recorded_datetime),
         needs_reminder=if_else(finished==0, "Yes", "No"),
         needs_payment=if_else(finished==1, "Yes", "No"))  


####READJUST THE COMPLETED/NEED NUDGE LISTS BASED ON THE DEADLINES AND DATES ----
#Different waves have different t18 extension deadlines. Add those deadlines in. 
callingpriorities2_survey3 <- callingpriorities2_survey2 %>%
  mutate(deadline=case_when(
        wave==1 | wave==2 ~ as_date("2023-03-14"),
        wave==3 | wave==4 ~ as_date("2023-05-01"),
        wave==5 | wave==6 ~ as_date("2023-07-01")))
        
#Remove people from payment list if they completed the survey after their wave's deadline
table(callingpriorities2_survey3$needs_payment)
callingpriorities2_survey3 <- callingpriorities2_survey3 %>%
  mutate(needs_payment=if_else(recorded_date>deadline & finished==1, "No", needs_payment))
table(callingpriorities2_survey3$needs_payment) #The yeses should be <= to previous tabulation

#Remove people from reminders list if the deadline has passed 
table(callingpriorities2_survey3$needs_reminder)
callingpriorities2_survey3 <- callingpriorities2_survey3 %>%
  mutate(needs_reminder=if_else(today()>deadline, "No", needs_reminder))
table(callingpriorities2_survey3$needs_reminder) #The yeses should be <= to previous tabulation

#Slim down the sent_dates file so we know whose regular (NOT short!) t18 survey is open. This will help us determine who needs a reminder, below. Only keep the t18 lines
sent_dates2 <- sent_dates %>%
  filter(t==18) %>%
  mutate(regt18_sent_date=sent_date) %>%
  dplyr::select(-t, -sent_date)

#Merge in the sent_dates list. Note that these are for the REGULAR t18 surveys, not the extended-window surveys being sent out. We only want these so that we know whether it's too early for some people to be reminded.
callingpriorities2_survey3_sentdate <- left_join(callingpriorities2_survey3, sent_dates2, by='wave')

#Remove people from the reminders list if it's too early for them to be reminded because they haven't even received their regular-window t18 survey yet
table(callingpriorities2_survey3_sentdate$needs_reminder)
callingpriorities2_survey3_sentdate <- callingpriorities2_survey3_sentdate %>%
  mutate(needs_reminder=if_else(is.na(regt18_sent_date), "No", needs_reminder))
table(callingpriorities2_survey3_sentdate$needs_reminder) #The yeses should be <= to previous tabulation
         
         
####READJUST THE PAYMENT LIST BASED ON WHETHER THEY WERE ALREADY PAID ----

#Identify people who completed the short survey but are NOT on the payment list from the regular window. (An alternative way to measure this would be to see who completed the survey after the regular survey window, but this is more prone to error.)

#Look at list of people who have already been paid
table(already_paid_t18_reg_window$wave) #make sure that there are enough waves here; if it's after 2/28 you should have 3 waves; if it's after 3/30 you should have 4 waves; if it's after 4/30 you should have 5 waves; if it's after 5/30 you should have 6 waves. If you don't, fix it in TIC_calc_survey_reminders_payments_partAC.R

#Clean up the already paid list
already_paid_t18_reg_window2 <- already_paid_t18_reg_window %>%
  mutate(entity_uuid=recipientEntityUuid) %>%
  dplyr::select(-wave, -recipientEntityUuid)

#Merge the working df with the list of people who have already been paid
callingpriorities2_survey3_sentdate_xpaid <- left_join(callingpriorities2_survey3_sentdate, already_paid_t18_reg_window2, by='entity_uuid')

#Remove anyone from the main df if they've already been paid; 
callingpriorities2_survey3_sentdate_xpaid <- callingpriorities2_survey3_sentdate_xpaid %>%
  filter(is.na(paid_t18_reg_window)) 


####READJUST THE PAYMENT LIST BASED ON WHETHER THEY WILL BE PAID DURING THE REGULAR WINDOW----

#Create a variable corresponding to when the survey is open during the regular window
callingpriorities2_survey3_sentdate_xpaid <- callingpriorities2_survey3_sentdate_xpaid %>% 
  mutate(regopenwindow=as_date(days(14)+regt18_sent_date))

#If the regular survey window has not yet closed, then the person doesn't need to be marked as 'needs payment' here. They will instead be paid on the regular cycle
callingpriorities2_survey3_sentdate_xpaid <- callingpriorities2_survey3_sentdate_xpaid %>% 
  mutate(needs_payment=if_else(today()<=regopenwindow & needs_payment=="Yes", "No", needs_payment))

#Add payment amount for anyone who needs payment
callingpriorities2_survey3_sentdate_xpaid <- callingpriorities2_survey3_sentdate_xpaid %>% 
  mutate(payment_amount=if_else(needs_payment=="Yes", "150", "NA"))


####ADD LINKS FOR PEOPLE WHO STILL NEED NUDGE ----

#Clean up links list
links2 <- links %>%
  filter(t==18) %>%
  dplyr::select(-wave, -t)

#Add survey links to list of people who need to be nudged to complete their survey 
need_reminder_payment <- left_join(callingpriorities2_survey3_sentdate_xpaid, links2, by="entity_uuid")
  
#Remove survey links for anyone who does not need a reminder (to avoid confusion)
need_reminder_payment <- need_reminder_payment %>%
  mutate(unique_link=if_else(needs_reminder=="No", "NA", unique_link))

#Remove recorded dates for anyone who does not need payment (to avoid confusion) (these are people who started the survey but didn't finish it, and eventually Qualtrics closed their survey and time stamped a recorded date)
need_reminder_payment <- need_reminder_payment %>%
  mutate(recorded_date = if_else(needs_payment == "No", as_date(NA), recorded_date))


####DO SOME TESTS ----

#Confirm that everyone either needs a reminder (but no payment), needs a payment (but no reminder), or it's too early for them, or their deadline has passed and they didn't do it
stopifnot((need_reminder_payment$needs_reminder=="Yes" & need_reminder_payment$needs_payment=="No") | (need_reminder_payment$needs_reminder=="No" & need_reminder_payment$needs_payment=="Yes") | is.na(need_reminder_payment$regt18_sent_date) | today()<need_reminder_payment$regopenwindow | (need_reminder_payment$needs_reminder=="No" & need_reminder_payment$needs_payment=="No" & today()>need_reminder_payment$deadline))

#Confirm that everyone who needs a reminder has a link, but no one else does
stopifnot((need_reminder_payment$needs_reminder=="Yes" & !is.na(need_reminder_payment$unique_link)) | (need_reminder_payment$needs_reminder=="No" & need_reminder_payment$unique_link=="NA"))

#Confirm that everyone who needs payment is getting one, but no one else is
stopifnot((need_reminder_payment$needs_payment=="Yes" & need_reminder_payment$payment_amount=="150") | (need_reminder_payment$needs_payment=="No" &(need_reminder_payment$payment_amount=="NA")))

#Confirm that for anyone who needs a reminder, there's a link for the short survey version 
table(need_reminder_payment$needs_reminder)
need_reminder_payment %>%
  mutate(test=ifelse(str_detect(unique_link, "031P0S9i44y4y0K"), 1, 0)) %>%
  group_by(test) %>%
  summarise(count=n()) 


####CLEAN UP & OUTPUT THE LIST OF COMPLETIONS & PEOPLE WHO NEED TO BE NUDGED TO COMPLETE THEIR SHORT T18 SURVEY ---- 

#Deduplicate so that we keep no more than 1 row per entity_uuid (this is to ensure that the survey admin team doesn't accidentally send >1 reminder email or >1 payment)
need_reminder_payment <- need_reminder_payment %>% 
  distinct(entity_uuid, .keep_all=TRUE)

#Clean up
need_reminder_payment <- need_reminder_payment %>%
  dplyr::select(entity_uuid, wave, t, finished, recorded_date, needs_reminder, unique_link, needs_payment, payment_amount) %>%
  arrange(wave, entity_uuid) %>%
  mutate(finished=case_when(
    finished==0 ~ "No",
    finished==1 ~ "Yes"))


#Print some summary messages for easy data checking: 
print(paste0("Number of wave1 people who need a reminder: ", sum(need_reminder_payment$wave == 1 & need_reminder_payment$needs_reminder=="Yes")))
print(paste0("Number of wave1 people who need payment: ", sum(need_reminder_payment$wave == 1 & need_reminder_payment$needs_payment=="Yes")))
print(paste0("Number of wave2 people who need a reminder: ", sum(need_reminder_payment$wave == 2 & need_reminder_payment$needs_reminder=="Yes")))
print(paste0("Number of wave2 people who need payment: ", sum(need_reminder_payment$wave == 2 & need_reminder_payment$needs_payment=="Yes")))
print(paste0("Number of wave3 people who need a reminder: ", sum(need_reminder_payment$wave == 3 & need_reminder_payment$needs_reminder=="Yes")))
print(paste0("Number of wave3 people who need payment: ", sum(need_reminder_payment$wave == 3 & need_reminder_payment$needs_payment=="Yes")))
print(paste0("Number of wave4 people who need a reminder: ", sum(need_reminder_payment$wave == 4 & need_reminder_payment$needs_reminder=="Yes")))
print(paste0("Number of wave4 people who need payment: ", sum(need_reminder_payment$wave == 4 & need_reminder_payment$needs_payment=="Yes")))
print(paste0("Number of wave5 people who need a reminder: ", sum(need_reminder_payment$wave == 5 & need_reminder_payment$needs_reminder=="Yes")))
print(paste0("Number of wave5 people who need payment: ", sum(need_reminder_payment$wave == 5 & need_reminder_payment$needs_payment=="Yes")))
print(paste0("Number of wave6 people who need a reminder: ", sum(need_reminder_payment$wave == 6 & need_reminder_payment$needs_reminder=="Yes")))
print(paste0("Number of wave6 people who need payment: ", sum(need_reminder_payment$wave == 6 & need_reminder_payment$needs_payment=="Yes")))
print("You're done! Now just check your folder for the file.")

todaysdate <- today()
filename <- paste0("TIC_extendedt18survey_reminders_payments_", todaysdate, ".csv")
write_csv(need_reminder_payment, filename)
