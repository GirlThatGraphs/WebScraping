## Monthly
suppressMessages(source("FFT_AE.R"))
suppressMessages(source("FFT_Community.R"))
suppressMessages(source("FFT_Inpatient.R"))
suppressMessages(source("FFT_Maternity.R"))
suppressMessages(source("FFT_MentalHealth.R"))
suppressMessages(source("Workforce_Turnover.R"))
suppressMessages(source("Workforce_Staff.R"))
suppressMessages(source("LDHC.R"))
suppressMessages(source("GPAD.R")) ## takes a REALLLLLLLY long time 
suppressMessages(source("CommWaitList.R"))
suppressMessages(source("MHWaitList.R"))
suppressMessages(source("MHSDS.R"))
suppressMessages(source("MSA.R"))
suppressMessages(source("CSS.R")) ## takes a while


## Quarterly
suppressMessages(source("Workforce.R"))
suppressMessages(source("Cervical.R"))
suppressMessages(source("Dementia.R"))
suppressMessages(source("COVERGP.R"))
suppressMessages(source("COVERLA.R"))
suppressMessages(source("NDA.R"))

## Annual
suppressMessages(source("Complains.R"))
suppressMessages(source("ChildMortality.R"))
suppressMessages(source("SMIPremDeaths.R"))
suppressMessages(source("ASCOutcomes.R"))
suppressMessages(source("QOF.R")) ## takes a long time to load
suppressMessages(source("SEND.R"))



Sys.sleep(3)
beep(4)
