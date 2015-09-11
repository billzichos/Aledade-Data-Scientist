wd <- "~/GitHub/Aledade-Data-Scientist"

setwd(wd)

cmsUrl <- "http://www.cms.gov/apps/ama/license.asp?file=http://download.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/"
zipFileName <- "Medicare_Provider_Util_Payment_PUF_CY2013.zip"
puf2012FileName <- "Medicare_Provider_Util_Payment_PUF_CY2012.txt"
puf2013FileName <- "Medicare_Provider_Util_Payment_PUF_CY2013.txt"

# download the cms package
#download.file(paste(cmsUrl,zipFileName,sep=""),
#                paste(getwd(),"\\",zipFileName,sep=""),
#                quiet = FALSE)

# unzip the package
#unzip(paste(getwd(),"\\",zipFileName,sep=""), exdir = paste(getwd(),"\\data",sep=""))
        
# delete the zip file
#unlink(paste(getwd(), "/", zipFileName,sep = ""))

# column names
colN <- (c("npi", "nppes_provider_last_org_name", "nppes_provider_first_name",
           "nppes_provider_mi", "nppes_credentials", "nppes_provider_gender",
           "nppes_entity_code", "nppes_provider_street1", "nppes_provider_street2",
           "nppes_provider_city", "nppes_provider_zip", "nppes_provider_state",
           "nppes_provider_country", "provider_type", "medicare_participation_indicator",
           "place_of_service", "hcpcs_code", "hcpcs_description", "hcpcs_drug_indicator",
           "line_srvc_cnt", "bene_unique_cnt", "bene_day_srvc_cnt", "average_Medicare_allowed_amt",
           "stdev_Medicare_allowed_amt", "average_submitted_chrg_amt", "stdev_submitted_chrg_amt",
           "average_Medicare_payment_amt", "stdev_Medicare_payment_amt"))

# column formats
#colC <- (c("int","factor","factor","factor","factor","factor","factor","character","character","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","integer","integer","integer","numeric","numeric","numeric","numeric","numeric","numeric"))

# 2012 load

        ## data import
        puf2012 <- read.csv(
                paste(getwd(),"/data//",puf2012FileName, sep = ""), skip = 2, 
                header = FALSE, sep = "\t", col.names = colN)
        
        # column formatting
        puf2012$npi <- as.character(puf2012$npi)
        puf2012$nppes_provider_last_org_name <- as.character(puf2012$nppes_provider_last_org_name)
        puf2012$nppes_provider_first_name <- as.character(puf2012$nppes_provider_first_name)
        puf2012$nppes_provider_street1 <- as.character(puf2012$nppes_provider_street1)
        puf2012$nppes_provider_street2 <- as.character(puf2012$nppes_provider_street2)
        
        # filter just for Delaware
        puf2012 <- puf2012[puf2012$nppes_provider_state=="DE",]
        puf2012$Year <- as.factor("2012")
        
# 2013 load
        
        ## data import
        puf2013 <- read.csv(
                paste(getwd(),"/data//",puf2013FileName, sep = ""), skip = 2, 
                header = FALSE, sep = "\t", col.names = colN)
        
        # column formatting
        puf2013$npi <- as.character(puf2013$npi)
        puf2013$nppes_provider_last_org_name <- as.character(puf2013$nppes_provider_last_org_name)
        puf2013$nppes_provider_first_name <- as.character(puf2013$nppes_provider_first_name)
        puf2013$nppes_provider_street1 <- as.character(puf2013$nppes_provider_street1)
        puf2013$nppes_provider_street2 <- as.character(puf2013$nppes_provider_street2)
        
        # filter just for Delaware
        puf2013 <- puf2013[puf2013$nppes_provider_state=="DE",]
        puf2013$Year <- as.factor("2013")
        
# combine the files
delaware <- rbind(puf2012, puf2013)

# do some cleanup
# DO physicians take a more holistic approach and typically practice in rural areas.
#   ref: https://ppa.byu.edu/sites/ppa.byu.edu/files/New%20PPA%20Website%20Content/Prehealth/Handouts/Premedical%20Handouts/MD%20vs.%20DO.pdf
delaware[delaware$nppes_credentials=="D.O"|
                 delaware$nppes_credentials=="D.O." |
                 delaware$nppes_credentials=="DO",5] <- "DO"

delaware[delaware$nppes_credentials=="M.D"|
                 delaware$nppes_credentials=="M.D." |
                 delaware$nppes_credentials=="MD",5] <- "MD"

# note the FACC is a special designation in the field of cardiology
delaware[delaware$nppes_credentials=="M.D,F.A.C.C"|
                 delaware$nppes_credentials=="M.D, F.A.C.C"|
                 delaware$nppes_credentials=="MD, FACC",5] <- "MD, FACC"

delaware$isFACCFlag <- FALSE
delaware$isFACCFlag[delaware$nppes_credentials=="MD, FACC"] <- TRUE

delaware[delaware$nppes_credentials=="MD, FACC",5] <- "MD"

unique(delaware$nppes_credentials)

delaware$MedicarePayment <- delaware$bene_day_srvc_cnt *
        delaware$average_Medicare_payment_amt

# delaware cardiology
delaware <- delaware[delaware$provider_type=="Cardiology",]

summary(delaware)
# all providers are in US

# derive some attrubutes
delaware$TotalsByCert <- 0
delaware[delaware$nppes_credentials=="MD",32] <- 46977402
delaware[delaware$nppes_credentials=="DO",32] <- 8129798

# cardio analysis
delaware$SimpleHcpcsCode <- substr(delaware$hcpcs_code,1,2)

# are there codes in particular we should focus on?
ggplot(delaware, aes(SimpleHcpcsCode, MedicarePayment)) + geom_bar(stat="identity")

# subset based on simple hcpcs code
delaware <- delaware[delaware$SimpleHcpcsCode=="78" |
                             delaware$SimpleHcpcsCode=="93" |
                             delaware$SimpleHcpcsCode=="99",]

# lets add back some detail for the subset
delaware$SimpleHcpcsCode <- substr(delaware$hcpcs_code,1,3)

# are there codes in particular we should focus on?
ggplot(delaware, aes(SimpleHcpcsCode, MedicarePayment)) + geom_bar(stat="identity")

# it looks like a bulk of the cardio work is focused on HCPCS Code that begin with 992.
unique(delaware[delaware$SimpleHcpcsCode=="992",c(17:18)])

bzstat <- summarize(group_by(delaware, hcpcs_code), TotPay = sum(MedicarePayment))
bzstat <- bzstat[order(desc(bzstat$TotPay)),]
bzstat <- bzstat[[1]][1:15]

delaware <- delaware[delaware$hcpcs_code %in% bzstat,]

# is this readable yet?
ggplot(delaware, aes(hcpcs_code, MedicarePayment)) + geom_bar(stat="identity")

# would like to see how this data breaks out across different facets
ggplot(delaware, aes(hcpcs_code, MedicarePayment)) + geom_bar(stat="identity") + facet_grid(nppes_credentials ~ .)

# let's scale this a little better.
ggplot(delaware, aes(hcpcs_code, MedicarePayment / TotalsByCert)) + geom_bar(stat="identity") + facet_grid(nppes_credentials ~ .)

# there is clearly a service that DOs are prefering.
# 99214
unique(delaware[delaware$hcpcs_code=="99214",c(17:18)])

# let's try a similar analysis for Place of Service
# are similar service geared more more an office than facility or vice versa?
ggplot(delaware, aes(hcpcs_code)) + geom_bar(stat="bin") + facet_grid(place_of_service ~ .)


# FACC analysis is difficult because of scale so i might go back and add some fetures.
ggplot(delaware, aes(hcpcs_code)) + geom_bar(stat="bin") + facet_grid(isFACCFlag ~ .)
# however, my hunch is that there are situations where FACC certified is required
delaware[
        which((delaware[delaware$isFACCFlag==TRUE,17] %in% delaware[delaware$isFACCFlag==FALSE,17]))
         ,17]


cardioProvider <- as.data.frame(unique(delaware$nppes_provider_last_org_name))
cardioAnalysis <- data.frame(row.names = unique(delaware[,17]))
cbind(cardioProvider, cardioAnalysis)

with(delaware, plot(substr(delaware$nppes_provider_zip,1,5), MedicarePayment))

delDO <- delaware[delaware$nppes_credentials=="DO",]
delMD <- delaware[delaware$nppes_credentials=="MD",]
# functions for building an analysis file
        
        # count (sum) the number distinct services
        fTotalCount <- function(service) {
                sum(delaware[delaware$hcpcs_code==service,22])
        }
        
        fMDCount <- function(service) {
                sum(delaware[delaware$hcpcs_code==service &
                        delaware$nppes_credentials=="MD",22])
        }
        
        fDOCount <- function(service) {
                sum(delaware[delaware$hcpcs_code==service &
                                     delaware$nppes_credentials=="DO",22])
        }

serviceAnalysis <- as.data.frame(unique(delaware[,17:18]))
serviceAnalysis$hcpcs_code <- as.character(serviceAnalysis$hcpcs_code)
serviceAnalysis$TotalCount <- 0
serviceAnalysis$TotalCount <- sapply(serviceAnalysis$hcpcs_code, fTotalCount)
serviceAnalysis$MDCount <- 0
serviceAnalysis$MDCount <- sapply(serviceAnalysis$hcpcs_code, fMDCount)
serviceAnalysis$DOCount <- 0
serviceAnalysis$DOCount <- sapply(serviceAnalysis$hcpcs_code, fDOCount)
serviceAnalysis$FACCCount <- 0

str(serviceAnalysis)
summary(serviceAnalysis)
# derive some attributes
delaware$diffAllowPaid <- delaware$average_Medicare_payment_amt - delaware$average_Medicare_allowed_amt
delaware$diffSubmitPaid <- delaware$average_Medicare_payment_amt - delaware$average_submitted_chrg_amt

#str(delaware)
#summary(delaware)





# derive some more attributes
delaware$AllowedNumSD <- abs(delaware$average_Medicare_allowed_amt - 114.3520) / 4.94
delaware$SubNumSD <- abs(delaware$average_submitted_chrg_amt - 298.75) / 20.550
delaware$PaidNumSD <- abs(delaware$average_Medicare_payment_amt - 88.350) / 11.6639

hist(delaware$average_submitted_chrg_amt)
hist(delaware$average_Medicare_allowed_amt)
hist(delaware$average_Medicare_payment_amt)

hist(delaware$stdev_submitted_chrg_amt)
hist(delaware$stdev_Medicare_allowed_amt)
hist(delaware$stdev_Medicare_payment_amt)

hist(delaware$diffAllowPaid)
hist(delaware$diffSubmitPaid)
library("dplyr")
library("ggplot2")

summarise(group_by(delaware, hcpcs_code), sum(delaware$average_submitted_chrg_amt))

table(delaware$provider_type ~ delaware$average_submitted_chrg_amt)

write.csv(delaware, "delaware.csv")

g <- ggplot(delaware, aes(provider_type, diffSubmitPaid))

g + geom_point()
