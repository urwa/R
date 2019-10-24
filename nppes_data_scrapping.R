## install.packages("pacman")
pacman::p_load("tidyverse", "httr", "jsonlite", "readr")
options(stringsAsFactors = FALSE)

GOBA_names <- read.csv("Physicians_total_left_join_27.csv")
path <- "https://npiregistry.cms.hhs.gov/api/?version=2.1"

df <- data.frame(
  result_count = integer(),
  results.addresses = list(),
  results.taxonomies = list(),
  results.identifiers = list(),
  results.basic.name_prefix = character(),
  results.basic.name = character(),
  results.basic.first_name = character(),
  results.basic.last_name = character(),
  results.basic.middle_name = character(),
  results.basic.credential = character(),
  results.basic.sole_proprietor = character(),
  results.basic.gender = character(),
  results.basic.enumeration_date = character(),
  results.basic.last_updated = character(),
  results.basic.status = character()
)

for (i in 1:nrow(GOBA_names)) {
  fname <- GOBA_names$firstname[i]
  lname <- GOBA_names$Last_name[i]
  
  request <-
    GET(url = path,
        query = list(first_name = fname, last_name = lname))
  
  if (request$status_code == 200) {
    
    response <- content(request, as = "text", encoding = "UTF-8")
    response_flat <- fromJSON(response, flatten = TRUE)
    
    if (is.null(response_flat$Errors)) {
      
      if (response_flat$result_count > 0) {
        
        df_temp <- data.frame(response_flat)
        
        df <- bind_rows(df, df_temp)
        message(c("i: ", i))
      } else {
        message(c("Empty response - ", i))
      }
    } else {
      message(response_flat$Errors)
    }
  } else {
    message(c("GET req unsuccesful - Error Code: ", request$status_code))
  }
}

df_all <- df

df_unnested_all <- df_all %>% 
  unnest(results.addresses, .drop = FALSE) %>%
  unnest(results.taxonomies, .drop = TRUE)

names(df_unnested_all)
names(df_unnested_all) <- c("result_count", "name_prefix", "name", "first_name", "last_name", "middle_name", "credential", "sole_proprietor", "gender", "enumeration_date", "last_updated", "status", "enumeration_type", "number", "last_updated_epoch", "created_epoch", "reactivation_date", "deactivation_date", "name_suffix", "country_code", "country_name", "address_purpose", "address_type", "address_1", "address_2", "city", "state", "postal_code", "telephone_number", "fax_number", "code", "desc", "primary", "state1", "license", "taxonomy_group")
write.csv(df_unnested_all,"nppes_all.csv")


pacman::p_load("tidyverse", "httr", "jsonlite", "readr","data.table")
options(stringsAsFactors=FALSE)

remove(list = ls())

df <- fread("nppes_all.csv")
names(df)
df <- df[,-c(1)]
DT <- df

names(DT)
str(DT)

DT$enumeration_date <- as.Date(ifelse(grepl("-", DT$enumeration_date), 
                                      as.Date(DT$enumeration_date, format = c("%Y-%m-%d")), 
                                      as.Date(DT$enumeration_date, format = c("%m/%d/%Y"))), 
                               origin = "1970-01-01")
DT$last_updated <- as.Date(ifelse(grepl("-", DT$last_updated), 
                                  as.Date(DT$last_updated, format = c("%Y-%m-%d")), 
                                  as.Date(DT$last_updated, format = c("%m/%d/%Y"))), 
                           origin = "1970-01-01")
DT$reactivation_date  <- as.Date(ifelse(grepl("-", DT$reactivation_date ), 
                                        as.Date(DT$reactivation_date , format = c("%Y-%m-%d")), 
                                        as.Date(DT$reactivation_date , format = c("%m/%d/%Y"))), 
                                 origin = "1970-01-01")
DT$deactivation_date  <- as.Date(ifelse(grepl("-", DT$deactivation_date ), 
                                        as.Date(DT$deactivation_date , format = c("%Y-%m-%d")), 
                                        as.Date(DT$deactivation_date , format = c("%m/%d/%Y"))), 
                                 origin = "1970-01-01")
unique(DT$postal_code)
DT$postal_code <- as.integer(DT$postal_code)

DT <- unique(DT)

addresses <- c("country_code","country_name","address_type",
               "address_1","address_2","city","state","postal_code",
               "telephone_number","fax_number")
taxonomies <- c("code","desc","primary","state1","license","taxonomy_group")

DT.mergedCols <- DT[, address_all := do.call(paste, c(.SD, sep = "_")), .SDcols = addresses]
DT.mergedCols <- DT.mergedCols[, taxonomies_all := do.call(paste, c(.SD, sep = "_")), .SDcols = taxonomies]

names(DT.mergedCols)

DT.mergedCols[, c("country_code","country_name","address_type",
                  "address_1","address_2","city","state","postal_code",
                  "telephone_number","fax_number") := NULL]

DT.mergedCols[, c("code","desc","primary","state1","license","taxonomy_group") := NULL]

names(DT.mergedCols)
str(DT.mergedCols)

DT.long <- dcast(DT.mergedCols, name_prefix + name + first_name + last_name + middle_name +
                   credential + sole_proprietor + gender + enumeration_date + last_updated +
                   status + enumeration_type + number + last_updated_epoch + created_epoch + 
                   reactivation_date + deactivation_date + name_suffix + taxonomies_all ~ address_purpose,
                 value.var = "address_all")

names(DT.long)

DT.split <- cSplit(DT.long, splitCols = "LOCATION", sep = "_", direction = "wide", drop = FALSE)
names(DT.split)
names(DT.split)[22:31] <- paste("l", addresses,sep = ".")

DT.split2 <- cSplit(DT.split, splitCols = "MAILING", sep = "_", direction = "wide", drop = FALSE)
names(DT.split2)
names(DT.split2)[32:41] <- paste("m", addresses,sep = ".")
names(DT.split2)

DT.split2 <- DT.split2[, c("LOCATION","MAILING"):=NULL]
names(DT.split2)

DT.seq <- DT.split2
DT.seq[, seq:=1:.N, by = c("name_prefix","name","first_name","last_name",
                              "middle_name","credential","sole_proprietor","gender",
                              "enumeration_date","last_updated","status","enumeration_type",
                              "number","last_updated_epoch","created_epoch","reactivation_date",
                              "deactivation_date","name_suffix","l.country_code",
                              "l.country_name","l.address_type","l.address_1","l.address_2",
                              "l.city","l.state","l.postal_code","l.telephone_number",
                              "l.fax_number","m.country_code","m.country_name","m.address_type",
                              "m.address_1","m.address_2","m.city","m.state",
                              "m.postal_code","m.telephone_number","m.fax_number")]
names(DT.seq)

DT.long2 <- dcast(DT.seq, name_prefix + name + first_name + last_name + 
                    middle_name + credential + sole_proprietor + gender + 
                    enumeration_date + last_updated + status + enumeration_type + 
                    number + last_updated_epoch + created_epoch + reactivation_date + 
                    deactivation_date + name_suffix + l.country_code + 
                    l.country_name + l.address_type + l.address_1 + l.address_2 + 
                    l.city + l.state + l.postal_code + l.telephone_number + 
                    l.fax_number + m.country_code + m.country_name + m.address_type + 
                    m.address_1 + m.address_2 + m.city + m.state + 
                    m.postal_code + m.telephone_number + m.fax_number ~ seq, value.var = "taxonomies_all")

names(DT.long2)

DT.split3.before <- DT.long2

DT.split3 <- cSplit(DT.split3.before, splitCols = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"),
                    sep = "_", direction = "wide", drop = FALSE)
names(DT.split3)
tax_cols <- c("t.code","t.desc","t.primary","t.state","t.license","t.group", "t.NA")
names(DT.split3)[54:158] <- paste(tax_cols, rep(1:15,each=7), sep = ".")
names(DT.split3)

DT.split3 <- DT.split3 %>% select_if(colSums(!is.na(.)) > 0)
names(DT.split3)

DT.noNAs <- DT.split3[, c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"):=NULL]
names(DT.noNAs)

DT.noNAs <- unique(DT.noNAs)

write.csv(DT.noNAs,"nppes_final_data.csv")
