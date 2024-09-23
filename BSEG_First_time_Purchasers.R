library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
install.packages("formattable", dependencies = TRUE)
library(formattable)
contactdf <- read.csv(file = "C:/Users/ross.williams/Downloads/RStudio/R_Contact_Report.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE)
accountdf <- read.csv(file = "C:/Users/ross.williams/Downloads/RStudio/R_Account_Report.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE)
oppdf <- read.csv(file = "C:/Users/ross.williams/Downloads/RStudio/R_Opps_report_.csv",
                  header = TRUE,
                  stringsAsFactors = FALSE)
opp_productdf <- read.csv(file = "C:/Users/ross.williams/Downloads/RStudio/R_Opp_Products_.csv",
                          header = TRUE,
                          stringsAsFactors = FALSE)

clean_opp_productdf <- opp_productdf %>% 
  mutate(
    close.date = mdy(Close.Date),
    created.date = mdy(Created.Date))


lubridate_oppdf <- oppdf %>% 
  mutate(
    Close.Date = mdy(Close.Date),
    Created.Date = mdy(Created.Date))

# Beginning of Opportunity Table Summary - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# OPPORTUNITY APPENDED SUMMARY
# 1. Filter to table where acct.id = cpa acct id
table1 <- lubridate_oppdf %>%
  filter(Account.ID == CPA.Contact.Acct.Id | Account.ID == CPA.Firm.Id) %>% 
  select(Opportunity.ID = Opportunity.ID,
         Account.ID = Account.ID,
         Stage = Stage,
         Revenue.Type = Revenue.Type,
         Close.Date = Close.Date,
         Opportunity.Record.Type = Opportunity.Record.Type,
         ACV.for.Quotas = ACV.for.Quotas)

# 2. created table 2 with acct.id
table2 <- lubridate_oppdf %>%
  filter(Account.ID != CPA.Contact.Acct.Id & Account.ID != CPA.Firm.Id) %>% 
  select(Opportunity.ID, Account.ID, Revenue.Type, Opportunity.Record.Type, Stage, Close.Date, ACV.for.Quotas)

# 3. Created table 3 with cpa.contact.account.id OR cpa.firm  
table3 <- lubridate_oppdf %>%
  filter(Account.ID != CPA.Contact.Acct.Id & Account.ID != CPA.Firm.Id) %>%
  mutate(CPA.Contact.Acct.Id = case_when(is.na(CPA.Contact.Acct.Id) | CPA.Contact.Acct.Id == "" ~ CPA.Firm.Id,
    TRUE ~ CPA.Contact.Acct.Id)) %>%
  select(Opportunity.ID, Account.ID = CPA.Contact.Acct.Id, Stage, Revenue.Type, Opportunity.Record.Type, Close.Date, ACV.for.Quotas) %>% 
  filter(Account.ID != "")

# 4. Append the three tables
appended_lubridate_oppdf <- bind_rows(table1, table2, table3)

# Appending Account data to Opportunity Summary - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# 5. Joining Client Type to appended lubridate opp df
opp_summary_joined <- appended_lubridate_oppdf %>%
  left_join(accountdf %>% select(Account.ID, Client.Type, Account.Name, Account.Owner),by = "Account.ID") %>% 
  mutate(Close.Year = year(Close.Date))

write.csv(duplicates_df, file = "duplicate_opps_table.csv", row.names = TRUE)
write.csv(appended_lubridate_oppdf, file = "only_unique_opps.csv", row.names = TRUE)

cpa_summary <- opp_summary_joined %>% 
  filter(Close.Year %in% c(2022,2023,2024),
         Stage == "6 - Closed Won",
         Client.Type == "CPA") %>% 
  group_by(Account.Name,Opportunity.Record.Type) %>% 
  summarise(ACV = sum(floor(ACV.for.Quotas), na.rm = TRUE))

cpa_count_ACV_summary <- opp_summary_joined %>%
  filter(
    Stage == "6 - Closed Won",
    Revenue.Type == "New Revenue",
    Client.Type == "CPA"
  ) %>%
  filter(Close.Year %in% c(2022, 2023, 2024)) %>% # Filter to specific years
  group_by(Close.Year) %>%
  summarize(
    Distinct_Account_IDs = n_distinct(Account.ID), # Count of distinct Account.IDs
    Total_ACV = sum(ACV.for.Quotas, na.rm = TRUE) # Sum of ACV.for.Quotas
  ) %>% 
  mutate(Total_ACV = dollar(Total_ACV))

distinct_account_counts <- opp_summary_joined %>%
  filter(Client.Type == "CPA") %>% # Filter to Client.Type = CPA
  summarize(
    Closed_Won = n_distinct(Account.ID[Stage == "6 - Closed Won"]),    # Count for Closed Won
    All_Inclusive = n_distinct(Account.ID) # Count for other stages
  )

# View the result
print(distinct_account_counts)
# View the result
print(total_distinct_accounts)

# First Won Opportunity - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

cpa_first_opp_summary <- opp_summary_joined %>% 
  filter(Stage == "6 - Closed Won",
         Client.Type == "CPA") %>%
  select(Account.Owner,Account.ID, Account.Name, Close.Date, Close.Year) %>% 
  group_by(Account.ID, Account.Name) %>% 
  arrange(Close.Date) %>% 
  slice(1) %>% 
  ungroup() %>% 
  rename(First_Won_Opp = Close.Date) %>% 
  arrange(desc(Close.Year))

write.csv(cpa_first_opp_summary,file = "New_Firm_Summary.csv", row.names = TRUE)

rm(process.events)


# Sales by Count of Service Lines Referred - Closed Won  - - - - - - - - - - - - - - - - - - - - - - -

library(dplyr)
library(tidyr)
library(formattable) # For formatting numbers as currency

# Step 1: Filter the data
filtered_data <- opp_summary_joined %>%
  filter(Stage == "6 - Closed Won", Client.Type == "CPA")

# Step 2: Count distinct opportunity.record.types per account.name
distinct_counts <- filtered_data %>%
  group_by(Account.Name) %>%
  summarize(distinct_count = n_distinct(Opportunity.Record.Type))

# Step 3: Join the distinct counts back to the main data
filtered_data <- filtered_data %>%
  left_join(distinct_counts, by = "Account.Name")

# Step 4: Summarize the acv.for.quotas by distinct count of opportunity.record.types and year
summary_data <- filtered_data %>%
  mutate(year = Close.Year) %>% # Replace YourDateColumn with the actual column name containing date information
  filter(year %in% c(2022, 2023, 2024)) %>%
  group_by(distinct_count, year) %>%
  summarize(avg_acv = mean(ACV.for.Quotas, na.rm = TRUE)) %>%
  pivot_wider(names_from = year, values_from = avg_acv, values_fill = 0) # Keep values numeric here

# Step 5: Format the total_acv columns as currency after pivoting
won_opps_summary <- summary_data %>%
  mutate(across(starts_with("20"), ~ as.character(currency(.x, symbol = "$", digits = 0))))
rm(summary_data)

# Sales by Count of Service Lines Referred - All Opportunities  - - - - - - - - - - - - - - - - - - - - - - -

# Step 1: Filter the data
filtered_data <- opp_summary_joined %>%
  filter(Client.Type == "CPA")

# Step 2: Count distinct opportunity.record.types per account.name
distinct_counts <- filtered_data %>%
  group_by(Account.Name) %>%
  summarize(distinct_count = n_distinct(Opportunity.Record.Type))

# Step 3: Join the distinct counts back to the main data
filtered_data <- filtered_data %>%
  left_join(distinct_counts, by = "Account.Name")

# Step 4: Summarize the acv.for.quotas by distinct count of opportunity.record.types and year
summary_data <- filtered_data %>%
  mutate(year = Close.Year) %>% # Replace YourDateColumn with the actual column name containing date information
  filter(year %in% c(2022, 2023, 2024)) %>%
  group_by(distinct_count, year) %>%
  summarize(avg_acv = mean(ACV.for.Quotas, na.rm = TRUE)) %>%
  pivot_wider(names_from = year, values_from = avg_acv, values_fill = 0) # Keep values numeric here

# Step 5: Format the total_acv columns as currency after pivoting
all_opps_summary <- summary_data %>%
  mutate(across(starts_with("20"), ~ as.character(currency(.x, symbol = "$", digits = 0))))
