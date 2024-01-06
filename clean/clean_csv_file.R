library(readr)
library(lubridate)

root_path = "../../../Downloads"
original_csv_name = "CSVData (5).csv"

df = readr::read_csv(file.path(root_path, original_csv_name), col_names = FALSE)

df %>% glimpse()

colnames(df) = c("Date", "Amount", "Description", "Balance")

new_df = 
    df %>% 
    mutate(Date = ymd(dmy(Date)),
           Amount = -Amount) %>% 
    filter(Amount > 0) %>% 
    arrange(Date)

new_df %>% View()

new_filename = "bas_csv_dec_2023.csv"

readr::write_csv(new_df, file.path(root_path, new_filename))
