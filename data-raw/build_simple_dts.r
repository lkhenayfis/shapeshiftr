library(data.table)

set.seed(1234)

date0 <- as.Date("2025-01-01")
simple_dt_date <- data.table(
    date = date0 + 1:20,
    X1 = sample(1:20, 20),
    X2 = sample(-20:-1, 20),
    X3 = sample(50:70, 20),
    Y = sample(120:140, 20)
)
usethis::use_data(simple_dt_date, overwrite = TRUE)

datetime0 <- as.POSIXct("2025-01-01 00:00:00", "GMT")
simple_dt_datetime <- data.table(
    datetime = datetime0 + seq(1, 20) * 3600,
    X1 = sample(1:20, 20),
    X2 = sample(-20:-1, 20),
    X3 = sample(50:70, 20),
    Y = sample(120:140, 20)
)
usethis::use_data(simple_dt_datetime, overwrite = TRUE)

