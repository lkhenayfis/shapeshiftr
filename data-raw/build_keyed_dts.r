library(data.table)

set.seed(1234)

date0 <- as.Date("2025-01-01")
keyed_dt_date <- data.table(
    date = date0 + rep(seq(1, 20), each = 5),
    target_date = date0 + rep(seq(1, 20), each = 5) + rep(seq(1, 5), 20),
    X1 = sample(1:20, 20),
    X2 = sample(-20:-1, 20),
    X3 = sample(50:70, 20),
    Y = sample(120:140, 20)
)
usethis::use_data(keyed_dt_date, overwrite = TRUE)

datetime0 <- as.POSIXct("2025-01-01 00:00:00", "GMT")
keyed_dt_datetime <- data.table(
    datetime = datetime0 + rep(seq(1, 20) * 3600, each = 5),
    target_datetime = datetime0 + rep(seq(1, 20) * 3600, each = 5) + rep(seq(1, 5) * 3600, 20),
    X1 = sample(1:100, 100),
    X2 = sample(-100:-1, 100),
    X3 = sample(50:150, 100),
    Y = sample(120:220, 100)
)
usethis::use_data(keyed_dt_datetime, overwrite = TRUE)