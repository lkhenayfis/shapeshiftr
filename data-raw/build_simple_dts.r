library(data.table)

set.seed(1234)

date0 <- as.Date("2025-01-01")
dates <- date0 + 1:20

simple_dt_date <- data.table(
    date = dates,
    X1 = sample(1:20, 20),
    X2 = sample(-20:-1, 20),
    X3 = sample(50:70, 20),
    Y = sample(120:140, 20)
)

simple_dt_date[, day_type := factor(
    ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "weekend", "weekday"),
    levels = c("weekday", "weekend")
)]

set.seed(5678)
simple_dt_date[, weather_condition := factor(
    sample(c("sunny", "cloudy", "rainy"), 20, replace = TRUE),
    levels = c("sunny", "cloudy", "rainy")
)]

usethis::use_data(simple_dt_date, overwrite = TRUE)

set.seed(1234)

datetime0 <- as.POSIXct("2025-01-01 00:00:00", "GMT")
datetimes <- datetime0 + seq(1, 20) * 3600

simple_dt_datetime <- data.table(
    datetime = datetimes,
    X1 = sample(1:20, 20),
    X2 = sample(-20:-1, 20),
    X3 = sample(50:70, 20),
    Y = sample(120:140, 20)
)

hours <- as.POSIXlt(datetimes)$hour
simple_dt_datetime[, day_type := factor(
    ifelse(hours >= 9 & hours < 18, "work_hours", "off_hours"),
    levels = c("work_hours", "off_hours")
)]

set.seed(5678)
simple_dt_datetime[, weather_condition := factor(
    sample(c("sunny", "cloudy", "rainy"), 20, replace = TRUE),
    levels = c("sunny", "cloudy", "rainy")
)]

usethis::use_data(simple_dt_datetime, overwrite = TRUE)

