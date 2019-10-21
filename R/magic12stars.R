#' @name magic12stars
#' @title Magic 12 Stars
#'
#' This function asks for an input of a date and
#' returns the corresponding astrological sign
#' if one is born on that day.
#'
#' The function uses the lubridate() package to
#' get the year, month, and the day from a given date.
#' The code is from Vitalie Spinu (github@vspinu)
#'
#' @param testdate What is the chosen date to be tested? Defaults to the birthday of this package.
#' @import lubridate
#' @export
#' @examples
#' magic12stars()

library(lubridate)

magic12stars <- function(testdate=20191020) {
    testdate <- readline(cat("What date is in your mind?",
                             "Please enter the date in yyyymmdd format.",
                             sep = "\n"))

    theyear <- year(ymd(testdate))
    themonth <- month(ymd(testdate))
    theday <- day(ymd(testdate))

    if (is.na(themonth) == T | is.na(theday) == T) {
        warning("Sorry, the date is invalid.")
    } else if ((themonth == 3 & theday >= 21 & theday <= 31) |
               (themonth == 4 & theday <= 19 & theday >= 0)) {
        cat("Hi, congratulations!",
            "You are a Aries baby!",
            sep = "\n")
    } else if ((themonth == 4 & theday >= 20 & theday <= 30) |
               (themonth == 5 & theday <= 20 & theday >= 0)) {
        cat("Hi, congratulations!",
            "You are a Taurus baby!",
            sep = "\n")
    } else if ((themonth == 5 & theday >= 21 & theday <= 31) |
               (themonth == 6 & theday <= 21 & theday >= 0)) {
        cat("Hi, congratulations!",
            "You are a Gemini baby!",
            sep = "\n")
    } else if ((themonth == 6 & theday >= 23 & theday <= 30) |
               (themonth == 7 & theday <= 22 & theday >= 0)) {
        cat("Hi, congratulations!",
            "You are a Cancer baby!",
            sep = "\n")
    } else if ((themonth == 7 & theday >= 23 & theday <= 31) |
               (themonth == 8 & theday <= 22 & theday >= 0)) {
        cat("Hi, congratulations!",
            "You are a Leo baby!",
            sep = "\n")
    } else if ((themonth == 8 & theday >= 23 & theday <= 31) |
               (themonth == 9 & theday <= 22 & theday >= 0)) {
        cat("Hi, congratulations!",
            "You are a Virgo baby!",
            sep = "\n")
    } else if ((themonth == 9 & theday >= 23 & theday <= 30) |
               (themonth == 10 & theday <= 23 & theday >= 0)) {
        cat("Hi, congratulations!",
            "You are a Libra baby!",
            sep = "\n")
    } else if ((themonth == 10 & theday >= 24 & theday <= 31) |
               (themonth == 11 & theday <= 21 & theday >= 0)) {
        cat("Hi, congratulations!",
            "You are a Scorpio baby!",
            sep = "\n")
    } else if ((themonth == 11 & theday >= 22 & theday <= 30) |
               (themonth == 12 & theday <= 21 & theday >= 0)) {
        cat("Hi, congratulations!",
            "You are a Sagittarius baby!",
            sep = "\n")
    } else if ((themonth == 12 & theday >= 22 & theday <= 31) |
               (themonth == 1 & theday <= 19 & theday >= 0)) {
        cat("Hi, congratulations!",
            "You are a Capricorn baby!",
            sep = "\n")
    } else if ((themonth == 1 & theday >= 20 & theday <= 31) |
               (themonth == 2 & theday <= 18 & theday >= 0)) {
        cat("Hi, congratulations!",
            "You are a Aquarius baby!",
            sep = "\n")
    } else if ((themonth == 2 & theday >= 19 & theday <= 29 & leap_year(theyear) == T) |
               (themonth == 3 & theday <= 20 & theday >= 0)) {
        cat("Hi, congratulations!",
            "You are a Pises baby!",
            sep = "\n")
    } else if ((themonth == 2 & theday >= 19 & theday <= 28 & leap_year(theyear) == F) |
               (themonth == 3 & theday <= 20 & theday >= 0)) {
        cat("Hi, congratulations!",
            "You are a Pises baby!",
            sep = "\n")
    }
}
