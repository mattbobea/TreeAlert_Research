##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ----                           Rules Rephrased                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rephrase_rule <- function(text) {
  library(stringr)
  # String Pre-processing ----------------------------------------------------------- 
  
  # Replace this text with that text
  replacements <- c(
    "Value" = "the Value",
    "or" = "and",
    " Sun " = " Sundays ", "Sun$" = "Sundays",
    " Mon " = " Mondays ", "Mon$" = "Mondays",
    " Tue " = " Tuesdays ", "Tue$" = "Tuesdays",
    " Wed " = " Wednesdays ", "Wed$" = "Wednesdays", 
    " Thu " = " Thursdays ", "Thu$" = "Thursdays", 
    " Fri " = " Fridays ", "Fri$" = "Fridays",
    " Sat " = " Saturdays ", "Sat$" = "Saturdays"
  )
  
  text <- stringr::str_replace_all(text, replacements)
  
  # Adjust into section--------------------------------------------------------- 
  
  # Extract the first number
  first_number <- as.numeric(sub(" .*", "", text))
  
  # Check if the number is positive or negative and replace accordingly
  if (first_number > 0) {
    text <- sub("^\\S+", "The model under forecasts", text)
  } else if (first_number < 0) {
    text <- sub("^\\S+", "The model over forecasts", text)
  }
  
  # Split the sentence into a list using "when" or "&" as delimiters
  split_text <- strsplit(text, "\\s*when\\s*|\\s*&\\s*")[[1]]
  
  # Adjust Hourly section --------------------------------------------------------
  hour_item_index <- grep("^Hour", split_text)
  
  # Process the "Hour" item(s)
  if (length(hour_item_index) > 0) {
    # Extract and format Hour values
    hours <- as.numeric(str_extract_all(split_text[hour_item_index],
                                        "\\d+")[[1]])
    if (length(hours) > 0) {
      format_hour <- function(hour) {
        period <- ifelse(hour < 12, "AM", "PM")
        hour <- ifelse(hour == 0, 12, ifelse(hour > 12, hour - 12, hour))
        paste0(hour, ":00", period)
      }
      hour_groups <- split(hours, cumsum(c(1, diff(hours) != 1)))
      time_info <- sapply(hour_groups, function(group) {
        start_time <- format_hour(group[1])
        end_time <- if (length(group) > 1) format_hour(group[length(group)]) else ""
        if (end_time == "") start_time else paste(start_time, "-", end_time)
      })
      split_text[hour_item_index] <- paste("during Hours",
                                           paste(time_info, collapse = " and "))
    }
  }
  
  # Adjust Weekday section -------------------------------------------------------
  
  weekday_item_index <- grep("^Weekday", split_text)
  
  # Process the "Weekday" item(s)
  if (length(weekday_item_index) > 0) {
    weekdays <- str_extract_all(split_text[weekday_item_index],
                                "(Sunday|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday)")[[1]]
    if (length(weekdays) > 0) {
      day_order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
      day_indices <- match(weekdays, day_order)
      day_groups <- split(weekdays, cumsum(c(1, diff(day_indices) != 1)))
      weekday_info <- sapply(day_groups, function(group) {
        if (length(group) > 1) {
          paste(group[1], "through", group[length(group)])
        } else {
          group
        }
      })
      split_text[weekday_item_index] <- paste("on", paste(weekday_info,
                                                          collapse = " and "))
    }
  }
  
  # Adjust WeekofMonth section ---------------------------------------------------
  weekofmonth_item_index <- grep("^WeekofMonth", split_text)
  
  if (length(weekofmonth_item_index) > 0) {
    weeks <- as.numeric(str_extract_all(split_text[weekofmonth_item_index], "\\d+")[[1]])
    if (length(weeks) > 0) {
      # Function to add ordinal suffixes
      ordinal_suffix <- function(x) {
        if (x %% 10 == 1 && x %% 100 != 11) return(paste0(x, "st"))
        if (x %% 10 == 2 && x %% 100 != 12) return(paste0(x, "nd"))
        if (x %% 10 == 3 && x %% 100 != 13) return(paste0(x, "rd"))
        return(paste0(x, "th"))
      }
      
      # Group consecutive weeks
      week_groups <- split(weeks, cumsum(c(1, diff(weeks) != 1)))
      week_info <- sapply(week_groups, function(group) {
        if (length(group) > 1) {
          paste(ordinal_suffix(group[1]), "through", ordinal_suffix(group[length(group)]), "weeks")
        } else {
          paste(ordinal_suffix(group[1]), "week")
        }
      })
      
      # Join the consolidated weeks into a descriptive phrase
      split_text[weekofmonth_item_index] <- paste(
        "during the", paste(week_info, collapse = " and "), "of the month"
      )
    }
  }
  
  # Generate final rephrased text ------------------------------------------------
  
  # Recombine with commas and final "and"
  condition_text <- if (length(split_text) > 2) {
    paste(
      paste(split_text[2:(length(split_text)-1)], collapse = ", "),
      "and", split_text[length(split_text)]
    )
  } else if (length(split_text) == 2) {
    split_text[2]
  } else {
    ""
  }
  
  final_text <- paste0(split_text[1], " when ", condition_text)
  final_text <- str_replace_all(final_text, ",\\s*\\.", ".")  # Clean trailing comma
  final_text <- str_trim(final_text)
  
  return(final_text)
}

# Example usage
text <- "4769.66  when  Weekday  is  Wed or Thu or Fri or Sat  &  Hour  is  2 or 3 or 9  &  Minute  is  15"
rephrased_text <- rephrase_rule(text)
cat("Rephrased Text:", rephrased_text)
# for list of hours, use commas in between them all except the last joint is and.  same thing for days and months.  
