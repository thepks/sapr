library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(tidyr)
library(stringi)

#' A Function To Calculate A Vector Of Quantile Values
#' The function calculates quantile values such as from STAD data
#' @param o The object to be evaluated
#' @param testfield The field to be calculated for
#' @param pattern A filter pattern for the textfield
#' @param summaryfield The field to be summarised
#' @export 
#' @keywords SAP quantile STAD
#' @examples
#' sap_stad_quantile(stad_values, field, filter_value, summaryfield)

sap_stad_quantile <- function(o,testfield,pattern,summaryfield) {

    testf <- which(names(o)==testfield,arr.ind=TRUE)
    items <- o[,testf] == pattern
    df <- as.data.frame ( quantile(o[items,summaryfield][[1]], seq.int(0.01,1,0.01)))
    

    names(df)[1] <- 'val'
    df$percentile <- as.numeric(str_extract(row.names(df),"[0-9]*"))
    row.names(df) <- NULL
    return (df)
}



#' A Function To Clean and Convert SAP Numbers
#' The function replace , and converts to numeric
#' @param f The value to be converted 
#' @keywords SAP number conversion 
#' @export 
#' @examples
#' i <- sap_numeric(stad_value)

sap_numeric <- function (f) {
    return (as.numeric(gsub(",","",f)))
}


#' A Function To Read STAD Unformated Exports
#' The function reads SAP STAD exports
#' @param dir_name The directory containing the STAD exports
#' @param pattern The file pattern to read
#' @param cleanup A function to clean read data
#' @param skip The nuumber of lines to skip
#' @keywords SAP STAD
#' @export 
#' @examples
#' i <- sap_file_load("directory",".*",my_cleanup,4)


sap_file_load <- function(dir_name,pattern,cleanup,skip) {

    baseline_files <- list.files(dir_name)
    matching_str <- match(baseline_files,pattern)
    target_files <- baseline_files[!is.na(matching)]

    if(length(target_files) >= 1) {

                                        # first get first for structure etc

        baseline <- read_delim(paste0(dir_name,"/",target_files[1]), delim="|", skip=skip)

        baseline <- cleanup(baseline)

        for (n in 2:length(target_files)) {

            moredata <- read_delim(paste0(dir_name,"/",target_files[n]), delim="|", skip=skip)
            moredata <- cleanup(moredata)
            baseline <- rbind(baseline,moredata)
        }
    }
    return (baseline)
}

#' A Starter Function To Clean STAD data
#' The function clean SAP STAD exports first field
#' @param f The dataframe to clean
#' @keywords SAP STAD
#' @export 
#' @examples
#' i <- sap_tidy_stad(df)


sap_tidy_stad <- function(f) {

    names(f) <- str_trim(names(f))
    f[,1] <- NULL
    f <- f[-c(1:4),]
    f <- f[-nrow(f),]
    names(f)[1] <- 'Combined'
   
    f <- separate(f,Combined,c('time','server','tcode','program','type','screen','wp'),c(8,25,46,87,89,94,96))

    return(f)
    }

#' A Starter Function To Clean SDF MON header data
#' The function clean SAP SDF MON header exports
#' @param o The dataframe to clean
#' @keywords SAP SDF MON 
#' @export 
#' @examples
#' i <- sap_tidy_sdf(df)


sap_tidy_sdf <- function (o) {

    o[,1] <- NULL
    o <- o[-1,]
    o <- o[-nrow(o),]

    names(o) <- str_trim(names(o))
    o <- select(o,DATUM,TIME,SERVER,ACT_WPS,ACT_DIA,IDLE_TOTAL,USERS,SESSIONS,AVAILCPUS)
    o$begin_date <- dmy(o$DATUM)
    o$begin_l_time <- hms(o$TIME)
    o$ACT_WPS <- sap_numeric(o$ACT_WPS)
    o$IDLE_TIME <- sap_numeric(o$IDLE_TOTAL)
    o$USERS <- sap_numeric(o$USERS)
    o$SESSIONS <- sap_numeric(o$SESSIONS)
    o$AVAILCPUS <- sap_numeric(o$AVAILCPUS)
    o$hour = hour(o$begin_l_time)

    o <- o[str_length(str_trim(o$SERVER)) >0,]

    return (o)

}



#' A Starter Function To Clean ST03N Hourly Data
#' The function clean SAP ST03N hourly export exports
#' @param o The dataframe to clean
#' @keywords SAP ST03N
#' @export 
#' @examples
#' i <- sap_tidy_st03n_hr(df)


sap_tidy_st03n_hr <- function(o) {

                                        # eliminate rows
    o[,1] <- NULL
    o <- o[-1,]
    o <-o[-nrow(o),]
    o <-o[,-ncol(o)]


                                        # first names
    names(o) <- c("interval", "num_steps", "t_response_time", "average_time", "process.", "avg._proc._time", "t_cpu_", "average_cpu_", "t_db_time", "average_db_time", "t_time","average_time_db_proc", "t_roll_wait_time",  "average_roll_wait_time", "t_wait_time", "average_wait_time", "num_trips", "average_time_1", "average_gui_time", "num_vmc_calls", "t_vmc_cpu", "t_vmcelaps", "avgvmc_cpu", "avgvmcelap")

                                        # now types
                                        # first column contains the starting hour
    o <- mutate(o,hr = as.numeric(str <- match(o$interval,"^[0-2][0-9]")))
    o <- mutate_at(o, .funs=sap <- numeric, .cols = c(2:24))

    return (o)
}



#' Calculate Duration Based On Separate Date and Time Fields For Start And End
#' The function calculates duration
#' @param start_date start date
#' @param end_date end date
#' @param start_time start time
#' @param end_time end time
#' @keywords SAP Duration
#' @export 
#' @examples
#' i <- sap_duration_calc(st_dt, en_dt, st_tm, en_tm)


sap_duration_calc <- function(start_date,end_date,start_time,end_time) {

    return (end_time - start_time + difftime(end_date,start_date))


    }

#' Convenience methot to top and tail (remove firat and last column and row from raw SAP output format
#' @param o object to clean
#' @keywords unformated SAP 
#' @export 
#' @examples
#' i <- sap_clean_raw(o)


sap_clean_raw <- function(o){
    o <- o[-1,]
    o <- o[,-1]
    o <- o[-nrow(o),]
    o <- o[,-ncol(o)]

    names(o) <- str_to_lower(str_trim(names(o)))

    return(o)

    }
