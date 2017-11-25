
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

sap_stad_quantile <- function(o, testfield, pattern, summaryfield) {
    
    testf <- which(names(o) == testfield, arr.ind = TRUE)
    items <- o[, testf] == pattern
    df <- as.data.frame(quantile(o[items, summaryfield][[1]], seq.int(0.01, 1, 0.01)))
    
    
    names(df)[1] <- "val"
    df$percentile <- as.numeric(stringr::str_extract(row.names(df), "[0-9]*"))
    row.names(df) <- NULL
    return(df)
}



#' A Function To Clean and Convert SAP Numbers
#' The function replace , and converts to numeric
#' @param f The value to be converted 
#' @keywords SAP number conversion 
#' @export 
#' @examples
#' i <- sap_numeric(stad_value)

sap_numeric <- function(f) {
# (\\d(?=\\..*,))|(^0?,\\d{3,})|(\\d,\\d{4,}(?!.*\\.))|(\\d\\.\\d{3}$)
# (\.\d{1,}(?=,|\.))|((,\d{3}){1,}(?!\.))
                                        # (\\d(?=\\.\\d{3}))|(\\d\\,(?!\\d{3}))
    # (\.\d{3}\,)|((\d){1,3}(\.)(\d{3}(\3|$))(?<!\,))

#    f <- ifelse ( stringr::str_count(pattern="(\\.\\d{3}\\,)|(^(\\d){1,3}(\\.)(\\d{3}(\\\4|$)))", string=f) >0, sap_numeric_comma(f), sap_numeric_point(f))

    a <- stringr::str_count(pattern="(\\.\\d{3}\\,)|(^(\\d){1,3}(\\.)(\\d{3}(\\4|$)))|(^\\d*,\\d{1,2}$)|(^\\d*,\\d{4,}$)", string=f) >0
    f[a] <- sap_numeric_comma(f[a])
    f[!a] <- sap_numeric_point(f[!a])
       
    return(f)
}

#' A Function To Clean and Convert SAP Numbers where the , is teh decimal indicator
#' The function replace , and converts to numeric
#' @param f The value to be converted 
#' @keywords SAP number conversion 
#' @export 
#' @examples
#' i <- sap_numeric_comma(stad_value)

sap_numeric_comma <- function(f) {

    f <- gsub("\\.","",f)
    f <- gsub(",",".",f)
    return(as.numeric(f))
}

#' A Function To Clean and Convert SAP Numbers where the . is the decimal indicator
#' The function replace , and converts to numeric
#' @param f The value to be converted 
#' @keywords SAP number conversion 
#' @export 
#' @examples
#' i <- sap_numeric_point(stad_value)

sap_numeric_point <- function(f) {

    f <- gsub(",","",f)
    return (as.numeric(f))
    
}

#' A Function To Clean and Convert SAP Dates
#' The function replace , and converts to dates
#' @param f The value to be converted
#' @param l The lubridate pattern, optional
#' @keywords SAP date conversion 
#' @export 
#' @examples
#' i <- sap_date(date_value,"ymd")

sap_date <- function(f) {
    if( stringr::str_count(pattern="(\\.|\\/)", string=f)>0 ) {
        if(stringr::str_locate("(\\.|\\/)",string=f)[1] == 5) {
            return(lubridate::ymd(f))
        } else {
            return(lubridate::dmy(f))
        }
        
    } else {
        return(lubridate::ymd(f))
    }
}


#' A Function To Clean and Convert SAP Times in the format HHMMSS
#' @param f The value to be converted 
#' @keywords SAP time conversion 
#' @export 
#' @examples
#' i <- sap_time("083000")

sap_time <- function(f) {
    return( f %>% stringr::str_trim() %>% stringr::str_match_all("\\d{2}(:)?") %>% stringr::str_c(sep=":") %>% lubridate::hms())
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
#' i <- sap_file_load('directory','.*',my_cleanup,4)


sap_file_load <- function(dir_name, pattern, cleanup, skip) {
    
    baseline_files <- list.files(dir_name)
    matching <- str_match(baseline_files, pattern)
    target_files <- baseline_files[!is.na(matching)]
    
    if (length(target_files) >= 1) {
        
        # first get first for structure etc
        
        baseline <- readr::read_delim(paste0(dir_name, "/", target_files[1]), delim = "|", 
            skip = skip)
        
        baseline <- cleanup(baseline)
        
        for (n in 2:length(target_files)) {
            
            moredata <- readr::read_delim(paste0(dir_name, "/", target_files[n]), delim = "|", 
                skip = skip)
            moredata <- cleanup(moredata)
            baseline <- rbind(baseline, moredata)
        }
    }
    return(baseline)
}

#' A Starter Function To Clean STAD data
#' The function clean SAP STAD exports first field
#' @param f The dataframe to clean
#' @keywords SAP STAD
#' @export 
#' @examples
#' i <- sap_tidy_stad(df)


sap_tidy_stad <- function(f) {
    
    names(f) <- stringr::str_trim(names(f))
    f[, 1] <- NULL
    names(f) <- paste(names(f),stringr::str_trim(f[1,]))
    f <- f[-c(1:4), ]
    f <- f[-nrow(f), ]
    names(f)[1] <- "Combined"
    
    f <- tidyr::separate(f, Combined, c("time", "server", "tcode", "program", "type", "screen", 
        "wp"), c(8, 25, 46, 87, 89, 94, 96))
    
    return(f)
}

#' A Starter Function To Clean SDF MON header data
#' The function clean SAP SDF MON header exports
#' @param o The dataframe to clean
#' @keywords SAP SDF MON 
#' @export 
#' @examples
#' i <- sap_tidy_sdf(df)


sap_tidy_sdf <- function(o) {
    
    o[, 1] <- NULL
    o <- o[-1, ]
    o <- o[-nrow(o), ]
    
    names(o) <- stringr::str_trim(names(o))
    o <- select(o, DATUM, TIME, SERVER, ACT_WPS, ACT_DIA, IDLE_TOTAL, USERS, SESSIONS, 
        AVAILCPUS)
    o$begin_date <- lubridate::dmy(o$DATUM)
    o$begin_l_time <- lubridate::hms(o$TIME)
    o$ACT_WPS <- sap_numeric(o$ACT_WPS)
    o$IDLE_TIME <- sap_numeric(o$IDLE_TOTAL)
    o$USERS <- sap_numeric(o$USERS)
    o$SESSIONS <- sap_numeric(o$SESSIONS)
    o$AVAILCPUS <- sap_numeric(o$AVAILCPUS)
    o$hour = lubridate::hour(o$begin_l_time)
    
    o <- o[stringr::str_length(stringr::str_trim(o$SERVER)) > 0, ]
    
    return(o)
    
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
    o[, 1] <- NULL
    o <- o[-1, ]
    o <- o[-nrow(o), ]
    o <- o[, -ncol(o)]
    
    
    # first names
    names(o) <- c("interval", "num_steps", "t_response_time", "average_time", "process.", 
        "avg._proc._time", "t_cpu_", "average_cpu_", "t_db_time", "average_db_time", 
        "t_time", "average_time_db_proc", "t_roll_wait_time", "average_roll_wait_time", 
        "t_wait_time", "average_wait_time", "num_trips", "average_time_1", "average_gui_time", 
        "num_vmc_calls", "t_vmc_cpu", "t_vmcelaps", "avgvmc_cpu", "avgvmcelap")
    
    # now types first column contains the starting hour
    o <- tidyr::mutate(o, hr = as.numeric(str <- match(o$interval, "^[0-2][0-9]")))
    o <- tidyr::mutate_at(o, .funs = sap <- numeric, .cols = c(2:24))
    
    return(o)
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


sap_duration_calc <- function(start_date, end_date, start_time, end_time) {
    
    return(end_time - start_time + difftime(end_date, start_date))
    
    
}

#' Convenience method to top and tail (remove firat and last column and row from raw SAP output format
#' @param o object to clean
#' @keywords unformated SAP 
#' @export 
#' @examples
#' i <- sap_clean_raw(o)


sap_clean_raw <- function(o) {
    o <- o[-1, ]
    o <- o[, -1]
    o <- o[-nrow(o), ]
    o <- o[, -ncol(o)]
    
    names(o) <- stringr::str_to_lower(str_trim(names(o)))
    
    return(o)
    
}

#' A Function To Form A Data Frame Of Missing Values
#' The function gives a data frame of columns that have missing data and the quantity in each
#' @param o The object to be evaluated
#' @export 
#' @keywords na
#' @examples
#' sap_missing_values(df)

sap_missing_values <- function(o) {

    absent_data_cols <- summarise_all(o,check_na)
    absent_data_cols_ind <- absent_data_cols[1,]>0
    absent_items <- t(absent_data_cols[,absent_data_cols_ind[1,]])
    absent_items <- cbind(absent_items,feature=row.names(absent_items))
    absent_items <- as.data.frame(absent_items,stringsAsFactors=FALSE)
    names(absent_items)[1] <- "gaps"
    absent_items$gaps <- as.numeric(absent_items$gaps)
    absent_items <- absent_items[order(absent_items$gaps,decreasing = TRUE),]
    row.names(absent_items) <- NULL
    return(absent_items)
}
