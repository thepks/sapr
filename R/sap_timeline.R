
#' A Timeline function
#'
#' The function calculates logical start and end times for an observation to support timeline drawing.
#' The timeline itself is draw by subsequent processing of the returned data.
#'
#' @param o The object to be evaluated
#' @param testfield The field to be monitored for changes within the samples
#' @param timefield The sample datetime field
#' @keywords SAP timeline
#' @export
#' @examples
#' timeline_data  <- sap_timeline(rawdata,'OBJECT_NAME','dt')
#'
#' timeline_data %>% group_by(start_time,OBJECT_NAME) %>% ggplot() + geom <- segment(aes(x=start <- time, xend=dt,y=OBJECT_NAME, yend=OBJECT_NAME,colour=OBJECT_NAME),size=6)+theme(legend.position='none')+labs(title='Sample Timeline')


sap_timeline <- function(o, testfield, timefield) {
    
    myo <- o
    testf <- which(names(o) == testfield, arr.ind = TRUE)
    timef <- which(names(o) == timefield, arr.ind = TRUE)
    laststart <- myo[1, timef]
    lastobj <- myo[1, testf]
    
    
    myo["start_time"] <- myo[, timef]
    
    
    for (i in 2:nrow(myo)) {
        
        curstart <- myo[i, timef]
        curobj <- myo[i, testf]
        
        myo[i, ]$endtime <- curstart
        if (curobj == lastobj) {
            myo[i, timef] <- laststart
        } else {
            
            lastobj <- curobj
            laststart <- curstart
        }
    }
    
    myo <- myo %>% dplyr::group_by_at(timefield)
    
    
    return(myo)
    
}
