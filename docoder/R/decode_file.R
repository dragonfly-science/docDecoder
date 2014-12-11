decode_file <-
function(file, debug = FALSE, test = TRUE, summary = TRUE) {

    sAF <- getOption('stringsAsFactors')
    options(stringsAsFactors = FALSE)

    ## Split data and convert to binary
    raw <- readLines(file, warn=F)
    vals0 <- unlist(strsplit(strsplit(raw, ',')[[1]], '\t'))
    vals <- as.numeric(vals0)
    vals[!is.na(vals) & vals<0] <- vals[!is.na(vals) & vals<0] + 256
    bvals <- intToBin(vals)

    BinToDec <- function(x) 
        sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))

    ## Functions to process the different types of records
    longrec <- function(i) {
        ## Long record (100)
        year <- BinToDec(substr(paste0(bvals[i], bvals[i+1]), 4, 11))
        month <- BinToDec(sprintf('%0.8i', as.numeric(substr(bvals[i+1], 4, 8))))
        day <- BinToDec(sprintf('%0.8i', as.numeric(substr(bvals[i+2], 4, 8))))
        hr <- BinToDec(sprintf('%0.8i', as.numeric(substr(bvals[i+3], 4, 8))))
        c1 <- BinToDec(bvals[i+4]) # Count 1
        c2 <- BinToDec(bvals[i+5]) # Count 2
        date <- sprintf('%04i/%02i/%02i', 2000 + year, month, day)
        return(data.frame(date      = date,
                          hour      = hr,
                          count1    = c1,
                          count2    = c2,
                          type      = 'long',
                          startbyte = i))
    }

    shortrec <- function(i) {
        ## Short record (010)
        hr <- BinToDec(sprintf('%0.8i', as.numeric(substr(bvals[i], 4, 8))))
        c1 <- BinToDec(bvals[i+1])
        c2 <- BinToDec(bvals[i+2])
        return(data.frame(date      = NA,
                          hour      = hr,
                          count1    = c1,
                          count2    = c2,
                          type      = 'short',
                          startbyte = i))
    }

    tinyrec <- function(i) {
        ## Tiny record (011)
        x <- sprintf('%0.8i', as.numeric(substr(bvals[i], 4, 8)))  # Hour
        hr <- BinToDec(x)
        x <- bvals[i+1] # Count 1
        if (is.na(x)) {
            warning('Problem decoding ', gsub('\t', '\\\\t', vals0[i+1]), ' at ', i+1)
            c1 <- NA
        } else  c1 <- BinToDec(x)
        return(data.frame(date      = NA,
                          hour      = hr,
                          count1    = NA,
                          count2    = c1,
                          type      = 'tiny',
                          startbyte = i))
    }


    ## Loop over records
    i <- 1
    j <- 1
    currdate <- NA
    res <- list()
    while (i < length(bvals)) {
        type <- substr(bvals[i], 1, 3)
        if (!is.na(type)) {
            if (type == '100') {
                r <- longrec(i)
                i <- i + 6
            } else if (type == '010') {
                r <- shortrec(i)
                i <- i + 3
            } else if (type == '011') {
                r <- tinyrec(i)
                i <- i + 2
            } else {
                warning('Cannot guess the record type at ', i, ' - skipping one byte')
                i <- i + 1
            }
            if (!is.na(r$date)) currdate <- r$date else r$date <- currdate
            res[[j]] <- r
            j <- j + 1
        } else {
            warning('Problem during decoding of first byte at ', i, ' (', dQuote(vals0[i]), ').')
            i <- i + 1
        }
    }
    df <- do.call('rbind', res)
    
    ## Convert datetime to POSIX and sum counts over the two bytes
    df$datetime <- as.POSIXct(sprintf('%s %s:00', df$date, df$hour))
    df$count <- with(df, ifelse(is.na(count1), 0, count1) + count2)

    ## Remove duplicated data
    pre_n <- nrow(df)
    isdup <- duplicated(paste0(df$datetime, df$count))
    df <- df[!isdup, ]
    post_n <- nrow(df)
    if (pre_n != post_n & debug)
        warning('Duplicates removal changed number of rows from ', pre_n, ' to ', post_n)

    ## Test decoded data
    if (test) {
        test.decode <- function(d) {
            cat('\nTesting...\n')
            allpassed <- T

            ## Jumps backward in time
            difft <- difftime(d$datetime[2:nrow(d)], d$datetime[1:(nrow(d)-1)], units='hours')
            w <- which(difft < 0)
            if (length(w) & debug) {
                allpassed <- F
                warning('Some jumps backward in time:')
                for (w1 in w) {
                    print(d[w1:(w1+1), ])
                }
            }

            ## NAs
            isNA <- is.na(d$datetime) | is.na(d$count)
            if (any(isNA)) {
                allpassed <- F
                warning('Some NAs in datetime or count:')
                print(d[isNA,])
            }
            if (allpassed)  cat('Ok.\n')

            ## Duplicated date-times
            dupdt <- duplicated(d$datetime)
            if (sum(dupdt)) {
                allpassed <- F
                warning(sum(dupdt), ' duplicated datetimes:')
                ddup <- subset(d, d$datetime %in% d[dupdt, 'datetime'])
                head(ddup);tail(ddup)
            }
            return(allpassed)
        }
        ok <- test.decode(df)
        if (ok)  cat('All tests passed\n')  else  cat('Some tests failed!\n')
    }

    ## Remove intermediate columns
    selvars <- c('datetime', 'count')
    if (!debug) {
        df <- df[, selvars]
    } else {
        df <- df[, c(selvars, setdiff(colnames(df), selvars))]
    }
    if (summary) {
        cat('\n')
        cat(nrow(df), 'counts decoded.\n')
        cat(paste0('Dates range between ', min(df$datetime, na.rm=T), ' and ', max(df$datetime, na.rm=T),'.\n'))
        cat(paste0('Counts range between ', min(df$count, na.rm=T), ' and ', max(df$count, na.rm=T), '.\n'))
    }
    options(stringsAsFactors = sAF)
    return(df)
}
