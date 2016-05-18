dat0 <- read.csv('../test-new-data/dbo_CounterDownload.csv', as.is=T)


decode_string <-
function(string, debug = FALSE, test = FALSE, summary = TRUE) {

    sAF <- getOption('stringsAsFactors')
    options(stringsAsFactors = FALSE)

    ## Split data and convert to binary
    vals0 <- unlist(strsplit(strsplit(string, ',')[[1]], '\t'))
    vals <- as.numeric(vals0)
    vals[!is.na(vals) & vals<0] <- vals[!is.na(vals) & vals<0] + 256
    bvals <- R.utils::intToBin(vals)

    BinToDec <- function(x) 
        sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))

    ## Functions to process the different types of records
    longrec <- function(i) {
        ## Long record (100)
        year <- BinToDec(substr(paste0(bvals[i], bvals[i+1]), 4, 11))
        month <- BinToDec(sprintf('%0.8i', as.numeric(substr(bvals[i+1], 4, 8))))
        day <- BinToDec(sprintf('%0.8i', as.numeric(substr(bvals[i+2], 4, 8))))
        hr <- BinToDec(sprintf('%0.8i', as.numeric(substr(bvals[i+3], 4, 8))))
        c1  <- BinToDec(bvals[i+4]) * 256 + BinToDec(bvals[i+5])
        date <- sprintf('%04i/%02i/%02i', 2000 + year, month, day)
        return(data.frame(date      = date,
                          hour      = hr,
                          count     = c1,
                          type      = 'long',
                          startbyte = i))
    }

    shortrec <- function(i) {
        ## Short record (010)
        hr <- BinToDec(sprintf('%0.8i', as.numeric(substr(bvals[i], 4, 8))))
        c1 <- BinToDec(bvals[i+1]) * 256 + BinToDec(bvals[i+2])
        return(data.frame(date      = NA,
                          hour      = hr,
                          count     = c1,
                          type      = 'short',
                          startbyte = i))
    }

    tinyrec <- function(i) {
        ## Tiny record (011)
        x <- sprintf('%0.8i', as.numeric(substr(bvals[i], 4, 8)))  # Hour
        hr <- BinToDec(x)
        x <- bvals[i+1]
        if (is.na(x)) {
            warning('Problem decoding ', gsub('\t', '\\\\t', vals0[i+1]), ' at ', i+1)
            c1 <- NA
        } else  c1 <- BinToDec(x)
        return(data.frame(date      = NA,
                          hour      = hr,
                          count     = c1,
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
    
    ## Convert datetime to POSIX
    df$datetime <- as.POSIXct(sprintf('%s %s:00', df$date, df$hour))

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

            ## Duplicated date-times
            dupdt <- unique(d$datetime[duplicated(d$datetime)])
            if (length(dupdt)) {
                allpassed <- F
                warning(length(dupdt), ' duplicated datetimes: ', paste(dupdt, collapse=', '))
                ddup <- subset(d, d$datetime %in% dupdt)
                print(ddup)
            }
            return(allpassed)
        }
        ok <- test.decode(df)
        if (ok)  cat('All tests passed\n')  else  cat('Some tests failed!\n')
    }

    ## Merge counts from duplicated date-times
    dupdt <- unique(df$datetime[duplicated(df$datetime)])
    if (length(dupdt)) {
        dedups <- do.call('rbind', lapply(dupdt, function(d) {
            df1 <- df[df$datetime %in% d, ]
            return(data.frame(date = unique(df1$date),
                              hour = unique(df1$hour),
                              count = sum(df1$count),
                              type = paste(unique(df1$type), collapse=','),
                              startbyte = paste(unique(df1$startbyte), collapse=','),
                              datetime = unique(df1$datetime)))
        }))
        df <- rbind(df[!(df$datetime %in% dupdt),], dedups)
        df <- df[order(df$datetime), ]
        rownames(df) <- NULL
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


run <- function() {
    res <- list()
    for (i in 1:nrow(dat0)) {
        cat('\n------------ ', i, ' -------------\n')
        string <- dat0$RawCountData[i]
        if (string != '') {
            dec <- decode_string(string, debug=T)
            df <- data.frame(ix = i, en = dat0$EquipmentNumber[i], cid = dat0$CounterId[i], dec)
            if (nrow(df) > 1)
                res[[i]] <- df
        }
    }
    return(res)
}
