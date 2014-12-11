decode_file <-
function(file, debug = FALSE, test = TRUE) {

    sAF <- getOption('stringsAsFactors')
    options(stringsAsFactors = FALSE)
    
    raw <- readLines(file, warn=F)
    vals0 <- unlist(strsplit(strsplit(raw, ',')[[1]], '\t'))
    vals <- as.numeric(vals0)
    vals[!is.na(vals) & vals<0] <- vals[!is.na(vals) & vals<0] + 256

    bvals <- intToBin(vals)

    BinToDec <- function(x) 
        sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))

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

    currdate <- NA

    i <- 1
    j <- 1
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
                warning('Cannot guess the type at ', i, ' - Skip 1')
                i <- i + 1
            }
            if (!is.na(r$date)) currdate <- r$date else r$date <- currdate
            res[[j]] <- r
            j <- j + 1
        } else {
            warning('Problem during decoding of first byte at ', i, ': ', vals0[i])
            i <- i + 1
        }
    }
    df <- do.call('rbind', res)

    df$datetime <- as.POSIXlt(sprintf('%s %s:00', df$date, df$hour))
    df$count <- with(df, ifelse(is.na(count1), 0, count1) + count2)

    pre_n <- nrow(df)
    isdup <- duplicated(paste0(df$datetime, df$count))
    df <- df[!isdup, ]
    post_n <- nrow(df)
    if (pre_n != post_n)  warning('Duplicates removal changed number of rows from ', pre_n, ' to ', post_n)

    if (test) {
        test.decode <- function(d) {
            cat('\nTesting...\n')
            allpassed <- T
            ## Jumps backward in time
            difft <- difftime(d$datetime[2:nrow(d)], d$datetime[1:(nrow(d)-1)], units='hours')
            w <- which(difft < 0)
            if (length(w)) {
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
            sum(dupdt)
            return(allpassed)
        }
        ok <- test.decode(df)
    }

    selvars <- c('datetime', 'count')
    if (!debug) {
        df <- df[, selvars]
    } else {
        df <- df[, c(selvars, setdiff(colnames(df), selvars))]
    }

    options(stringsAsFactors = sAF)
    return(df)
}
