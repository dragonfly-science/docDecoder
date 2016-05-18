source('../docoder/R/decode_string.R')
source('../optimisations/decode_string.R')

## library(parallel)
library(data.table)
library(compiler)
library(lineprof)

decode_string0 <- decode_string
decode_string <- cmpfun(decode_string)

dat0 <- read.csv('./dbo_CounterDownload.csv', as.is=T)

comp <- read.csv('./comparison.csv', as.is=T)
comp$dt <- as.POSIXct(comp$datetime, format='%d/%m/%Y %H:%M')
comp$dttxt <- as.character(comp$dt)
comp <- comp[order(comp$dt), ]

## trace("decode_string")

l <- lineprof({
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
})

## untrace("decode_string")

r <- do.call('rbind', res)
r$dt <- r$datetime
r$dttxt <- as.character(r$dt)
r <- data.table(r)
r <- r[, list(ix = paste(unique(ix), collapse=','),
             en = paste(unique(en), collapse=','),
             cid = paste(unique(cid), collapse=','),
             count = sum(count),
             type = paste(unique(type), collapse=','),
             startbyte = paste(unique(startbyte), collapse=',')),
      by=list(datetime, hour, date, dt, dttxt)]
if (any(duplicated(r$datetime)))
    stop('Still some duplicated date/times')

m <- merge(comp, r, by='dt', all=F)
m$diff_prevver <- m$COUNT.SAP - m$count.x
m$diff_newver <-  m$COUNT.SAP - m$count.y


tabl(m$diff_prevver, sort=F)
tabl(m$diff_newver, sort=F)

diff <- subset(m, COUNT.SAP != count.y)
diff[, c('dt', 'count.x', 'COUNT.SAP', 'count.y', 'type', 'ix', 'startbyte')]


## Check for duplicated datetimes
for (i in 1:length(res)) {
    x <- res[[i]]
    dups <- unique(x$datetime[duplicated(x$datetime)])
    for (dup in dups) {
        duprows <- x[x$datetime %in% dup, ]
        duprows$dttxt <- as.character(duprows$datetime)
        print(duprows)
    }
}

dups <- r$datetime[duplicated( r$datetime )]
duplicated_rows(r, 'datetime')


subset(m, diff_newver %in% c(-116, 107))

ts1 <- unique(comp$dt)
ts2 <- unique(r$dt)




m[m$count.y - m$count.x == -148, ]

subset(m, count.x != COUNT.SAP)
subset(m, count0 != COUNT.SAP)
m[13830,]

string <- dat0$RawCountData[2]

res <- sapply(1:length(dat), function(i) {
    return(data.frame(ix = i, decode_string(dat[i])))
}, simplify=F)
r <- res[[1]]
i=2
r <- decode_string(dat[i])
r <- data.frame(ix = i, r)


head(res)
for (i in ) {
    
}
