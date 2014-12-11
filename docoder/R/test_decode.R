test_decode <-
function(file) {

    ## Decode data
    d <- decode_file(file)
    ## Read decoded data
    t <- read.csv(sub('\\.txt$', '.csv', file), as.is=T)
    ## Compare
    d$date1 <- format(d$datetime, '%Y/%m/%d_%H:00')
    t$date1 <- sprintf('%s/%02i/%02i_%02i:00', t$Year, t$Month, t$Day, t$Hour)
    m <- merge(d, t, all=T, by='date1')
    m$same <- m$count == m$Count
    allgood <- T
    if (nrow(d) != nrow(t))  {
        allgood <- F
        warning('Different number of rows! d: ', nrow(d), ' - t: ', nrow(t))
    }
    if (any(!m$same))  {
        allgood <- F
        warning('Cases not matching: ', sum(!m$same))
    }
    if (allgood) cat('\nDecoded data match those in csv file.\n')
}
