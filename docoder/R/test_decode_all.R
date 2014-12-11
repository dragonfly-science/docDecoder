test_decode_all <-
function() {
    testdir <- system.file('extdata', package='docoder')
    files <- dir(testdir, 'Ins.*\\.txt')
    for (file in files) {
        cat('\n\n====== ', file, ' ======\n\n')
        test_decode(paste0(testdir,'/', file))
    }
}
