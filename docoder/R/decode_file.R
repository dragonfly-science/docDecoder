decode_file <-
function(file, debug = FALSE, test = FALSE, summary = TRUE) {
    return(decode_string(readLines(file, warn=F), debug=debug, test=test, summary=summary))
}
