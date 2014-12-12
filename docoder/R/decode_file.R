decode_file <-
function(file, debug = FALSE, test = TRUE, summary = TRUE) {
    return(decode_string(readLines(file, warn=F), debug=debug, test=test, summary=summary))
}
