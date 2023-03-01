
## 
## INTERNAL FUNCTION cs() for case sensitive text in cln()
## (CC BY-SA 4.0) Antonio Rivero Ostoic, multiplex@post.com 
##
## version 0.1.0 (01-03-2023)
##
## PARAMETERS
## xz       (list, vector, or data frame with characters)
## level    (clean level, 2 for Latin-Greek mixed text)
## case     (1 for 1st uppercase, 2 lower, 3 upper)
## na.rm    (logical, remove NAs?)


cs <-
function (xz, level = 1, case = 0, na.rm = FALSE) 
{
    ifelse(isTRUE(is.data.frame(xz) == TRUE) == TRUE, flgdf <- TRUE, 
        flgdf <- FALSE)
    if (isTRUE(flgdf == TRUE) == TRUE) {
        xzl <- xz
        if (isTRUE(case == 1L) == TRUE) {
            xzl[] <- sapply(xz, function(w) {
                s <- strsplit(w, " ")[[1]]
                paste(toupper(substring(s, 1, 1)), substring(s, 
                  2), sep = "", collapse = " ")
            })
            ifelse(isTRUE(level > 1) == TRUE, xzl[] <- lapply(xz, 
                function(z) {
                  gsub("\\b([[:lower:]])([[:lower:]]+)", "\\U\\1\\L\\2", 
                    z, perl = TRUE)
                }), NA)
        }
        else if (isTRUE(case == 2L) == TRUE) {
            xzl[] <- lapply(xz, tolower)
        }
        else if (isTRUE(case == 3L) == TRUE) {
            xzl[] <- lapply(xz, toupper)
        }
        xz <- as.data.frame(matrix(unlist(xzl), ncol = ncol(xz), 
            byrow = FALSE, dimnames = list(rownames(xz), colnames(xz))))
    }
    else {
        if (isTRUE(case == 1L) == TRUE) {
            xz <- sapply(xz, function(w) {
                s <- strsplit(w, " ")[[1]]
                paste(toupper(substring(s, 1, 1)), substring(s, 
                  2), sep = "", collapse = " ")
            })
        }
        else if (isTRUE(case == 2L) == TRUE) {
            xz <- lapply(xz, tolower)
        }
        else if (isTRUE(case == 3L) == TRUE) {
            xz <- lapply(xz, toupper)
        }
        xz <- unlist(xz, use.names = FALSE)
    }
    if (isTRUE(na.rm == TRUE) == TRUE) {
        ifelse(isTRUE(level > 1) == TRUE, xz[xz == "NA"] <- NA, 
            invisible(NA))
        ifelse(isTRUE(flgdf == TRUE) == TRUE, xz <- xz[!apply(xz, 
            1, function(z) all(is.na(z))), ], xz <- Filter(function(y) !all(is.na(y)), 
            xz))
    }
    else {
        ifelse(isTRUE(level > 0) == TRUE, xz[xz == ""] <- NA, 
            invisible(NA))
    }
    ifelse(isTRUE(flgdf == TRUE) == TRUE, return(xz), return(as.vector(xz)))
}
