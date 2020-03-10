
## 
## FUNCTION simil() to compute similarity of entries in a given data frame
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.1 (10-03-2020)
##
## INPUT AND ARGUMENTS
## x  a data frame with an 'ID' column
## att (vector) column(s) in x representing attributes
## null (optional) include NA or NULLs?
## uniq (optional) remove duplicates?
## diag.incl (optional) include entries in diagonal?


simil <-
function (x, att, null, uniq, diag.incl) 
{
    ifelse(missing(null) == FALSE && isTRUE(null == TRUE) == 
        TRUE, null <- TRUE, null <- FALSE)
    ifelse(missing(uniq) == FALSE && isTRUE(uniq == FALSE) == 
        TRUE, uniq <- FALSE, uniq <- TRUE)
    ifelse(isTRUE(uniq == TRUE) == TRUE, x <- unique(x), NA)
    mat <- matrix(0L, nrow = nrow(x), ncol = nrow(x), dimnames = list(x$ID, 
        x$ID))
    for (at in att) {
        ccat <- unlist(unique(x[, at]))
        ifelse(isTRUE(null == TRUE) == TRUE, NA, ccat <- ccat[which(ccat != 
            "NULL")])
        for (i in seq_len(length(ccat))) {
            mat[which(x[, at] == ccat[i]), which(x[, at] == ccat[i])] <- mat[which(x[, 
                at] == ccat[i]), which(x[, at] == ccat[i])] + 
                1L
        }
        rm(i)
    }
    rm(at)
    ifelse(missing(diag.incl) == FALSE && isTRUE(diag.incl == 
        TRUE) == TRUE, NA, diag(mat) <- 0)
    return(mat)
}
