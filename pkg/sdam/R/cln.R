
## 
## FUNCTION cln() for cleansing and re-encoding glyphs and Greek characters
## (CC BY-SA 4.0) Antonio Rivero Ostoic, multiplex@post.com 
##
## version 0.5.8 (06-06-2023)
##
##
## PARAMETERS
##
## x        (scalar or vector, with character to clean)
## level    (clean level, 2 for Latin-Greek mixed text, 3 extra round)
##
## OPTIONAL PARAMETERS
##
## chr.rm   (vector of characters to remove)
## na.rm    (logical, remove NAs?)
## case     (1 for 1st uppercase, 2 lower, 3 upper)
## repl     (data frame with text for replacement)
##
## DEPENDS: clv() (cleansing vector)
##          cs() (if 'case')
##

cln <-
function (x, level = 1, chr.rm, na.rm, case, repl, space) 
{
    if (isTRUE(level == 0) == TRUE || isTRUE(length(x) == 0) == 
        TRUE) 
        return(x)
    ifelse(missing(chr.rm) == FALSE && isTRUE("" %in% chr.rm) == 
        TRUE, chr.rm <- chr.rm[-which(chr.rm == "")], NA)
    ifelse(missing(na.rm) == FALSE && isTRUE(na.rm == TRUE) == 
        TRUE, na.rm <- TRUE, na.rm <- FALSE)
    xo <- x
    if (isTRUE(is.data.frame(x) == TRUE) == TRUE || isTRUE(is.data.frame(x[[1]]) == 
        TRUE) == TRUE) {
        if (isTRUE(is.data.frame(x[[1]]) == TRUE) == TRUE) {
            warning("\"x\" is list of data frames and only first data frame is considered.")
            x <- as.data.frame(x[[1]])
        }
        else {
            invisible(NA)
        }
        if (isTRUE(nrow(x) == 0) == TRUE) 
            return(x)
        if (missing(chr.rm) == FALSE) {
            rnx <- rownames(x)
            for (w in seq_len(length(chr.rm))) {
                x <- as.data.frame(sapply(x, function(z) as.list(gsub(paste0("\\", 
                  chr.rm[w], sep = ""), "", z))), check.names = FALSE)
            }
            rm(w)
            rownames(x) <- rnx
        }
        x[is.null(x)] <- NA
        x[x == ""] <- NA
        xdf <- data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
        x <- as.list(sapply(xdf, as.character))
        ifelse(isTRUE(typeof(x) == "list") == TRUE, x1 <- unlist(x, 
            use.names = FALSE), x1 <- as.vector(x))
        resl <- vector("list", length = length(x1))
        for (k in seq_len(length(x1))) {
            xi <- as.character(x1[k])
            if (isTRUE(level == 3) == TRUE) {
                resl[[k]] <- clv(clv(xi, level = level, chr.rm = chr.rm, 
                  case = case, na.rm = na.rm, space = space))
            }
            else {
                resl[[k]] <- clv(xi, level = level, chr.rm = chr.rm, 
                  case = case, na.rm = na.rm, space = space)
            }
        }
        rm(k)
        resdf <- data.frame(matrix(unlist(resl), ncol = ncol(xdf), 
            byrow = FALSE, dimnames = list(rownames(xdf), colnames(xdf))), 
            check.names = FALSE, stringsAsFactors = FALSE)
        if (isTRUE(level > 0) == TRUE) {
            resdf <- data.frame(lapply(resdf, trimws), stringsAsFactors = FALSE, 
                check.names = FALSE)
            rownames(resdf) <- rownames(xdf)
        }
        else {
            NA
        }
        if (missing(repl) == FALSE) {
            if (is.vector(repl) == TRUE && is.data.frame(repl) == 
                FALSE) {
                resdf <- as.data.frame(mapply(gsub, repl[1], 
                  repl[2], resdf, USE.NAMES = FALSE), stringsAsFactors = FALSE)
            }
            else {
                for (i in seq_len(nrow(repl))) {
                  resdf <- as.data.frame(mapply(gsub, repl[i, 
                    1], repl[i, 2], resdf, USE.NAMES = FALSE), 
                    stringsAsFactors = FALSE)
                }
                rm(i)
            }
            rownames(resdf) <- rownames(xdf)
            colnames(resdf) <- colnames(xdf)
        }
        else {
            NA
        }
        if (missing(na.rm) == FALSE && isTRUE(na.rm == TRUE) == 
            TRUE) {
            is.na(resdf) <- resdf == "NULL"
            resdf[resdf == ""] <- NA
            if (isTRUE("id" %in% attr(resdf, "names")) == TRUE) {
                df <- resdf[, which(attr(resdf, "names") != "id")]
                if (isTRUE(length(which(rowSums(is.na(df)) == 
                  ncol(df))) == 0) == TRUE) {
                  return(resdf)
                }
                else {
                  return(resdf[-which(rowSums(is.na(df)) == ncol(df)), 
                    ])
                }
            }
            else {
                if (isTRUE(ncol(xo) == 1L) == TRUE) {
                  resdf1 <- as.data.frame(resdf[-which(rowSums(is.na(resdf)) == 
                    ncol(resdf)), ])
                  rownames(resdf1) <- names(which(rowSums(is.na(resdf)) != 
                    ncol(resdf)))
                  colnames(resdf1) <- colnames(resdf)
                  return(resdf1)
                }
                else {
                  return(resdf[-which(rowSums(is.na(resdf)) == 
                    ncol(resdf)), ])
                }
            }
        }
        else {
            return(resdf)
        }
    }
    else {
        x1 <- as.vector(x)
        if (missing(repl) == FALSE) {
            x1 <- suppressWarnings(mapply(gsub, repl[1], repl[2], 
                x1, USE.NAMES = FALSE))
        }
        else {
            NA
        }
        if (isTRUE(length(x) == 1L) == TRUE) {
            names(x1) <- xo
            if (isTRUE(level == 3) == TRUE) {
                res <- clv(clv(x1, level = level, chr.rm = chr.rm, 
                  case = case, na.rm = na.rm, space = space))
                names(res) <- xo
                return(res)
            }
            else {
                res <- clv(x1, level = level, chr.rm = chr.rm, 
                  case = case, na.rm = na.rm, space = space)
                names(res) <- xo
                return(res)
            }
        }
        else {
            resl <- vector("list", length = length(x1))
            for (k in seq_len(length(x1))) {
                xi <- as.character(x1[k])
                resl[[k]] <- clv(xi, level = level, chr.rm = chr.rm, 
                  case = case, na.rm = na.rm, space = space)
            }
            rm(k)
            if (isTRUE(length(x1[[1]]) == 1L) == FALSE) {
                res <- unlist(resl, use.names = FALSE)
            }
            else {
                for (k in seq_len(length(resl))) {
                  attr(resl[[k]], "names") <- NULL
                }
                rm(k)
                res <- resl
                attr(res, "names") <- attr(x, "names")
            }
            if (missing(na.rm) == FALSE && isTRUE(na.rm == TRUE) == 
                TRUE) {
                ifelse(isTRUE(is.list(xo) == FALSE) == TRUE, 
                  return(unlist(res[!is.na(res)])), return(res[!is.na(res)]))
            }
            else {
                ifelse(isTRUE(is.list(xo) == FALSE) == TRUE, 
                  return(unlist(res)), return(res))
            }
        }
    }
}
