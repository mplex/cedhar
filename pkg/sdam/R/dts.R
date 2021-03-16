
## 
## FUNCTION dts() to convert date formats into numerical
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.0.5 (16-03-2021)
##
## PARAMETERS
## x        (scalar or vector,  with character dates)
## cent     (optional and logiucal, centuries?)
## sep      (separator, works only for 'cent')


dts <-
function (x, cent, sep) 
{
    if (is.numeric(x) == TRUE) {
        x
    }
    else {
        if (isTRUE(length(x) > 1) == TRUE && is.na(suppressWarnings(as.numeric(x))) == 
            FALSE) 
            return(as.numeric(x))
        if (missing(cent) == FALSE && isTRUE(cent == TRUE) == 
            TRUE) {
            ifelse(missing(sep) == TRUE, sep <- " to ", NA)
            tmp <- sapply(x, function(z) {
                as.numeric(paste(strsplit(z, "")[[1]][which(!(strsplit(z, 
                  "")[[1]] %in% c("t", "h", " ", "C", "e", "n", 
                  "t", ".", "A", "D")))], collapse = ""))
            })
            vec <- as.vector(tmp)
            for (k in seq_len(length(vec))) {
                if (isTRUE(any(strsplit(names(tmp), "")[[k]] %in% 
                  c(".", "c")) == TRUE) == TRUE) {
                  vec[k] <- paste(head(seq(to = tmp[k] * 100L, 
                    from = ((tmp[k] - 1L) * 100L) + 1L), 1), 
                    tail(seq(to = tmp[k] * 100L, from = ((tmp[k] - 
                      1L) * 100L) + 1L), 1), sep = sep)
                }
                else {
                  NA
                }
            }
            rm(k)
            names(vec) <- names(tmp)
            return(vec)
        }
        else {
            if (any(c("E", "e") %in% strsplit(x, "")[[1]]) == 
                TRUE) {
                xn <- sub("e", "", sub("[.].*", "", sub("or.*", 
                  "", x, ignore.case = TRUE)), ignore.case = TRUE)
            }
            else {
                xn <- sub("or.*", "", x, ignore.case = TRUE)
            }
            pck <- which(gsub("[^-]", "", xn) %in% "-")
            ifelse(isTRUE(length(pck) > 0) == TRUE, xn[pck] <- paste0(gsub("-.*", 
                "\\1", xn[pck]), " ", gsub("([^A-Za-z])+", "", 
                gsub(".*-", "\\1", xn[pck]))), NA)
            xn2 <- toupper(sub("C", "", sub("[.].*", "", xn), 
                ignore.case = TRUE))
            xnd <- sapply(xn2, function(y) {
                tmp <- suppressWarnings(sapply(y, function(z) as.numeric(paste(strsplit(z, 
                  "")[[1]][which(!(strsplit(z, "")[[1]] %in% 
                  c("?", " ", "B")))], collapse = "")) * (-1L)))
                ifelse(isTRUE(suppressWarnings(as.numeric(y)) > 
                  0) == TRUE, tmp <- tmp * (-1L), NA)
                mmd <- vector(length = length(y))
                mmd[which(!(is.na(tmp)))] <- as.vector(tmp)[which(!(is.na(tmp)))]
                mmd[which(is.na(tmp))] <- suppressWarnings(sapply(y[which(is.na(tmp))], 
                  function(z) as.numeric(paste(strsplit(z, "")[[1]][which(!(strsplit(z, 
                    "")[[1]] %in% c("?", " ", "A", "D")))], collapse = "")) * 
                    (+1L)))
                names(mmd) <- NULL
                rm(tmp)
                unlist(mmd)
            })
            names(xnd) <- x
            return(xnd)
        }
    }
}
