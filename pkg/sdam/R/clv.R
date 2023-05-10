
## 
## INTERNAL FUNCTION clv() for cleansing vectors in cln()
## (CC BY-SA 4.0) Antonio Rivero Ostoic, multiplex@post.com 
##
## version 0.2.5 (10-05-2023)
##
## PARAMETERS
## x        (vector with text for cleansing)
## level    (clean level, 2 for Latin-Greek mixed text)
## case     (1 for 1st uppercase, 2 lower, 3 upper)
## chr.rm   (vector of characters to remove)
## na.rm    (logical, remove NAs?)
## space    (1 double to single space/space at end, 2 around -/=)


clv <-
function (x, level = 1, case, chr.rm, na.rm = FALSE, space) 
{
    xo <- x
    ifelse(is.factor(x) == TRUE, x <- as.vector(x), NA)
    ifelse(is.vector(x) == TRUE, flgvc <- TRUE, flgvc <- FALSE)
    ifelse(is.list(x) == TRUE, flgvl <- TRUE, flgvl <- FALSE)
    is.na(x) <- x == "NULL"
    x[x == ""] <- NA
    x[x == "NA"] <- NA
    if (missing(space) == FALSE && is.numeric(space) == TRUE) {
        if (isTRUE(space > 0) == TRUE) {
            x <- gsub("\\s+", " ", x)
            x <- gsub("\\s$", "", x)
            if (isTRUE(space > 1) == TRUE) {
                x <- gsub("-\\s", "-", x)
                x <- gsub("\\s-", "-", x)
                x <- gsub("/\\s", "/", x)
                x <- gsub("\\s/", "/", x)
                x <- gsub("=\\s", "=", x)
                x <- gsub("\\s=", "=", x)
            }
            else {
                NA
            }
        }
        else {
            NA
        }
    }
    xx1 <- strsplit(x, "")[[1]]
    if (isTRUE(level > 0) == TRUE && (isTRUE("<" %in% xx1) == 
        TRUE && isTRUE(">" %in% xx1) == TRUE)) {
        dbe <- c("\u0080", "\u0081", "\u0082", "\u0083", "\u0084", 
            "\u0085", "\u0086", "\u0087", "\u0088", "\u0089", 
            "\u008a", "\u008b", "\u008c", "\u008d", "\u008e", 
            "\u008f", "\u0090", "\u0091", "\u0092", "\u0093", 
            "\u0094", "\u0095", "\u0096", "\u0097", "\u0099", 
            "\u0099", "\u009a", "\u009b", "\u009c", "\u009d", 
            "\u009e", "\u009f")
        names(dbe) <- c("80", "81", "82", "83", "84", "85", "86", 
            "87", "88", "89", "8A", "8B", "8C", "8D", "8E", "8F", 
            "90", "91", "92", "93", "94", "95", "96", "97", "99", 
            "99", "9A", "9B", "9C", "9D", "9E", "9F")
        ck <- which(xx1 %in% "<")
        ck2 <- which(xx1 %in% ">")
        x0 <- vector()
        ifelse(isTRUE(ck[1] == 1) == TRUE, NA, x0 <- append(x0, 
            xx1[1:ck[1] - 1L]))
        for (i in seq_len(length(ck))) {
            if (isTRUE(ck2[i] - ck[i] < 3L) == TRUE || isTRUE(length(which(names(dbe) %in% 
                paste(xx1[(ck[i] + 5L):(ck[i] + 6L)], collapse = ""))) == 
                0) == TRUE) {
                warning(c("Couldn't resolve \"", paste(xx1[ck[i]:ck2[i]], 
                  collapse = ""), "\""), call. = FALSE)
                if (isTRUE(length(ck2) == i) == TRUE || isTRUE(length(x0) == 
                  tail(ck2, 1)) == TRUE) {
                  x0 <- append(x0, xx1[ck[i]:length(xx1)])
                  break
                }
                else {
                  x0 <- append(x0, xx1[ck[i]:ck2[i + 1]])
                }
            }
            else {
                x0 <- append(x0, as.vector(dbe[which(names(dbe) %in% 
                  paste(xx1[(ck[i] + 5L):(ck[i] + 6L)], collapse = ""))]))
                if (isTRUE(ck[i] == max(ck)) == TRUE) {
                  ifelse(isTRUE(tail(xx1, 1) == ">") == TRUE, 
                    NA, x0 <- append(x0, xx1[(ck[i] + 8L):length(xx1)]))
                }
                else {
                  x0 <- append(x0, xx1[(ck[i] + 8L):(ck[i + 1L] - 
                    1L)])
                }
            }
        }
        rm(i)
        x <- paste(x0, collapse = "")
    }
    if (isTRUE(is.na(as.vector(x)) == TRUE) == TRUE) 
        return(x)
    x1 <- as.vector(x)
    if (missing(chr.rm) == FALSE) {
        for (w in seq_len(length(chr.rm))) {
            x1 <- paste(strsplit(x1, "")[[1]][which(!(strsplit(x1, 
                "")[[1]] == chr.rm[w]))], collapse = "")
        }
        rm(w)
    }
    utix1 <- utf8ToInt(x1)
    ifelse(isTRUE(is.na(utix1) == TRUE) == TRUE, utix1 <- utf8ToInt(iconv(x1, 
        "", "UTF-8")), NA)
    if (isTRUE(any(utix1 > 255) == TRUE) == TRUE) {
        chk <- which(utix1 > 255)
        utix0 <- utix1
        utix0[chk - 1L] <- utix1[chk - 1L] + 1L
        utix1 <- utix0[-chk]
        flgc <- TRUE
    }
    else {
        flgc <- FALSE
    }
    gs1 <- which(as.raw(utix1) %in% c("e2", "e4", "f6", "fc"))
    gs2 <- which(as.raw(utix1) %in% c("cf", "ce"))
    gs2a <- which(as.raw(utix1) %in% c("c2", "c3", "c4", "c5", 
        "c8"))
    gs3 <- which(as.raw(utix1) %in% c("e1"))
    if (isTRUE(length(gs3) > 0) == TRUE) {
        invisible(NA)
    }
    else {
        gs3 <- NULL
    }
    gsx <- which(as.raw(utix1) %in% c("9f"))
    if (isTRUE(length(gsx) > 0) == TRUE) {
        gs1 <- gs1[-which(gs1 %in% (gsx - 1L))]
    }
    else {
        NA
    }
    if (isTRUE(length(c(gs1, gs2, gs2a, gs3)) == 0) == TRUE) {
        if (missing(case) == FALSE && is.numeric(case) == TRUE) {
            x1c <- cs(x1, level = level, case = case, na.rm = na.rm)
            names(x1c) <- xo
            return(x1c)
        }
        else {
            return(x1)
        }
    }
    if (isTRUE(length(gs1) == 0) == TRUE) {
        xx <- strsplit(rawToChar(as.raw(utix1)), "")[[1]]
    }
    else {
        if (isTRUE(length(gs1) == 1L) == TRUE) {
            xx <- c(xx1[1:(gs1 - 1)], xx1[gs1], xx1[(gs1 + 1):length(xx1)])
        }
        else {
            k <- 1
            xx <- c(xx1[1:(gs1[k] - 1)], xx1[gs1[k]], xx1[(gs1[k] + 
                1):(gs1[k + 1] - 1)])
            lxx <- length(xx)
            for (i in gs1) {
                if (isTRUE(length(xx) == length(xx1)) == TRUE) {
                  NA
                }
                else {
                  k <- k + 1L
                  if (isTRUE(k == length(gs1)) == TRUE) {
                    xx <- append(xx, c(xx1[gs1[k]], xx1[(gs1[k] + 
                      1):length(xx1)]))
                  }
                  else if (isTRUE(length(gs1) > k) == TRUE) {
                    xx <- append(xx, xx1[gs1[k]], xx1[(gs1[k] + 
                      1):(gs1[k + 1] - 1)])
                  }
                  else {
                    warning(c("Couldn't resolve \"", xx, "\""), 
                      call. = FALSE)
                  }
                }
            }
            rm(i)
        }
    }
    if (isTRUE(length(c(gs2, gs2a, gs3)) == 0) == TRUE | isTRUE(flgc == 
        FALSE) == TRUE) {
        res <- paste(xx, collapse = "")
        names(res) <- xo
        if (missing(case) == FALSE && is.numeric(case) == TRUE) {
            resc <- cs(res, level = level, case = case, na.rm = na.rm)
            names(resc) <- xo
            return(resc)
        }
        else {
            return(res)
        }
    }
    else {
        if (isTRUE(level == 2L) == TRUE) {
            xx0 <- xx1[1:min(c(gs2a, gs3)) - 1L]
            x2 <- paste(strsplit(x1, "")[[1]][length(xx0) + 1L:(nchar(x1) - 
                length(xx0))], collapse = "")
            xx2 <- try(strsplit(rawToChar(as.raw(utf8ToInt(x2))), 
                "")[[1]])
            res <- paste(c(xx0, xx2), collapse = "")
            names(res) <- xo
            return(res)
        }
        else {
            res <- paste(xx, collapse = "")
            names(res) <- xo
            return(res)
        }
    }
}
