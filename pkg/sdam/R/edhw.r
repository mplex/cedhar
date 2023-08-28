
## 
## FUNCTION edhw() to manipulate data API from the EDH dataset
## (CC BY-SA 4.0) Antonio Rivero Ostoic, multiplex@post.com 
##
## version 0.6.0 (28-08-2023)
##
## PARAMETERS
##
## vars   (vector, variables or attributes to be chosen from x)
## as     (output format: lists or data frames)
## type   (data frame type: long or wide or narrow)
##
## OPTIONAL PARAMETERS
##
## x        (list, typically fragments of EDH dataset or database API)
## split    (logical, divide the data into groups by id?)
## select   (vector, people variables to select)
## addID    (logical, add "HD id" to output?)
## limit    (integers, vector with nr records limit in output, offset supported)
## id       (integer or character, select only hd_nr records)
## na.rm    (logical, remove entries with missing data?)
## ldf      (experimental, is 'x' a list of data frames?)
## province (choose EDH provinces, names or abbreviations)
## gender   (choose EDH gender)
## rp       (list of Roman provinces complementing the 'rp' dataset)

edhw <- 
function (x = "EDH", vars, as = c("df", "list"), type = c("long", 
    "wide", "narrow"), split, select, addID, limit, id, na.rm, 
    ldf, province, gender, rp, ...) 
{
    if (isTRUE(match.arg(as) %in% c("df", "list")) == FALSE) 
        stop("'arg' should be one of \"df\", \"list\".")
    if (missing(addID) == FALSE && isTRUE(addID == FALSE) == 
        TRUE) {
        if ((isTRUE(na.rm == TRUE) == TRUE) | (match.arg(as) == 
            "df")) {
            message("\"addID\" is set to TRUE for \"na.rm\" option and data frame output.")
            addID <- TRUE
        }
        else {
            addID <- FALSE
        }
    }
    else if (missing(addID) == FALSE && isTRUE(addID == TRUE) == 
        TRUE) {
        addID <- TRUE
    }
    else {
        ifelse(match.arg(as) == "df", addID <- TRUE, addID <- FALSE)
    }
    ifelse(missing(na.rm) == FALSE && isTRUE(na.rm == TRUE) == 
        TRUE, na.rm <- TRUE, na.rm <- FALSE)
    flgdf <- FALSE
    if (is.null(x) == TRUE) 
        stop("'x' is NULL")
    if (isTRUE(x == "EDH") == TRUE) {
        message("\"x\" is for dataset \"EDH\".")
        flglv <- TRUE
        if (!(exists("EDH"))) {
            utils::data("EDH", package = "sdam", envir = environment())
            EDH <- get("EDH", envir = environment())
        }
        else {
            NA
        }
        x <- EDH
        class(x) <- NULL
        comment(x) <- NULL
    }
    else if (isTRUE(is.data.frame(x) == TRUE) == TRUE || isTRUE(is.data.frame(x[[1]]) == 
        TRUE) == TRUE) {
        flgdf <- TRUE
        if (isTRUE(is.data.frame(x[[1]]) == TRUE) == TRUE) {
            message("\"x\" is list of data frames.")
            x <- data.frame(lapply(do.call("rbind.data.frame", 
                x), as.character), stringsAsFactors = FALSE)
            rownames(x) <- NULL
        }
        else {
            ifelse(isTRUE(is.list(x) == TRUE) == TRUE, x <- as.data.frame(x), 
                NA)
        }
        if (match.arg(as) == "df") {
            if (missing(id) == FALSE) {
                if (missing(vars) == FALSE) {
                  ifelse(all(vars %in% colnames(x)) == TRUE, 
                    return(x[which(x$id %in% id), vars]), NA)
                }
                else {
                  return(x[which(x$id %in% id), ])
                }
            }
            else if (missing(limit) == FALSE) {
                NA
            }
        }
        else if (match.arg(as) == "list") {
            if (missing(id) == FALSE) {
                if (missing(vars) == FALSE) {
                  ifelse(all(vars %in% colnames(x)) == TRUE, 
                    return(as.list(x[which(x$id %in% id), vars])), 
                    NA)
                }
                else {
                  return(as.list(x[which(x$id %in% id), ]))
                }
            }
            else if (missing(limit) == FALSE) {
                if (missing(vars) == FALSE) {
                  ifelse(all(vars %in% colnames(x)) == TRUE, 
                    xll <- as.list(x[seq_len(limit), which(colnames(x) %in% 
                      c("id", vars))]), NA)
                }
                else {
                  xll <- as.list(x[seq_len(limit), ])
                }
                return(xll)
            }
        }
        if (missing(province) == FALSE) {
            if (missing(rp) == TRUE) {
                utils::data("rp", package = "sdam", envir = environment())
                rp <- get("rp", envir = environment())
            }
            else {
                NA
            }
            if (isTRUE(length(province) == 1) == TRUE) {
                if (isTRUE(province %in% names(rp)) == FALSE) {
                  if (isTRUE(province %in% rp) == FALSE) 
                    stop("\"province\" not found. Use \"rp\" argument with province names.")
                  province <- names(rp)[which(rp %in% province)]
                }
                else {
                  NA
                }
                xp <- x[x$province_label == unlist(rp[which(names(rp) == 
                  province)], use.names = FALSE), ]
                xp <- xp[which(!(is.na(xp$province_label))), 
                  ]
                ifelse(missing(vars) == TRUE, NA, xp <- xp[, 
                  -which(!(colnames(xp) %in% c(vars, "id")))])
            }
            else if (isTRUE(length(province) > 1) == TRUE) {
                message("Using several provinces as data frame is experimental.")
                xl <- vector("list")
                for (k in seq_len(length(province))) {
                  xp <- x[x$province_label == unlist(rp[which(names(rp) == 
                    province[k])], use.names = FALSE), ]
                  xp <- xp[which(!(is.na(xp$province_label))), 
                    ]
                  ifelse(missing(vars) == TRUE, NA, xp <- xp[, 
                    -which(!(colnames(xp) %in% c(vars, "id")))])
                  xl <- append(xl, xp)
                }
                rm(k)
                xp <- xl[order(unlist(lapply(xl, `[`, "id"), 
                  use.names = FALSE))]
            }
            else {
                NA
            }
        }
        else {
            xp <- x
        }
        ifelse(isTRUE(na.rm == FALSE) == TRUE, NA, xp <- xp[which(lapply(strsplit(rownames(xp), 
            "[.]"), function(x) {
            "NA" %in% x
        }) == FALSE), ])
        if (missing(gender) == FALSE) {
            xp <- xp[-which(is.na(xp$gender)), ]
            return(xp[-which(xp$gender != gender), ])
        }
        else {
            if (missing(province) == FALSE || missing(gender) == 
                FALSE) {
                if (match.arg(as) == "list") {
                  xpl <- apply(xp, 1, function(x) {
                    lapply(as.list(x), "as.vector")
                  })
                  names(xpl) <- NULL
                  return(xpl)
                }
                else {
                  return(xp)
                }
            }
            else {
                NA
            }
        }
    }
    else if (isTRUE(is.list(x) == TRUE) == TRUE) {
        if (is.list(x[[1]]) == TRUE) {
            ifelse(is.list(x[[1]][[1]]) == FALSE, flglv <- TRUE, 
                flglv <- FALSE)
            if (missing(ldf) == FALSE && isTRUE(ldf == TRUE) == 
                TRUE) {
                flglv <- FALSE
                x <- x[[1]]
                message("Only first component is taken for list of data frames.")
            }
            else {
                NA
            }
        }
        else {
            flglv <- FALSE
        }
    }
    else {
        ifelse(isTRUE(is.character(x) == TRUE) == TRUE, x <- eval(parse(text = x)), 
            NA)
    }
    if (missing(vars) == TRUE) {
        flgv <- FALSE
        ifelse(match.arg(as) == "df", flgp <- TRUE, flgp <- FALSE)
        if (match.arg(as) == "list" || (missing(province) == 
            FALSE)) {
            if (match.arg(as) == "df") 
                warning("\"province\" with no \"vars\" returns lists.")
            if (missing(id) == TRUE && missing(limit) == TRUE) {
                if (isTRUE(flgdf == TRUE) == TRUE) {
                  edhl <- list()
                  for (k in seq_len(dim(x)[1])) {
                    edhll <- vector("list", ncol(x))
                    attr(edhll, "names") <- colnames(x)
                    for (i in seq_len(ncol(x))) {
                      ifelse(isTRUE(length(x[[which(attr(x, "names") == 
                        colnames(x)[i])]][[k]]) == 0) == TRUE, 
                        edhll[i] <- NA, edhll[i] <- x[[which(attr(x, 
                          "names") == colnames(x)[i])]][[k]])
                    }
                    rm(i)
                    edhl[[k]] <- edhll
                  }
                  rm(k)
                  return(edhl)
                }
                else {
                  if (missing(province) == TRUE) {
                    return(x)
                  }
                  else if (missing(province) == FALSE) {
                    if (missing(rp) == TRUE) {
                      utils::data("rp", package = "sdam", envir = environment())
                      rp <- get("rp", envir = environment())
                    }
                    else {
                      NA
                    }
                    if (isTRUE(length(province) == 1) == TRUE) {
                      prvx <- x[which(sapply(lapply(x, `[`, c("id", 
                        "province_label")), tail, 1) == unlist(rp[which(names(rp) == 
                        province)], use.names = FALSE))]
                      ifelse(isTRUE(length(prvx) > 0) == TRUE, 
                        invisible(NA), prvx <- x[which(sapply(lapply(x, 
                          `[`, c("id", "province_label")), tail, 
                          1) == unlist(rp[which(rp == province)], 
                          use.names = FALSE))])
                    }
                    else if (isTRUE(length(province) > 1) == 
                      TRUE) {
                      xl <- vector("list")
                      for (k in seq_len(length(province))) {
                        prvx <- x[which(sapply(lapply(x, `[`, 
                          c("id", "province_label")), tail, 1) == 
                          unlist(rp[which(names(rp) == province[k])], 
                            use.names = FALSE))]
                        ifelse(isTRUE(length(prvx) > 0) == TRUE, 
                          invisible(NA), prvx <- x[which(sapply(lapply(x, 
                            `[`, c("id", "province_label")), 
                            tail, 1) == unlist(rp[which(rp == 
                            province[k])], use.names = FALSE))])
                        xl <- append(xl, prvx)
                      }
                      rm(k)
                      prvx <- xl[order(unlist(lapply(xl, `[`, 
                        "id"), use.names = FALSE))]
                      class(prvx) <- province
                    }
                    else {
                      NA
                    }
                    ifelse(isTRUE(length(prvx) == 0) == TRUE, 
                      return(NULL), return(prvx))
                  }
                }
            }
            else {
                if (missing(id) == FALSE) {
                  if (is.character(id) == TRUE && isTRUE(is.na(as.numeric(gsub("HD", 
                    "", id))) == TRUE) == TRUE) {
                    stop("Invalid \"id\".")
                  }
                  else {
                    ifelse(is.character(id) == TRUE, id <- as.numeric(gsub("HD", 
                      "", id)), NA)
                  }
                  ifelse(isTRUE(flgdf == TRUE) == TRUE, return(as.list(x[id])), 
                    NA)
                }
                else if (missing(id) == TRUE) {
                  NA
                }
            }
        }
        else if (match.arg(as) == "df") {
            if (missing(id) == FALSE && isTRUE(is.numeric(id) == 
                TRUE) == TRUE) {
                if (isTRUE(length(id) == 1) == TRUE) {
                  id <- paste0("HD", paste(rep(0, 6 - nchar(id)), 
                    collapse = ""), id, sep = "")
                }
                else {
                  tmp <- vector()
                  for (i in seq_len(length(id))) {
                    tmp <- append(tmp, paste0("HD", paste(rep(0, 
                      6 - nchar(id[i])), collapse = ""), id[i], 
                      sep = ""))
                  }
                  rm(i)
                  id <- tmp
                }
            }
            ifelse(missing(id) == FALSE && isTRUE(flgdf == TRUE) == 
                TRUE, x <- x[which(x$id %in% id), ], NA)
            ifelse(isTRUE(flgdf == TRUE) == TRUE || (missing(ldf) == 
                FALSE && isTRUE(ldf == TRUE) == TRUE), return(x), 
                vars <- unique(names(unlist(x))))
            ifelse(isTRUE("people" %in% vars) == TRUE || any(grepl("people.", 
                vars, fixed = TRUE)) == TRUE, flgp <- TRUE, flgp <- FALSE)
            xvars <- vars
        }
    }
    else if (missing(vars) == FALSE) {
        flgv <- TRUE
        if (is.character(vars) == FALSE) {
            if (isTRUE(isTRUE(is.list(vars) == TRUE) == TRUE) || 
                isTRUE(is.vector(vars) == FALSE) == TRUE) 
                stop("'vars' should be a vector or character.")
        }
        ifelse(isTRUE("id" %in% vars) == TRUE, vars <- vars[which(!(vars == 
            "id"))], NA)
        ifelse(isTRUE("people" %in% vars) == TRUE || any(grepl("people.", 
            vars, fixed = TRUE)) == TRUE, flgp <- TRUE, flgp <- FALSE)
        if (missing(province) == TRUE) {
            invisible(NA)
        }
        else if (missing(province) == FALSE) {
            if (missing(rp) == TRUE) {
                utils::data("rp", package = "sdam", envir = environment())
                rp <- get("rp", envir = environment())
            }
            else {
                NA
            }
            xpr <- x[which(sapply(lapply(x, `[`, c("id", "province_label")), 
                tail, 1) == unlist(rp[which(names(rp) == province)], 
                use.names = FALSE))]
            ifelse(isTRUE(length(xpr) > 0) == TRUE, invisible(NA), 
                xpr <- x[which(sapply(lapply(x, `[`, c("id", 
                  "province_label")), tail, 1) == unlist(rp[which(rp == 
                  province)], use.names = FALSE))])
            if (isTRUE(length(xpr) == 0) == TRUE) {
                invisible(NA)
            }
            else {
                if (match.arg(as) == "df" && (isTRUE("people" %in% 
                  vars) == TRUE || any(grepl("people.", vars, 
                  fixed = TRUE)) == TRUE)) {
                  warning("\"people\" is disregarded for data frames with \"province\".")
                  vars <- vars[which(!(vars == "people"))]
                }
                if (isTRUE(addID == TRUE) == TRUE) {
                  xprv <- lapply(xpr, `[`, c("id", vars))
                  xprv <- lapply(xprv, setNames, c("id", vars))
                  xprv <- lapply(xprv, function(x) {
                    x[sapply(x, is.null)] <- NA
                    return(x)
                  })
                }
                else {
                  xprv <- lapply(xpr, `[`, vars)
                  xprv <- lapply(xprv, setNames, vars)
                  xprv <- lapply(xprv, function(x) {
                    x[sapply(x, is.null)] <- NA
                    return(x)
                  })
                }
                rmv <- as.vector(which(lapply(lapply(xprv, `[`, 
                  "people"), is.na) == TRUE))
                xprv[rmv] <- lapply(xprv[rmv], `[`, which(!(c("id", 
                  vars) %in% "people")))
                if (match.arg(as) == "df" && (isTRUE("people" %in% 
                  vars) == TRUE || any(grepl("people.", vars, 
                  fixed = TRUE)) == TRUE)) {
                }
                else {
                  NA
                }
                if (match.arg(as) == "df") {
                  xprvdf <- do.call(rbind.data.frame, xprv)
                  attr(xprvdf, "class") <- c(attr(xprvdf, "class"), 
                    province)
                  return(cln(xprvdf))
                }
                else {
                  attr(xprv, "class") <- c(attr(xprv, "class"), 
                    province)
                  return(xprv)
                }
            }
        }
        if (isTRUE(flgdf == TRUE) == TRUE) {
            xvars <- colnames(x)
        }
        else {
            ifelse(is.null(names(x)) == TRUE || isTRUE(unique(names(x)) == 
                "") == TRUE, xvars <- unique(names(unlist(x))), 
                xvars <- unique(names(x)))
        }
        if (isTRUE(vars != "people") == TRUE && all(vars %in% 
            xvars) == FALSE) {
            warning(paste("Variable(s) \"", vars[which(!(vars %in% 
                xvars))], "\" is/are not present in \"x\" and may be disregarded.", 
                sep = ""))
            vars <- vars[which(vars %in% xvars)]
        }
        else if (all(vars %in% xvars) == FALSE && isTRUE(length(unlist(strsplit(xvars, 
            split = "people."))) > length(xvars)) == FALSE) {
            warning(paste("Variable(s) \"", vars[which(!(vars %in% 
                xvars))], "\" is/are not present in input data.", 
                sep = ""))
        }
        else if (all(vars %in% xvars) == FALSE) {
            if (isTRUE(length(vars[which(!(vars %in% xvars))][vars[which(!(vars %in% 
                xvars))] != "people"]) > 0) == TRUE) {
                warning(paste("Variable(s) \"", paste(vars[which(!(vars %in% 
                  xvars))][vars[which(!(vars %in% xvars))] != 
                  "people"], collapse = ", "), "\" is(are) not present in \"x\" and might been disregarded.", 
                  sep = ""))
            }
        }
        else {
            NA
        }
    }
    ifelse(isTRUE(typeof(x[[1]]) == "list") == FALSE && isTRUE(flgdf == 
        FALSE) == TRUE, x <- list(x), NA)
    if (isTRUE(flgdf == TRUE) == TRUE) {
        if (match.arg(as) == "df") {
            if (missing(id) == FALSE) {
                if (any(is.na(do.call(c, lapply(x$id, (function(x) {
                  if (is.null(x) | length(x) == 0) {
                    NA
                  } else {
                    x
                  }
                }))))) == TRUE) {
                  tmpx <- x[!(sapply(x$id, is.null)), ]
                  pck <- which(unlist(lapply(tmpx$id, function(x) {
                    (as.numeric(paste(strsplit(x, "")[[1]][3:8], 
                      collapse = "")))
                  })) %in% id)
                  return(tmpx[pck, ])
                }
                else {
                  pck <- which(unlist(lapply(x$id, function(x) {
                    (as.numeric(paste(strsplit(x, "")[[1]][3:8], 
                      collapse = "")))
                  })) %in% id)
                  return(x[pck, which(colnames(x) %in% c("id", 
                    vars))])
                }
            }
            else if (missing(limit) == FALSE) {
                if (isTRUE(flgv == TRUE) == TRUE) {
                  ifelse(isTRUE(length(limit) == 1L) == TRUE, 
                    return(head(x[which(colnames(x) %in% c("id", 
                      vars))], limit)), return(x[limit, which(colnames(x) %in% 
                      c("id", vars))]))
                }
                else {
                  ifelse(isTRUE(length(limit) == 1L) == TRUE, 
                    return(head(x, limit)), return(x[limit, ]))
                }
            }
            ifelse(isTRUE(flgv == TRUE) == TRUE, return(x[which(colnames(x) %in% 
                c("id", vars))]), return(x))
        }
        else if (match.arg(as) == "list") {
            edhl <- list()
            for (k in seq_len(dim(x)[1])) {
                edhll <- vector("list", length(vars))
                attr(edhll, "names") <- vars
                for (i in seq_len(length(vars))) {
                  ifelse(isTRUE(length(x[[which(attr(x, "names") == 
                    vars[i])]][[k]]) == 0) == TRUE, edhll[i] <- NA, 
                    edhll[i] <- x[[which(attr(x, "names") == 
                      vars[i])]][[k]])
                }
                rm(i)
                edhl[[k]] <- edhll
            }
            rm(k)
            rm(edhll)
        }
    }
    if (isTRUE(flgdf == FALSE) == TRUE) {
        if (missing(id) == FALSE) {
            if (is.character(id) == TRUE && isTRUE(is.na(as.numeric(gsub("HD", 
                "", id))) == TRUE) == TRUE) {
                stop("Invalid \"id\".")
            }
            else {
                ifelse(is.character(id) == TRUE, id <- as.numeric(gsub("HD", 
                  "", id)), NA)
            }
            xn <- unique(x)
            xn[sapply(lapply(xn, function(x) {
                x$id
            }), is.null)] <- NULL
            edhlm <- list()
            for (i in id) {
                if (isTRUE(length(which(as.vector(unlist(lapply(xn, 
                  `[`, "id"))) == sprintf("HD%06d", as.numeric(i)))) > 
                  0) == TRUE) {
                  if ((xn[which(as.vector(unlist(lapply(xn, `[`, 
                    "id"))) == sprintf("HD%06d", as.numeric(i)))][[1]]$id == 
                    sprintf("HD%06d", as.numeric(i))) == FALSE) {
                    edhlm[length(edhlm) + 1L] <- xn[i]
                  }
                  else {
                    edhlm[length(edhlm) + 1L] <- xn[as.numeric(which(unlist(lapply(xn, 
                      `[`, "id")) == sprintf("HD%06d", as.numeric(i))))][1]
                  }
                }
                else {
                  ifelse(isTRUE(length(id) == 1L) == TRUE, return(NULL), 
                    NA)
                }
            }
            rm(i)
        }
        else if (missing(id) == TRUE) {
            if (missing(limit) == TRUE || (missing(limit) == 
                FALSE && isTRUE(length(x) < limit) == TRUE)) {
                edhlm <- x
            }
            else if (missing(limit) == FALSE) {
                if (isTRUE(length(limit) == 1L) == TRUE) {
                  edhlm <- x[seq_len(limit)]
                }
                else {
                  edhlm <- x[limit]
                }
            }
        }
        if (isTRUE(flgv == TRUE) == TRUE && isTRUE(is.vector(vars) == 
            TRUE) == TRUE) {
            edhl <- lapply(edhlm, `[`, vars)
            edhl <- lapply(edhl, setNames, vars)
            edhl <- lapply(edhl, function(x) {
                x[sapply(x, is.null)] <- NA
                return(x)
            })
            if (is.null(unlist(edhl)) == TRUE) {
                warning("\"vars\" not in \"x\".")
                return(NULL)
            }
            else {
                ifelse(isTRUE(na.rm == TRUE) == TRUE, valids <- which(as.vector(unlist(lapply(edhl, 
                  function(x) {
                    all(is.na(as.vector(unlist(x))))
                  }))) == FALSE), valids <- seq_len(length(edhl)))
                if (isTRUE(na.rm == TRUE) == TRUE) {
                  edhrm <- lapply(lapply(lapply(edhl, names), 
                    is.na), all)
                  if (isTRUE(length(which(unlist(edhrm) == TRUE)) > 
                    0) == TRUE) {
                    edhlm <- edhlm[-which(unlist(edhrm) == TRUE)]
                    edhl <- edhl[-which(unlist(edhrm) == TRUE)]
                  }
                  else {
                    NA
                  }
                }
                else {
                  NA
                }
            }
        }
        else if (isTRUE(flgv == FALSE) == TRUE) {
            edhl <- edhlm
            valids <- which(as.vector(unlist(lapply(edhl, function(x) {
                all(is.na(as.vector(unlist(x))))
            }))) == FALSE)
        }
        else {
            stop("Argument 'vars' should be a vector.")
        }
    }
    else {
        NA
    }
    if (match.arg(as) == "df") {
        ifelse(missing(split) == FALSE && isTRUE(split == TRUE) == 
            TRUE, split <- TRUE, split <- FALSE)
        if (isTRUE(flgp == TRUE) == TRUE) {
            pnames <- lapply(edhl, "names")
            if (isTRUE(flgv == FALSE) == TRUE) {
                vars <- unique(names(unlist(x)))
            }
            else {
                NA
            }
            if (all(is.na(edhl)) == FALSE) {
                if (isTRUE(vars == "people") == TRUE) {
                  edhlp <- edhl[which(!(is.na(unlist(pnames))))]
                  if (isTRUE(length(edhlp) > 0) == TRUE) {
                    plbs <- unique(attr(unlist(edhlp), "names"))
                    plbs <- sort(unique(unlist(strsplit(plbs, 
                      split = "people."))))
                    plbs <- plbs[which(plbs != "people")]
                    ifelse(isTRUE(addID == TRUE) == TRUE, plbs[1] <- "id", 
                      plbs <- plbs[2:length(plbs)])
                  }
                  else {
                    return(NULL)
                  }
                }
                else {
                  edhlp <- lapply(edhlm, `[`, "people")
                  edhlp <- lapply(edhlp, setNames, "people")
                  edhlp <- lapply(edhlp, function(x) {
                    x[sapply(x, is.null)] <- NA
                    return(x)
                  })
                  npvars <- vars[!(vars %in% "people")]
                  ifelse(isTRUE(flgp == TRUE) == TRUE && isTRUE(vars == 
                    "people") == FALSE, vars <- c("people", npvars), 
                    vars <- npvars)
                  ifelse(isTRUE(flgv == FALSE) == TRUE, npvars <- npvars[which(grepl("people.", 
                    npvars, fixed = TRUE) == FALSE)], NA)
                  if (isTRUE(flgv == TRUE) == TRUE) {
                    edhlq <- lapply(edhlm, `[`, sort(vars[which(vars != 
                      "people")]))
                    edhlq <- lapply(edhlq, setNames, sort(vars[which(vars != 
                      "people")]))
                  }
                  else {
                    edhlq <- lapply(edhlm, `[`, npvars)
                    edhlq <- lapply(edhlq, setNames, npvars)
                  }
                  edhlq <- lapply(edhlq, function(x) {
                    x[sapply(x, is.null)] <- NA
                    return(x)
                  })
                  if (is.null(unlist(edhlq)) == FALSE) {
                    if (isTRUE(length(unique(names(unlist(edhlq)))) != 
                      length(npvars)) == FALSE || any(is.na(unique(unlist(lapply(edhlq, 
                      "names"))))) == TRUE) {
                      for (k in seq_len(length(edhlq))) {
                        ifelse(any(is.na(names(edhlq[[k]]))) == 
                          FALSE, NA, names(edhlq[[k]])[which(is.na(names(edhlq[[k]])))] <- npvars[which(!(npvars %in% 
                          names(edhlq[[k]])))])
                      }
                      rm(k)
                    }
                    else {
                      NA
                    }
                  }
                  if (isTRUE(length(edhlp) > 0) == TRUE && is.null(unlist(edhlp)) == 
                    FALSE) {
                    plbs <- unique(attr(unlist(edhlp), "names"))
                    plbs <- sort(unique(unlist(strsplit(plbs, 
                      split = "people."))))
                    ifelse(isTRUE(addID == TRUE) == TRUE, plbs[1] <- "id", 
                      plbs <- plbs[2:length(plbs)])
                  }
                  else {
                    plbs <- vector()
                  }
                  if (isTRUE(length(edhlq) > 0) == TRUE && is.null(unlist(edhlq)) == 
                    FALSE) {
                    qlbs <- sort(unique(unlist(lapply(edhlq, 
                      "names"))), na.last = TRUE)
                    ifelse(isTRUE(addID == TRUE) == TRUE, qlbs <- append("id", 
                      qlbs), NA)
                  }
                  else {
                    qlbs <- vector()
                  }
                }
            }
            else {
                return(as.data.frame(edhl))
            }
            ids <- lapply(edhlm, function(x) {
                x$id
            })
            ids <- ids[which(is.na(pnames) == FALSE)]
            if (isTRUE(is.na(unlist(edhlp, use.names = FALSE))) == 
                TRUE) {
                edhlp <- Map(c, id = ids, edhlp)
            }
            else {
                NA
            }
            if (match.arg(type) == "long") {
                if (isTRUE(length(edhlp) > 0) == TRUE && is.null(unlist(edhlp)) == 
                  FALSE) {
                  if (isTRUE(length(plbs) == 1L) == TRUE) {
                    xdfp <- data.frame(matrix(nrow = length(ids), 
                      ncol = length(select) + 1L, dimnames = list(NULL, 
                        c("id", select))))
                    xdfp[, 1] <- unlist(ids)
                  }
                  else {
                    xdfp <- data.frame(matrix(ncol = length(plbs), 
                      nrow = 0))
                    colnames(xdfp) <- plbs
                    xdfp0 <- suppressWarnings(lapply(edhlp, function(w) {
                      if (is.null(unlist(w)) == FALSE) {
                        tmpdf <- data.frame(matrix(ncol = length(plbs), 
                          nrow = 0))
                        colnames(tmpdf) <- plbs
                        for (i in seq_len(length(w$people))) {
                          w$people[[i]] <- w$people[[i]][order(names(w$people[[i]]))]
                          w$people[[i]][lengths(w$people[[i]]) == 
                            0L] <- NA
                          tmpdf[i, which((plbs %in% attr(w$people[[i]], 
                            "names")))] <- as.vector(unlist(w$people[[i]]))
                        }
                        rm(i)
                        xdfp <- rbind(xdfp, tmpdf)
                      }
                      else {
                        NA
                      }
                      return(xdfp)
                    }))
                    if (isTRUE(addID == TRUE) == TRUE) {
                      for (k in seq_len(length(xdfp0))) {
                        ifelse(isTRUE(nrow(xdfp0[[k]]) == 0) == 
                          TRUE, NA, xdfp0[[k]]$id <- ids[k])
                        ifelse(isTRUE(nrow(xdfp0[[k]]) == 0) == 
                          FALSE, NA, xdfp0[[k]] <- c(ids[which(!(unlist(ids) %in% 
                          unlist(lapply(xdfp0, `[`, "id"))))][k][[1]], 
                          rep(NA, (ncol(xdfp0[[k]]) - 1L))))
                      }
                      rm(k)
                    }
                    xdfp <- suppressWarnings(do.call("rbind", 
                      xdfp0))
                  }
                  ifelse(isTRUE(addID == TRUE) == TRUE && isTRUE(length(unique(ids)) == 
                    1) == TRUE, xdfp$id <- ids[valids], NA)
                  if (missing(select) == FALSE) {
                    xdfp <- subset(xdfp, select = c("id", select[which(select %in% 
                      plbs)]))
                  }
                  else if (all(is.na(xdfp$people)) == TRUE) {
                    if (isTRUE(vars != "people") == TRUE) {
                      people <- NULL
                      xdfp <- subset(xdfp, select = -c(people))
                    }
                    else {
                      NA
                    }
                  }
                }
                else {
                  if (isTRUE(vars == "people") == TRUE) {
                    return(NULL)
                  }
                  else {
                    NA
                  }
                }
                if (isTRUE(vars == "people") == TRUE) {
                  if (isTRUE(na.rm == TRUE) == TRUE) {
                    if (isTRUE(nrow(xdfp) != 0) == TRUE && isTRUE(addID == 
                      TRUE) == TRUE) {
                      xdfp <- xdfp[which(apply(xdfp[2:ncol(xdfp)], 
                        1, function(x) {
                          all(is.na(x))
                        }) == FALSE), ]
                    }
                    else {
                      return(NULL)
                    }
                  }
                  else {
                    NA
                  }
                  if (isTRUE(split == TRUE) == TRUE) {
                    return(split(xdfp[, -1], xdfp$id))
                  }
                  else {
                    ifelse(isTRUE(any(is.na(unlist(xdfp$id)))) == 
                      TRUE, return(xdfp[-which(is.na(unlist(xdfp$id, 
                      use.names = FALSE))), ]), return(xdfp))
                  }
                }
                else {
                  if (isTRUE(length(edhlq) > 0) == TRUE) {
                    if (isTRUE(length(valids) == 1) == TRUE) {
                      edhlq <- as.list(unlist(edhlq[valids]))
                      xdfq <- data.frame(matrix(unlist(edhlq), 
                        ncol = length(edhlq), byrow = TRUE))
                      colnames(xdfq) <- names(edhlq)
                    }
                    else {
                      edhlq <- edhlq[valids]
                      xdfq <- as.data.frame(do.call(rbind, lapply(edhlq, 
                        `length<-`, max(lengths(edhlq)))), stringsAsFactors = TRUE)
                    }
                    if (isTRUE(addID == TRUE) == TRUE) {
                      xdfq$id <- ids[valids]
                      ifelse(isTRUE(flgv == TRUE) == TRUE, xdfq <- rev(xdfq), 
                        NA)
                    }
                    else {
                      NA
                    }
                  }
                  else {
                    ifelse(isTRUE("people" %in% vars == FALSE) == 
                      TRUE, return(NULL), NA)
                  }
                  if (isTRUE(length(edhlq) > 0) == TRUE && (isTRUE(length(edhlp) > 
                    0) == TRUE && is.null(unlist(edhlp)) == FALSE)) {
                    tmpxdfp <- xdfp
                    ifelse(isTRUE(!("id" %in% colnames(xdfp))) == 
                      TRUE, tmpxdfp$id <- seq_len(nrow(xdfp)), 
                      tmpxdfp$id <- sub("[[:alpha:]]+", "", xdfp$id))
                    tmpxdfq <- xdfq
                    ifelse(isTRUE(!("id" %in% colnames(xdfq))) == 
                      TRUE, tmpxdfq$id <- seq_len(nrow(xdfq)), 
                      tmpxdfq$id <- sub("[[:alpha:]]+", "", xdfq$id))
                    xdfpq <- merge(tmpxdfp, tmpxdfq, by = "id")
                    ifelse(isTRUE(!("id" %in% colnames(xdfp))) == 
                      TRUE || isTRUE(!("id" %in% colnames(xdfq))) == 
                      TRUE, NA, xdfpq$id <- paste("HD", xdfpq$id, 
                      sep = ""))
                    ifelse(isTRUE("people" %in% colnames(xdfpq)) == 
                      TRUE, xdfpq <- subset(xdfpq, select = -c(people)), 
                      NA)
                    if (isTRUE(na.rm == TRUE) == TRUE) {
                      if (isTRUE(nrow(xdfpq) != 0) == TRUE && 
                        isTRUE(addID == TRUE) == TRUE) {
                        xdfpq <- xdfpq[which(apply(xdfpq[2:ncol(xdfpq)], 
                          1, function(x) {
                            all(is.na(x))
                          }) == FALSE), ]
                      }
                      else {
                        return(NULL)
                      }
                    }
                    else {
                      NA
                    }
                    if (isTRUE(na.rm == TRUE) == TRUE) {
                      if (isTRUE(nrow(xdfq) != 0) == TRUE && 
                        isTRUE(addID == TRUE) == TRUE) {
                        xdfq <- xdfq[which(apply(xdfq[2:ncol(xdfq)], 
                          1, function(x) {
                            all(is.na(x))
                          }) == FALSE), ]
                      }
                      else {
                        return(NULL)
                      }
                    }
                    else {
                      NA
                    }
                    if (isTRUE(split == TRUE) == TRUE) {
                      return(split(xdfpq[, -1], xdfpq$id))
                    }
                    else {
                      return(xdfpq)
                    }
                  }
                  else {
                    if (isTRUE(length(plbs) > 0) == TRUE) {
                      if (isTRUE(split == TRUE) == TRUE) {
                        return(split(xdfp[, -1], xdfp$id))
                      }
                      else {
                        return(xdfp)
                      }
                    }
                    else if (isTRUE(length(qlbs) > 0) == TRUE) {
                      if (isTRUE(split == TRUE) == TRUE) {
                        return(split(xdfq[, -1], xdfq$id))
                      }
                      else {
                        return(xdfq)
                      }
                    }
                    else {
                      return(NULL)
                    }
                  }
                }
            }
            else if (match.arg(type) == "wide") {
                ifelse(isTRUE(flgp == FALSE) == TRUE, NA, edhlp <- edhlp[valids])
                ifelse(isTRUE(vars == "people") == TRUE, NA, 
                  edhlq <- edhlq[valids])
                ifelse(isTRUE(addID == FALSE) == TRUE, NA, ids <- ids[valids])
                if (isTRUE(length(edhlp) > 0) == TRUE && is.null(unlist(edhlp)) == 
                  FALSE) {
                  pp <- max(as.numeric(unlist(edhlm)[which(attr(unlist(edhlm), 
                    "names") == "people.person_id")]))
                  plbss <- vector()
                  for (i in seq_len(pp)) {
                    plbss <- append(plbss, paste(plbs[2:length(plbs)], 
                      i, sep = ""))
                  }
                  rm(i)
                  ifelse(isTRUE(addID == TRUE) == TRUE, plbss <- append("id", 
                    plbss), NA)
                  options(stringsAsFactors = FALSE)
                  xdfpp <- data.frame(matrix(ncol = length(plbss), 
                    nrow = 0))
                  for (k in seq_len(length(edhlp))) {
                    if (isTRUE(is.na(edhlp[[k]]$people) == TRUE) == 
                      TRUE) {
                      NA
                    }
                    else {
                      tmpdf <- data.frame(matrix(ncol = length(plbs), 
                        nrow = 0))
                      colnames(tmpdf) <- plbs
                      for (i in seq_len(length(edhlp[[k]]$people))) {
                        edhlp[[k]]$people[[i]] <- edhlp[[k]]$people[[i]][order(names(edhlp[[k]]$people[[i]]))]
                        edhlp[[k]]$people[[i]][lengths(edhlp[[k]]$people[[i]]) == 
                          0L] <- NA
                        tmpdf[i, which((plbs %in% attr(edhlp[[k]]$people[[i]], 
                          "names")))] <- as.vector(unlist(edhlp[[k]]$people[[i]]))
                      }
                      rm(i)
                      if (isTRUE(nrow(tmpdf) > 1L) == TRUE) {
                        vecp <- vector()
                        for (i in seq_len(nrow(tmpdf))) {
                          vecp <- append(vecp, unlist(tmpdf[i, 
                            2:ncol(tmpdf)], use.names = FALSE))
                        }
                        rm(i)
                        ifelse(isTRUE(dim(tmpdf)[1] == pp) == 
                          TRUE, vecpp <- c(as.vector(unlist(ids))[k], 
                          vecp), vecpp <- c(as.vector(unlist(ids))[k], 
                          vecp, rep(NA, (length(plbss) - length(vecp) - 
                            1L))))
                        xdfpp <- (rbind(xdfpp, vecpp))
                      }
                      else if (isTRUE(nrow(tmpdf) == 1L) == TRUE) {
                        if (isTRUE(dim(xdfpp)[1] == 0) == TRUE) {
                          xdfpp <- rbind(as.vector(unlist(xdfpp)), 
                            c(as.vector(unlist(ids))[k], as.vector(unlist(tmpdf[2:ncol(tmpdf)])), 
                              rep(NA, (length(plbss) - ncol(tmpdf)))))
                        }
                        else {
                          xdfpp <- (rbind(xdfpp, c(as.vector(unlist(ids))[k], 
                            as.vector(unlist(tmpdf[2:ncol(tmpdf)])), 
                            rep(NA, (length(plbss) - ncol(tmpdf))))))
                        }
                      }
                    }
                  }
                  rm(k)
                  xdfpp <- as.data.frame(xdfpp)
                  colnames(xdfpp) <- plbss
                  rownames(xdfpp) <- NULL
                  if (missing(select) == FALSE) {
                    colnames(xdfpp) <- c("id", sub("[[:digit:]]+$", 
                      "", plbss[2:length(plbss)]))
                    xdfpp <- xdfpp[, c(1, which(colnames(xdfpp) %in% 
                      select))]
                  }
                  else {
                    NA
                  }
                }
                else {
                  ifelse(isTRUE(vars == "people") == TRUE, return(NULL), 
                    NA)
                }
                if (isTRUE(vars == "people") == TRUE) {
                  if (isTRUE(split == TRUE) == TRUE) {
                    return(split(xdfpp[, -1], xdfpp$id))
                  }
                  else {
                    return(xdfpp)
                  }
                }
                else {
                  if (isTRUE(length(edhlq) > 0) == TRUE) {
                    xdfq <- data.frame(matrix(ncol = length(as.vector(stats::na.omit(qlbs))), 
                      nrow = length(edhlq)))
                    colnames(xdfq) <- as.vector(stats::na.omit(qlbs))
                    for (i in seq_len(length(edhlq))) {
                      edhlq[[i]] <- edhlq[[i]][order(names(edhlq[[i]]))]
                      edhlq[[i]][lengths(edhlq[[i]]) == 0L] <- NA
                      xdfq[i, 2:ncol(xdfq)] <- as.vector(unlist(edhlq[[i]]))
                    }
                    rm(i)
                    ifelse(isTRUE(addID == TRUE) == TRUE, xdfq[, 
                      1] <- as.vector(unlist(ids)), NA)
                  }
                  if (isTRUE(length(plbs) > 0) == TRUE && isTRUE(length(qlbs) > 
                    0) == TRUE) {
                    tmpxdfpp <- xdfpp
                    tmpxdfpp$id <- sub("[[:alpha:]]+", "", xdfpp$id)
                    tmpxdfq <- xdfq
                    tmpxdfq$id <- sub("[[:alpha:]]+", "", xdfq$id)
                    xdfpq <- merge(tmpxdfpp, tmpxdfq, all = TRUE)
                    xdfpq$id <- paste("HD", xdfpq$id, sep = "")
                    if (isTRUE(split == TRUE) == TRUE) {
                      return(split(xdfpq[, -1], xdfpq$id))
                    }
                    else {
                      return(xdfpq)
                    }
                  }
                  else if (isTRUE(length(plbs) > 0) == TRUE) {
                    if (isTRUE(split == TRUE) == TRUE) {
                      return(split(xdfpp[, -1], xdfpp$id))
                    }
                    else {
                      return(xdfpp)
                    }
                  }
                  else if (isTRUE(length(qlbs) > 0) == TRUE) {
                    if (isTRUE(split == TRUE) == TRUE) {
                      return(split(xdfq[, -1], xdfq$id))
                    }
                    else {
                      return(xdfq)
                    }
                  }
                  else {
                    print("w")
                  }
                }
            }
            else if (match.arg(type) == "narrow") {
                print("Argument 'type' *narrow* is not yet implemented.")
            }
        }
        if (isTRUE(flgp == FALSE) == TRUE) {
            edhlq <- lapply(edhlm, `[`, sort(vars[which(vars != 
                "people")]))
            edhlq <- lapply(edhlq, setNames, sort(vars[which(vars != 
                "people")]))
            ifelse(lapply(edhlq, function(z) {
                length(z[sapply(z, is.null)])
            })[[1]] > 0, flgn <- TRUE, flgn <- FALSE)
            if (isTRUE(flgn == FALSE) == TRUE) {
                edhlq <- lapply(edhlq, function(x) {
                  x[sapply(x, is.null)] <- NA
                  return(x)
                })
            }
            else {
                NA
            }
            for (k in seq_len(length(edhlq))) {
                ifelse(any(is.na(names(edhlq[[k]]))) == FALSE, 
                  NA, names(edhlq[[k]])[which(is.na(names(edhlq[[k]])))] <- vars[which(!(vars %in% 
                    names(edhlq[[k]])))])
            }
            rm(k)
            if (isTRUE(length(edhlq) > 0) == TRUE) {
                if (isTRUE(flgn == TRUE) == TRUE) {
                  if (isTRUE(length(edhlq) == 1) == TRUE && isTRUE(length(edhlq[[1]]) == 
                    2) == TRUE) {
                    tmpq <- lapply(edhlq, function(z) {
                      (z[!(sapply(z, is.null))])
                    })
                    xdfq <- data.frame(matrix(unlist(tmpq), ncol = max(length(tmpq)), 
                      dimnames = list(NULL, names(tmpq[[1]]))))
                  }
                  else {
                    xdfq <- as.data.frame(do.call(rbind, lapply(edhlq, 
                      `length<-`, max(lengths(edhlq)))))
                  }
                }
                else {
                  ifelse(isTRUE(flglv == TRUE) == FALSE, xdfq <- data.frame(matrix(unlist(edhlq), 
                    ncol = max(lengths(edhlq)), byrow = FALSE)), 
                    xdfq <- data.frame(matrix(unlist(edhlq), 
                      ncol = max(lengths(edhlq)), byrow = TRUE)))
                  qlbs <- sort(unique(unlist(lapply(edhlq, "names"))), 
                    na.last = TRUE)
                  colnames(xdfq) <- as.vector(stats::na.omit(qlbs))
                }
                xdfq <- cln(xdfq)
                if (isTRUE(addID == TRUE) == TRUE) {
                  ids <- lapply(edhlm, function(x) {
                    x$id
                  })
                  ids[sapply(ids, is.null)] <- NA
                  xdfq$id <- as.vector(unlist(ids))
                  ifelse(isTRUE(flgv == TRUE) == TRUE, xdfq <- rev(xdfq), 
                    NA)
                }
                else {
                  NA
                }
            }
            else {
                return(NULL)
            }
            if (isTRUE(split == TRUE) == TRUE) {
                return(split(xdfq[, -1], xdfq$id))
            }
            else {
                return(xdfq)
            }
        }
    }
    else if (match.arg(as) == "list") {
        if (isTRUE(flgp == TRUE) == TRUE && isTRUE(flgv == TRUE) == 
            TRUE) {
            if (missing(select) == FALSE) {
                edhlp <- lapply(edhlm, `[`, "people")
                if (isTRUE(na.rm == TRUE) == TRUE) {
                  ids <- ids[which(as.vector(unlist(lapply(edhlp, 
                    function(x) all(is.na(x))))) == FALSE)]
                  edhlp0 <- Filter(function(x) !all(is.na(x)), 
                    edhlp)
                }
                else {
                  edhlp0 <- edhlp
                }
                edhl1 <- vector("list", length = length(edhlp0))
                for (k in seq_len(length(edhlp0))) {
                  tmpsl <- lapply(edhlp0[[k]]$people, `[`, select)
                  for (j in seq_len(length(tmpsl))) {
                    ifelse(any(is.na(names(tmpsl[[j]]))) == FALSE, 
                      NA, names(tmpsl[[j]])[which(is.na(names(tmpsl[[j]])))] <- select[which(!(select %in% 
                        names(tmpsl[[j]])))])
                  }
                  rm(j)
                  tmpsl <- lapply(tmpsl, function(x) {
                    x[sapply(x, is.null)] <- NA
                    return(x)
                  })
                  edhl1[[k]] <- tmpsl
                }
                rm(k)
            }
            else {
                edhl1 <- lapply(edhlm, `[`, "people")
            }
            if (isTRUE(vars == "people") == TRUE) {
                edhl2 <- Map(c, people = edhl1)
            }
            else if (isTRUE("people" %in% vars) == TRUE) {
                edhlq <- lapply(edhlm, `[`, sort(vars[which(vars != 
                  "people")]))
                edhlq <- lapply(edhlq, setNames, sort(vars[which(vars != 
                  "people")]))
                ifelse(isTRUE(na.rm == TRUE) == TRUE, edhlq <- Filter(function(x) !all(is.na(x)), 
                  edhlq), NA)
                edhl2 <- suppressWarnings(Map(c, people = edhl1, 
                  edhlq))
            }
            else {
                edhl2 <- lapply(edhlm, `[`, sort(vars[which(vars != 
                  "people")]))
                edhl2 <- lapply(edhl2, setNames, sort(vars[which(vars != 
                  "people")]))
                ifelse(isTRUE(na.rm == TRUE) == TRUE, edhl2 <- Filter(function(x) !all(is.na(x)), 
                  edhl2), NA)
            }
            edhl2 <- lapply(edhl2, function(x) {
                x[sapply(x, is.null)] <- NA
                return(x)
            })
        }
        if (isTRUE(addID == TRUE) == TRUE) {
            ids <- lapply(edhlm, function(x) {
                x$id
            })
            ifelse(isTRUE(flgp == FALSE) == TRUE, return(Map(c, 
                id = ids, edhl)), return(Map(c, id = ids, edhl2)))
        }
        else {
            if (isTRUE(flgp == FALSE) == TRUE) {
                ifelse(isTRUE(length(edhl) == 1) == TRUE, return(edhl[[1]]), 
                  return(edhl))
            }
            else {
                ifelse(isTRUE(length(edhl2) == 1) == TRUE, return(edhl2[[1]]), 
                  return(edhl2))
            }
        }
    }
}
