library(jsonlite)

ast_obj <- fromJSON("st_tree.txt",
    flatten           = FALSE,
    simplifyDataFrame = FALSE,
    simplifyMatrix    = FALSE,
    simplifyVector    = FALSE
)

emit <- function(msg) {
    cat(msg, "\n")
}

debugFlag <- FALSE

debugPrint <- function(...) {
    if (debugFlag) {
        args <- lapply(list(...), function(x) {
            if (is.list(x)) paste(unlist(x), collapse = " ") else as.character(x)
        })
        cat("[DEBUG]", paste(args, collapse = " "), "\n")
    }
}

symbol_table <- new.env(parent = emptyenv())
pointer_table <- new.env(parent = emptyenv())
based_table <- new.env(parent = emptyenv())

determineCodeType <- function(ast) {
    if (is.null(ast$declarations)) {
        return("Unknown")
    }

    for (decl in ast$declarations) {
        tokens <- decl$tokens
        if (length(tokens) > 0 && tokens[[1]]$type == "numeric_constant") {
            formatValue <- function(val) {
                if (is.list(val)) {
                    if (length(val) == 0) {
                        return("")
                    }
                    if (!is.null(names(val))) {
                        return(paste(sapply(val, formatValue), collapse = " "))
                    } else {
                        return(paste(sapply(val, formatValue), collapse = " "))
                    }
                } else {
                    return(as.character(val))
                }
            }

            createAggregate <- function(declTokens) {
                debugPrint("createAggregate: declTokens =", sapply(declTokens, function(t) t$value))
                if (length(declTokens) >= 2 && declTokens[[2]]$value == "(") {
                    sizes <- c()
                    i <- 3
                    while (i <= length(declTokens) && declTokens[[i]]$value != ")") {
                        if (declTokens[[i]]$type == "numeric_constant") {
                            sizes <- c(sizes, as.integer(declTokens[[i]]$value))
                        }
                        i <- i + 1
                    }
                    debugPrint("Создаем массив с размерами:", sizes)
                    createArray <- function(dims) {
                        if (length(dims) == 0) {
                            return(NA)
                        }
                        res <- vector("list", dims[1])
                        if (length(dims) > 1) {
                            for (j in seq_len(dims[1])) {
                                res[[j]] <- createArray(dims[-1])
                            }
                        } else {
                            for (j in seq_len(dims[1])) {
                                res[[j]] <- NA
                            }
                        }
                        return(res)
                    }
                    return(createArray(sizes))
                }
                debugPrint("Создаем структуру (пустой список)")
                return(list())
            }

            parseCompoundIdentifier <- function(tokens) {
                debugPrint("parseCompoundIdentifier: tokens =", sapply(tokens, function(t) t$value))
                parts <- list()
                i <- 1
                if (tokens[[i]]$type %in% c("identifier", "numeric_constant")) {
                    parts[[length(parts) + 1]] <- list(type = "name", value = tokens[[i]]$value)
                    i <- i + 1
                }
                while (i <= length(tokens)) {
                    if (tokens[[i]]$value == "(") {
                        i <- i + 1
                        indices <- c()
                        while (i <= length(tokens) && tokens[[i]]$value != ")") {
                            if (tokens[[i]]$type == "numeric_constant") {
                                indices <- c(indices, as.integer(tokens[[i]]$value))
                            }
                            i <- i + 1
                        }
                        parts[[length(parts) + 1]] <- list(type = "index", value = indices)
                        i <- i + 1
                    } else if (tokens[[i]]$value == ".") {
                        i <- i + 1
                        if (i <= length(tokens) && tokens[[i]]$type == "identifier") {
                            parts[[length(parts) + 1]] <- list(type = "field", value = tokens[[i]]$value)
                            i <- i + 1
                        }
                    } else {
                        i <- i + 1
                    }
                }
                debugPrint("parseCompoundIdentifier:", parts)
                return(parts)
            }

            getCompoundValue <- function(parts) {
                debugPrint("getCompoundValue: parts =", parts)
                curVal <- if (exists(parts[[1]]$value, envir = symbol_table, inherits = FALSE)) {
                    get(parts[[1]]$value, envir = symbol_table)
                } else {
                    NA
                }
                debugPrint("Начальное значение", parts[[1]]$value, "=", curVal)
                if (is.atomic(curVal) && length(curVal) == 1 && is.na(curVal)) {
                    return(NA)
                }
                if (length(parts) > 1) {
                    for (i in 2:length(parts)) {
                        p <- parts[[i]]
                        if (p$type == "index") {
                            debugPrint("Обрабатываем индекс:", p$value)
                            for (idx in p$value) {
                                if (is.list(curVal) && length(curVal) >= idx) {
                                    curVal <- curVal[[idx]]
                                } else {
                                    debugPrint("Индекс", idx, "вне диапазона")
                                    return(NA)
                                }
                            }
                        } else if (p$type == "field") {
                            debugPrint("Обрабатываем поле:", p$value)
                            if (is.list(curVal) && !is.null(curVal[[p$value]])) {
                                curVal <- curVal[[p$value]]
                            } else {
                                debugPrint("Поле", p$value, "не найдено")
                                return(NA)
                            }
                        }
                    }
                }
                debugPrint("getCompoundValue возвращает:", curVal)
                return(curVal)
            }

            setCompoundValue <- function(parts, newVal) {
                debugPrint("setCompoundValue: parts =", parts, "новое значение =", newVal)
                if (length(parts) == 0) {
                    return()
                }
                baseName <- parts[[1]]$value
                if (!exists(baseName, envir = symbol_table, inherits = FALSE)) {
                    assign(baseName, NA, envir = symbol_table)
                }
                curVal <- get(baseName, envir = symbol_table)
                if ((length(parts) >= 2) &&
                    ((is.atomic(curVal) && length(curVal) == 1 && is.na(curVal)) || is.null(curVal))) {
                    curVal <- list()
                }
                setNested <- function(cur, parts, pos, newVal) {
                    if (pos > length(parts)) {
                        return(newVal)
                    }
                    p <- parts[[pos]]
                    if (p$type == "index") {
                        indices <- p$value
                        if (!is.list(cur)) cur <- list()
                        idx <- indices[1]
                        if (length(cur) < idx || is.null(cur[[idx]])) {
                            cur[[idx]] <- if (length(indices) == 1 && pos == length(parts)) NA else list()
                        }
                        if (length(indices) > 1) {
                            cur[[idx]] <- setNested(cur[[idx]], list(list(type = "index", value = indices[-1])), 1, newVal)
                        } else {
                            cur[[idx]] <- setNested(cur[[idx]], parts, pos + 1, newVal)
                        }
                        return(cur)
                    } else if (p$type == "field") {
                        if (!is.list(cur)) cur <- list()
                        fieldName <- p$value
                        debugPrint("Устанавливаем поле", fieldName)
                        cur[[fieldName]] <- setNested(if (!is.null(cur[[fieldName]])) cur[[fieldName]] else NA, parts, pos + 1, newVal)
                        return(cur)
                    } else {
                        return(newVal)
                    }
                }
                newBaseVal <- setNested(curVal, parts, 2, newVal)
                assign(baseName, newBaseVal, envir = symbol_table)
                debugPrint("После установки, базовое значение", baseName, "=", get(baseName, envir = symbol_table))
            }

            parseNumeric <- function(x) {
                debugPrint("parseNumeric: x =", x)
                if (is.null(x) || length(x) == 0) {
                    debugPrint("parseNumeric: x is null or empty")
                    return(NULL)
                }
                x <- as.character(x)
                x <- trimws(x)
                if (nchar(x) == 0) {
                    debugPrint("parseNumeric: trimmed x is empty")
                    return(NULL)
                }
                val <- suppressWarnings(as.numeric(x))
                debugPrint("parseNumeric: val =", val)
                if (is.na(val)) {
                    return(NULL)
                }
                return(val)
            }

            getValue <- function(varToken) {
                debugPrint("getValue: varToken =", varToken)
                if (is.list(varToken) && length(varToken) > 1) {
                    compound <- parseCompoundIdentifier(varToken)
                    return(getCompoundValue(compound))
                }
                if (exists(varToken, envir = symbol_table, inherits = FALSE)) {
                    value <- get(varToken, envir = symbol_table)
                    debugPrint("getValue:", varToken, "=", value)
                    return(value)
                } else {
                    debugPrint("getValue:", varToken, "не найдено")
                    return(NA)
                }
            }

            setValue <- function(varToken, newVal) {
                debugPrint("setValue: varToken =", varToken, "newVal =", newVal)
                if (is.list(varToken) && length(varToken) > 1) {
                    compound <- parseCompoundIdentifier(varToken)
                    setCompoundValue(compound, newVal)
                    return()
                }
                assign(varToken, newVal, envir = symbol_table)
            }

            setPointer <- function(ptrName, varName) {
                debugPrint("setPointer:", ptrName, "->", varName)
                assign(ptrName, varName, envir = pointer_table)
            }

            declareBased <- function(basedVar, ptrName) {
                debugPrint("declareBased:", basedVar, "->", ptrName)
                assign(basedVar, ptrName, envir = based_table)
            }

            handleOpenFile <- function(tokens) {
                titleIdx <- which(toupper(sapply(tokens, function(t) t$value)) == "TITLE")
                if (length(titleIdx) == 0) {
                    debugPrint("handleOpenFile: TITLE не найден")
                    return()
                }
                closeParenIdx <- which(sapply(tokens, function(t) t$value) == ")")
                validClose <- closeParenIdx[closeParenIdx > titleIdx[1]]
                if (length(validClose) == 0) {
                    debugPrint("handleOpenFile: закрывающая скобка не найдена")
                    return()
                }
                fileNameToken <- tokens[[min(validClose) - 1]]
                fileName <- fileNameToken$value
                fileName <- gsub("^['\"](.*)['\"]$", "\\1", fileName)
                debugPrint("handleOpenFile: filename =", fileName)
                if (!file.exists(fileName)) {
                    file.create(fileName)
                    debugPrint("handleOpenFile: файл создан")
                } else {
                    debugPrint("handleOpenFile: файл уже существует")
                }
            }

            if (!is.null(ast_obj$declarations)) {
                for (decl in ast_obj$declarations) {
                    tokens <- decl$tokens
                    if (length(tokens) < 1) next
                    varNameToken <- tokens[[1]]
                    if (varNameToken$type == "numeric_constant") {
                        varNameToken <- tokens[[2]]
                        setValue(varNameToken$value, list())
                        debugPrint("Обработка декларации структуры:", varNameToken$value)
                    } else {
                        if (length(tokens) >= 2 && tokens[[2]]$value == "(") {
                            agg <- createAggregate(tokens)
                            setValue(varNameToken$value, agg)
                            debugPrint("Обработка декларации массива:", varNameToken$value)
                        } else {
                            setValue(varNameToken$value, NA)
                            debugPrint("Обработка декларации скалярной переменной:", varNameToken$value)
                        }
                    }
                    inInit <- FALSE
                    buf <- c()
                    for (iTok in seq_along(tokens)) {
                        t <- tokens[[iTok]]
                        if (t$type == "keyword" && toupper(t$value) == "INITIAL") {
                            inInit <- TRUE
                            next
                        }
                        if (inInit) {
                            if (t$type == "operator" && t$value == ")") {
                                inInit <- FALSE
                                raw <- paste0(buf, collapse = "")
                                raw <- trimws(raw)
                                raw <- gsub("^['\"](.*)['\"]$", "\\1", raw)
                                debugPrint("Обнаружено значение для", varNameToken$value, ":", raw)
                                maybeN <- parseNumeric(raw)
                                initVal <- if (!is.null(maybeN)) maybeN else raw
                                setValue(varNameToken$value, initVal)
                                buf <- c()
                            } else if (!(t$type == "operator" && t$value == "(")) {
                                buf <- c(buf, t$value)
                            }
                        }
                    }
                    debugPrint("Установлено значение", varNameToken$value, "=", getValue(varNameToken$value))
                }
            }

            processPutTokens <- function(tokens) {
                outParts <- c()
                i <- 1
                parenCount <- 0
                compoundTokens <- list()
                while (i <= length(tokens)) {
                    t <- tokens[[i]]
                    if (t$value == "(") {
                        parenCount <- parenCount + 1
                        compoundTokens[[length(compoundTokens) + 1]] <- t
                    } else if (t$value == ")") {
                        parenCount <- parenCount - 1
                        compoundTokens[[length(compoundTokens) + 1]] <- t
                    } else if (t$value == "," && parenCount == 0) {
                        if (length(compoundTokens) > 0) {
                            outParts <- c(outParts, formatValue(getCompoundValue(parseCompoundIdentifier(compoundTokens))))
                            compoundTokens <- list()
                        }
                    } else if (t$type == "string_constant") {
                        if (length(compoundTokens) > 0) {
                            outParts <- c(outParts, formatValue(getCompoundValue(parseCompoundIdentifier(compoundTokens))))
                            compoundTokens <- list()
                        }
                        outParts <- c(outParts, gsub("^['\"](.*)['\"]$", "\\1", t$value))
                    } else {
                        compoundTokens[[length(compoundTokens) + 1]] <- t
                    }
                    i <- i + 1
                }
                if (length(compoundTokens) > 0) {
                    outParts <- c(outParts, formatValue(getCompoundValue(parseCompoundIdentifier(compoundTokens))))
                }
                return(outParts)
            }

            findMatchingParen <- function(tokens, startIndex) {
                count <- 0
                for (i in startIndex:length(tokens)) {
                    if (tokens[[i]]$value == "(") {
                        count <- count + 1
                    } else if (tokens[[i]]$value == ")") {
                        count <- count - 1
                        if (count == 0) {
                            return(i)
                        }
                    }
                }
                return(NA)
            }

            evaluateCondition <- function(tokens) {
                debugPrint("evaluateCondition: tokens =", sapply(tokens, function(t) t$value))
                if (length(tokens) < 3) {
                    return(FALSE)
                }
                leftVal <- parseLeftExpression(tokens, 1, 1)
                op <- tokens[[2]]$value
                rightVal <- parseCompareValue(tokens[[3]])
                debugPrint("evaluateCondition: leftVal =", leftVal, "op =", op, "rightVal =", rightVal)
                if (is.null(leftVal) || is.null(rightVal) || is.na(leftVal) || is.na(rightVal)) {
                    return(FALSE)
                }
                if (op == "<") {
                    return(leftVal < rightVal)
                }
                if (op == "<=") {
                    return(leftVal <= rightVal)
                }
                if (op == ">") {
                    return(leftVal > rightVal)
                }
                if (op == ">=") {
                    return(leftVal >= rightVal)
                }
                if (op == "=") {
                    return(leftVal == rightVal)
                }
                if (op == "<>") {
                    return(leftVal != rightVal)
                }
                return(FALSE)
            }

            evaluateExpression <- function(tokens, env) {
                debugPrint("evaluateExpression: tokens =", sapply(tokens, function(t) t$value))
                if (length(tokens) == 0) {
                    return(NULL)
                }
                result <- NULL
                current_op <- NULL
                for (token in tokens) {
                    if (token$type %in% c("numeric_constant", "string_constant")) {
                        value <- token$value
                        if (token$type == "numeric_constant") {
                            value <- if (grepl("\\.", value)) as.numeric(value) else as.integer(value)
                        }
                        result <- if (is.null(result)) value else result + (if (!is.null(current_op) && current_op == "+") value else 0)
                    } else if (token$type == "identifier") {
                        value <- getValue(token$value)
                        if (is.null(value) || is.na(value)) value <- 0
                        result <- if (is.null(result)) value else result + (if (!is.null(current_op) && current_op == "+") value else 0)
                    } else if (token$type == "operator" && token$value == "+") {
                        current_op <- token$value
                    }
                }
                debugPrint("evaluateExpression: result =", result)
                return(result)
            }

            parseCompareValue <- function(tok) {
                debugPrint("parseCompareValue: ток =", tok$value, "тип =", tok$type)
                if (tok$type == "numeric_constant") {
                    maybe <- parseNumeric(tok$value)
                    if (!is.null(maybe)) {
                        return(maybe)
                    } else {
                        return(NA)
                    }
                } else if (tok$type == "string_constant") {
                    return(gsub("^['\"](.*)['\"]$", "\\1", tok$value))
                } else if (tok$type == "identifier") {
                    return(getValue(tok$value))
                }
                return(NA)
            }

            parseLeftExpression <- function(tokens, startPos, endPos) {
                debugPrint("parseLeftExpression: tokens =", sapply(tokens[startPos:endPos], function(t) t$value))
                if (startPos > length(tokens) || startPos > endPos) {
                    return(NA)
                }
                if (toupper(tokens[[startPos]]$value) == "MOD") {
                    debugPrint("parseLeftExpression: обнаружен вызов MOD")
                    if ((endPos - startPos + 1) < 6) {
                        return(NA)
                    }
                    arg1 <- getValue(tokens[[4]]$value)
                    arg2 <- parseNumeric(tokens[[6]]$value)
                    debugPrint("MOD args:", arg1, arg2)
                    if (is.null(arg1) || is.null(arg2)) {
                        return(NA)
                    }
                    return(as.numeric(arg1) %% arg2)
                } else {
                    varName <- tokens[[startPos]]$value
                    v <- getValue(varName)
                    maybeNum <- parseNumeric(v)
                    debugPrint("parseLeftExpression: var", varName, "=", v)
                    return(if (!is.null(maybeNum)) maybeNum else v)
                }
            }

            interpretIf <- function(tokens) {
                debugPrint("interpretIf: tokens =", sapply(tokens, function(t) t$value))
                condOpPos <- NA
                for (i in seq_along(tokens)) {
                    if (tokens[[i]]$type == "operator" && tokens[[i]]$value %in% c("=", "<", "<=", ">", ">=", "<>")) {
                        condOpPos <- i
                        break
                    }
                }
                if (is.na(condOpPos) || condOpPos + 1 > length(tokens)) {
                    return(list(action = "normal"))
                }
                rightVal <- parseCompareValue(tokens[[condOpPos + 1]])
                leftVal <- parseLeftExpression(tokens, 2, condOpPos - 1)
                op <- tokens[[condOpPos]]$value
                pass <- FALSE
                if (!is.null(leftVal) && !is.null(rightVal) && !is.na(leftVal) && !is.na(rightVal)) {
                    if (op == "=") {
                        pass <- (leftVal == rightVal)
                    } else if (op == "<") {
                        pass <- (leftVal < rightVal)
                    } else if (op == "<=") {
                        pass <- (leftVal <= rightVal)
                    } else if (op == ">") {
                        pass <- (leftVal > rightVal)
                    } else if (op == ">=") {
                        pass <- (leftVal >= rightVal)
                    } else if (op == "<>") pass <- (leftVal != rightVal)
                }
                debugPrint("interpretIf: условие =", pass)
                idxThen <- which(sapply(tokens, function(t) t$value) == "THEN")
                if (length(idxThen) == 0) {
                    return(list(action = "normal"))
                }
                idxThen <- idxThen[1]
                if (any(sapply(tokens, function(t) t$value) == "ELSE")) {
                    idxElse <- which(sapply(tokens, function(t) t$value) == "ELSE")[1]
                    branchTokens <- if (pass) tokens[(idxThen + 1):(idxElse - 1)] else tokens[(idxElse + 1):length(tokens)]
                } else {
                    branchTokens <- if (pass) tokens[(idxThen + 1):length(tokens)] else list()
                }
                if (length(branchTokens) == 0) {
                    return(list(action = "normal"))
                }
                firstWord <- toupper(trimws(branchTokens[[1]]$value))
                if (firstWord %in% c("LEAVE", "CONTINUE")) {
                    return(list(action = tolower(firstWord)))
                }
                interpretPut(branchTokens)
                return(list(action = "normal"))
            }

            interpretPut <- function(tokens) {
                debugPrint("interpretPut: tokens =", sapply(tokens, function(t) t$value))
                opens <- which(sapply(tokens, function(t) t$value) == "(")
                closes <- which(sapply(tokens, function(t) t$value) == ")")
                if (length(opens) > 0 && length(closes) > 0 && opens[1] < closes[length(closes)]) {
                    mid <- tokens[(opens[1] + 1):(closes[length(closes)] - 1)]
                    out <- processPutTokens(mid)
                    emit(paste(out, collapse = " "))
                }
            }

            interpretAssignment <- function(tokens) {
                debugPrint("interpretAssignment: tokens =", sapply(tokens, function(t) t$value))
                eqPos <- which(sapply(tokens, function(t) t$value) == "=")[1]
                lhsTokens <- tokens[1:(eqPos - 1)]
                rhsTokens <- tokens[(eqPos + 1):(length(tokens) - 1)]
                raw <- paste(sapply(rhsTokens, function(t) t$value), collapse = " ")
                maybeN <- parseNumeric(raw)
                if (!is.null(maybeN)) {
                    setCompoundValue(parseCompoundIdentifier(lhsTokens), maybeN)
                    debugPrint("interpretAssignment: присваиваем число", maybeN)
                } else {
                    vR <- if (length(rhsTokens) == 1) {
                        if (rhsTokens[[1]]$type == "string_constant") {
                            gsub("^['\"](.*)['\"]$", "\\1", rhsTokens[[1]]$value)
                        } else {
                            getValue(rhsTokens[[1]]$value)
                        }
                    } else {
                        raw
                    }
                    setCompoundValue(parseCompoundIdentifier(lhsTokens), vR)
                    debugPrint("interpretAssignment: присваиваем значение", vR)
                }
            }

            interpretDeclare <- function(tokens) {
                debugPrint("interpretDeclare: tokens =", sapply(tokens, function(t) t$value))
                if (length(tokens) < 2) {
                    return()
                }
                if (!(length(tokens) >= 2 && tokens[[2]]$value %in% c("(", "BASED"))) {
                    varN <- tokens[[1]]$value
                    setValue(varN, NA)
                }
            }

            findMatchingEnd <- function(startPos) {
                for (i in seq(from = startPos, to = length(statements))) {
                    t <- statements[[i]]$tokens
                    if (length(t) == 1 && toupper(t[[1]]$value) == "END") {
                        return(i)
                    }
                }
                return(-1)
            }

            parseDoLine <- function(tokens) {
                headerWords <- sapply(tokens, function(t) t$value)
                debugPrint("parseDoLine: headerWords =", headerWords)
                if (any(headerWords == ",")) {
                    loopVar <- tokens[[2]]$value
                    splitIndex <- NA
                    for (i in 4:(length(tokens) - 1)) {
                        if (tokens[[i]]$value == loopVar && tokens[[i + 1]]$value == "=") {
                            splitIndex <- i
                            break
                        }
                    }
                    if (is.na(splitIndex)) stop("Не найден разделитель в цикле LIST.")
                    explicitVals <- c()
                    for (i in 4:(splitIndex - 1)) {
                        if (tokens[[i]]$type == "numeric_constant") {
                            explicitVals <- c(explicitVals, as.numeric(tokens[[i]]$value))
                        }
                    }
                    rangeTokens <- tokens[splitIndex:length(tokens)]
                    if (length(rangeTokens) < 5) stop("Неверный формат диапазонной части цикла.")
                    startVal <- as.numeric(rangeTokens[[3]]$value)
                    endVal <- as.numeric(rangeTokens[[5]]$value)
                    stepVal <- 1
                    if ("BY" %in% toupper(sapply(rangeTokens, function(t) t$value))) {
                        byIdx <- which(toupper(sapply(rangeTokens, function(t) t$value)) == "BY")[1]
                        if (rangeTokens[[byIdx + 1]]$value == "-") {
                            stepVal <- -as.numeric(rangeTokens[[byIdx + 2]]$value)
                        } else {
                            stepVal <- as.numeric(rangeTokens[[byIdx + 1]]$value)
                        }
                    }
                    seqVals <- seq(from = startVal, to = endVal, by = stepVal)
                    finalVals <- c(explicitVals, seqVals)
                    debugPrint("parseDoLine (LIST): for variable", loopVar, "values =", finalVals)
                    return(list(type = "LIST", var = loopVar, values = finalVals))
                }
                if (toupper(tokens[[2]]$value) == "WHILE") {
                    startParen <- which(headerWords == "(")[1]
                    endParen <- which(headerWords == ")")[1]
                    conditionTokens <- tokens[(startParen + 1):(endParen - 1)]
                    debugPrint("parseDoLine: WHILE condition tokens =", sapply(conditionTokens, function(t) t$value))
                    return(list(type = "WHILE", condition = conditionTokens))
                }
                if ("REPEAT" %in% toupper(headerWords)) {
                    eqIdx <- which(headerWords == "=")[1]
                    repeatIdx <- which(toupper(headerWords) == "REPEAT")[1]
                    loopVar <- tokens[[2]]$value
                    startVal <- as.numeric(tokens[[eqIdx + 1]]$value)
                    incVal <- 1
                    if (repeatIdx + 1 <= length(tokens) && tokens[[repeatIdx + 1]]$value == "(") {
                        startParen <- repeatIdx + 1
                        endParen <- findMatchingParen(tokens, startParen)
                        incTokens <- tokens[(startParen + 1):(endParen - 1)]
                        rawInc <- evaluateExpression(incTokens, NULL)
                        incVal <- rawInc - startVal
                        if (is.null(incVal) || !is.numeric(incVal)) incVal <- 1
                    }
                    if ("WHILE" %in% toupper(headerWords)) {
                        allParenIdx <- which(headerWords %in% c("(", ")"))
                        if (length(allParenIdx) >= 4) {
                            condStart <- allParenIdx[3]
                            condEnd <- allParenIdx[4]
                            conditionTokens <- tokens[(condStart + 1):(condEnd - 1)]
                        } else {
                            conditionTokens <- list()
                        }
                        return(list(type = "REPEAT_WHILE", var = loopVar, startVal = startVal, inc = incVal, condition = conditionTokens))
                    } else {
                        return(list(type = "REPEAT", var = loopVar, startVal = startVal, inc = incVal))
                    }
                }
                if ("TO" %in% toupper(headerWords)) {
                    eqIdx <- which(headerWords == "=")[1]
                    toIdx <- which(toupper(headerWords) == "TO")[1]
                    loopVar <- tokens[[2]]$value
                    startVal <- as.numeric(tokens[[eqIdx + 1]]$value)
                    endVal <- as.numeric(tokens[[toIdx + 1]]$value)
                    stepVal <- 1
                    if ("BY" %in% toupper(headerWords)) {
                        byIdx <- which(toupper(headerWords) == "BY")[1]
                        if (tokens[[byIdx + 1]]$value == "-") {
                            if (byIdx + 2 <= length(tokens)) stepVal <- -as.numeric(tokens[[byIdx + 2]]$value)
                        } else {
                            stepVal <- as.numeric(tokens[[byIdx + 1]]$value)
                        }
                    }
                    if ("WHILE" %in% toupper(headerWords)) {
                        whIdx <- which(toupper(headerWords) == "WHILE")[1]
                        condCandidates <- which(headerWords == "(" & seq_along(headerWords) > whIdx)
                        condStart <- if (length(condCandidates) == 0) NA else condCandidates[1]
                        condCandidates2 <- which(headerWords == ")" & seq_along(headerWords) > whIdx)
                        condEnd <- if (length(condCandidates2) == 0) NA else condCandidates2[1]
                        conditionTokens <- if (is.na(condStart) || is.na(condEnd)) list() else tokens[(condStart + 1):(condEnd - 1)]
                        return(list(type = "TO_WHILE", var = loopVar, startVal = startVal, endVal = endVal, step = stepVal, condition = conditionTokens))
                    } else {
                        return(list(type = "TO", var = loopVar, startVal = startVal, endVal = endVal, step = stepVal))
                    }
                }
                return(NULL)
            }

            runDoBlock <- function(def, bodyStart, bodyEnd) {
                debugPrint("runDoBlock: type =", def$type)
                if (def$type == "WHILE") {
                    nIter <- 0
                    while (TRUE) {
                        nIter <- nIter + 1
                        if (nIter > 1000) break
                        cond <- evaluateCondition(def$condition)
                        debugPrint("runDoBlock WHILE: condition =", cond)
                        if (!isTRUE(cond)) break
                        res <- interpretBlockSlice(bodyStart, bodyEnd)
                        if (!is.null(res$action) && res$action == "leave") {
                            return("breakOuter")
                        }
                    }
                } else if (def$type == "REPEAT") {
                    setValue(def$var, def$startVal)
                    nIter <- 0
                    repeat {
                        nIter <- nIter + 1
                        if (nIter > 1000) break
                        res <- interpretBlockSlice(bodyStart, bodyEnd)
                        if (!is.null(res$action) && res$action == "leave") {
                            return("breakOuter")
                        }
                        cur <- parseNumeric(getValue(def$var))
                        if (is.null(cur)) break
                        setValue(def$var, cur + def$inc)
                    }
                } else if (def$type == "REPEAT_WHILE") {
                    setValue(def$var, def$startVal)
                    nIter <- 0
                    repeat {
                        nIter <- nIter + 1
                        if (nIter > 1000) break
                        cond <- evaluateCondition(def$condition)
                        if (!isTRUE(cond)) break
                        res <- interpretBlockSlice(bodyStart, bodyEnd)
                        if (!is.null(res$action) && res$action == "leave") {
                            return("breakOuter")
                        }
                        cur <- parseNumeric(getValue(def$var))
                        if (is.null(cur)) break
                        setValue(def$var, cur + def$inc)
                    }
                } else if (def$type == "TO") {
                    setValue(def$var, def$startVal)
                    nIter <- 0
                    repeat {
                        nIter <- nIter + 1
                        cur <- parseNumeric(getValue(def$var))
                        if (is.null(cur)) break
                        if (def$startVal <= def$endVal) {
                            if (cur > def$endVal) break
                        } else {
                            if (cur < def$endVal) break
                        }
                        res <- interpretBlockSlice(bodyStart, bodyEnd)
                        if (!is.null(res$action) && res$action == "leave") {
                            return("breakOuter")
                        }
                        setValue(def$var, cur + def$step)
                    }
                } else if (def$type == "TO_WHILE") {
                    setValue(def$var, def$startVal)
                    nIter <- 0
                    repeat {
                        nIter <- nIter + 1
                        cur <- parseNumeric(getValue(def$var))
                        if (is.null(cur)) break
                        if (def$startVal <= def$endVal) {
                            if (cur > def$endVal) break
                        } else {
                            if (cur < def$endVal) break
                        }
                        cond <- evaluateCondition(def$condition)
                        if (!isTRUE(cond)) break
                        res <- interpretBlockSlice(bodyStart, bodyEnd)
                        if (!is.null(res$action) && res$action == "leave") {
                            return("breakOuter")
                        }
                        setValue(def$var, cur + def$step)
                    }
                } else if (def$type == "LIST") {
                    for (val in def$values) {
                        setValue(def$var, val)
                        res <- interpretBlockSlice(bodyStart, bodyEnd)
                        if (!is.null(res$action) && res$action == "leave") {
                            return("breakOuter")
                        }
                    }
                }
                return("normal")
            }

            interpretBlockSlice <- function(from, to) {
                pc <- from
                while (pc <= to) {
                    if (pc < to &&
                        toupper(statements[[pc]]$tokens[[1]]$value) == "IF" &&
                        toupper(statements[[pc + 1]]$tokens[[1]]$value) == "ELSE") {
                        combinedTokens <- c(statements[[pc]]$tokens, statements[[pc + 1]]$tokens)
                        debugPrint("interpretBlockSlice: объединяем IF и ELSE:", sapply(combinedTokens, function(t) t$value))
                        r <- interpretIf(combinedTokens)
                        if (!is.null(r$action) && r$action %in% c("jump", "continue", "leave")) {
                            return(r)
                        }
                        pc <- pc + 2
                        next
                    }
                    toks <- statements[[pc]]$tokens
                    debugPrint("interpretBlockSlice: pc =", pc, "toks =", sapply(toks, function(t) t$value))
                    if (length(toks) == 1 && toupper(toks[[1]]$value) == "END") {
                        return(list(action = "normal", nextPC = pc + 1))
                    }
                    if (length(toks) >= 2 && toks[[2]]$value == ":") {
                        newTokens <- toks[-c(1, 2)]
                        if (length(newTokens) > 0) {
                            r <- interpretOneStatementCustom(newTokens)
                            if (!is.null(r$action) && r$action == "jump") {
                                pc <- r$nextPC
                                next
                            }
                        }
                        pc <- pc + 1
                        next
                    }
                    r <- interpretOneStatement(pc)
                    if (!is.null(r$action) && r$action %in% c("jump", "continue", "leave")) {
                        return(r)
                    }
                    pc <- r$nextPC
                }
                return(list(action = "normal", nextPC = pc))
            }

            interpretOneStatement <- function(pc) {
                tokens <- statements[[pc]]$tokens
                debugPrint("interpretOneStatement: pc =", pc, "tokens =", sapply(tokens, function(t) t$value))
                if (length(tokens) == 0) {
                    return(list(action = "normal", nextPC = pc + 1))
                }
                cmd <- toupper(tokens[[1]]$value)
                if (cmd == "END") {
                    return(list(action = "normal", nextPC = pc + 1))
                }
                if (cmd == "OPEN") {
                    handleOpenFile(tokens)
                    return(list(action = "normal", nextPC = pc + 1))
                }
                if (cmd == "IF") {
                    r <- interpretIf(tokens)
                    return(list(action = r$action, nextPC = pc + 1))
                }
                if (cmd == "PUT") {
                    interpretPut(tokens)
                    return(list(action = "normal", nextPC = pc + 1))
                }
                if (cmd == "GOTO") {
                    labVar <- tokens[[2]]$value
                    labVal <- getValue(labVar)
                    debugPrint("GOTO: labVar =", labVar, "labVal =", labVal)
                    if (!is.null(labVal) && !is.na(labVal)) {
                        jumpIdx <- labelIndices[[as.character(labVal)]]
                        if (!is.null(jumpIdx)) {
                            return(list(action = "jump", nextPC = jumpIdx))
                        }
                    }
                    return(list(action = "normal", nextPC = pc + 1))
                }
                if (cmd == "CALL") {
                    procName <- tokens[[2]]$value
                    pval <- getValue(procName)
                    if (!is.null(pval) && !is.na(pval)) {
                        j <- labelIndices[[as.character(pval)]]
                        if (!is.null(j)) {
                            return(list(action = "jump", nextPC = j))
                        }
                    }
                    return(list(action = "normal", nextPC = pc + 1))
                }
                if (cmd == "DECLARE") {
                    interpretDeclare(tokens)
                    return(list(action = "normal", nextPC = pc + 1))
                }
                if (cmd %in% c("CONTINUE", "LEAVE")) {
                    return(list(action = tolower(cmd), nextPC = pc + 1))
                }
                if (tokens[[1]]$type == "identifier" &&
                    any(sapply(tokens, function(t) t$value) %in% c("(", ".")) &&
                    any(sapply(tokens, function(t) t$value) == "=")) {
                    interpretAssignment(tokens)
                    return(list(action = "normal", nextPC = pc + 1))
                }
                return(list(action = "normal", nextPC = pc + 1))
            }

            interpretOneStatementCustom <- function(tokens) {
                if (length(tokens) == 0) {
                    return(list(action = "normal", nextPC = NA))
                }
                cmd <- toupper(tokens[[1]]$value)
                if (cmd == "END") {
                    return(list(action = "normal", nextPC = NA))
                }
                if (cmd == "ELSE") {
                    return(interpretOneStatementCustom(tokens[-1]))
                }
                if (cmd == "IF") {
                    r <- interpretIf(tokens)
                    return(list(action = r$action, nextPC = NA))
                }
                if (cmd == "PUT") {
                    interpretPut(tokens)
                    return(list(action = "normal", nextPC = NA))
                }
                if (cmd == "GOTO") {
                    labVar <- tokens[[2]]$value
                    labVal <- getValue(labVar)
                    if (!is.null(labVal) && !is.na(labVal)) {
                        jumpIdx <- labelIndices[[as.character(labVal)]]
                        if (!is.null(jumpIdx)) {
                            return(list(action = "jump", nextPC = jumpIdx))
                        }
                    }
                    return(list(action = "normal", nextPC = NA))
                }
                if (cmd == "CALL") {
                    procName <- tokens[[2]]$value
                    pval <- getValue(procName)
                    if (!is.null(pval) && !is.na(pval)) {
                        j <- labelIndices[[as.character(pval)]]
                        if (!is.null(j)) {
                            return(list(action = "jump", nextPC = j))
                        }
                    }
                    return(list(action = "normal", nextPC = NA))
                }
                if (cmd == "DECLARE") {
                    interpretDeclare(tokens)
                    return(list(action = "normal", nextPC = NA))
                }
                if (cmd %in% c("CONTINUE", "LEAVE")) {
                    return(list(action = tolower(cmd), nextPC = NA))
                }
                if (tokens[[1]]$type == "identifier" &&
                    any(sapply(tokens, function(t) t$value) %in% c("(", ".")) &&
                    any(sapply(tokens, function(t) t$value) == "=")) {
                    interpretAssignment(tokens)
                    return(list(action = "normal", nextPC = NA))
                }
                return(list(action = "normal", nextPC = NA))
            }

            interpretBlock <- function(startPC = 1) {
                pc <- startPC
                while (pc <= length(statements)) {
                    tokens <- statements[[pc]]$tokens
                    debugPrint("interpretBlock: pc =", pc, "tokens =", sapply(tokens, function(t) t$value))
                    if (length(tokens) == 0) {
                        pc <- pc + 1
                        next
                    }
                    if (length(tokens) == 1 && toupper(tokens[[1]]$value) == "END") {
                        return(list(action = "normal", pc = pc + 1))
                    }
                    if (length(tokens) >= 2 && tokens[[2]]$value == ":") {
                        newTokens <- tokens[-c(1, 2)]
                        if (length(newTokens) > 0) {
                            r <- interpretOneStatementCustom(newTokens)
                            if (!is.null(r$action) && r$action == "jump") {
                                pc <- r$nextPC
                                next
                            }
                        }
                        pc <- pc + 1
                        next
                    }
                    r <- interpretOneStatement(pc)
                    if (r$action == "jump") {
                        pc <- r$nextPC
                        next
                    }
                    if (r$action %in% c("continue", "leave")) {
                        return(r)
                    }
                    pc <- r$nextPC
                }
                return(list(action = "normal", pc = pc))
            }

            labelIndices <- list()
            statements <- NULL

            if (is.list(ast_obj) && !is.null(ast_obj$type)) {
                statements <- ast_obj$statements
                invisible(interpretBlock(1))
            } else if (is.list(ast_obj) && length(ast_obj) > 0) {
                statements <- ast_obj[[1]]$statements
                invisible(interpretBlock(1))
            }

            emit("=== Интерпретация завершена ===")

            return("Code 3")
        }
    }

    findMatchingParen <- function(tokens, startIndex) {
        count <- 0
        for (i in startIndex:length(tokens)) {
            if (tokens[[i]]$value == "(") {
                count <- count + 1
            } else if (tokens[[i]]$value == ")") {
                count <- count - 1
                if (count == 0) {
                    return(i)
                }
            }
        }
        return(NA)
    }

    parseNumeric <- function(x) {
        debugPrint("parseNumeric: x =", x)
        if (is.null(x) || length(x) == 0) {
            debugPrint("parseNumeric: x is null or empty")
            return(NULL)
        }
        x <- as.character(x)
        x <- trimws(x)
        if (nchar(x) == 0) {
            debugPrint("parseNumeric: trimmed x is empty")
            return(NULL)
        }
        val <- suppressWarnings(as.numeric(x))
        debugPrint("parseNumeric: val =", val)
        if (is.na(val)) {
            return(NULL)
        }
        return(val)
    }

    getValue <- function(varName) {
        debugPrint("getValue:", varName)
        if (exists(varName, envir = based_table, inherits = FALSE)) {
            ptrN <- get(varName, envir = based_table)
            debugPrint("getValue: found based var, pointer =", ptrN)
            if (exists(ptrN, envir = pointer_table, inherits = FALSE)) {
                realVar <- get(ptrN, envir = pointer_table)
                debugPrint("getValue: realVar =", realVar)
                if (exists(realVar, envir = symbol_table, inherits = FALSE)) {
                    value <- get(realVar, envir = symbol_table)
                    debugPrint("getValue: value =", value)
                    return(value)
                } else {
                    return(NA)
                }
            } else {
                return(NA)
            }
        } else if (exists(varName, envir = symbol_table, inherits = FALSE)) {
            value <- get(varName, envir = symbol_table)
            debugPrint("getValue: value =", value)
            return(value)
        } else {
            return(NA)
        }
    }

    setValue <- function(varName, newVal) {
        debugPrint("setValue:", varName, "=", newVal)
        if (exists(varName, envir = based_table, inherits = FALSE)) {
            ptrN <- get(varName, envir = based_table)
            if (exists(ptrN, envir = pointer_table, inherits = FALSE)) {
                realVar <- get(ptrN, envir = pointer_table)
                assign(realVar, newVal, envir = symbol_table)
                return()
            }
        }
        assign(varName, newVal, envir = symbol_table)
    }

    setPointer <- function(ptrName, varName) {
        debugPrint("setPointer:", ptrName, "->", varName)
        assign(ptrName, varName, envir = pointer_table)
    }

    declareBased <- function(basedVar, ptrName) {
        debugPrint("declareBased:", basedVar, "->", ptrName)
        assign(basedVar, ptrName, envir = based_table)
    }

    handleOpenFile <- function(tokens) {
        titleIdx <- which(toupper(sapply(tokens, function(t) t$value)) == "TITLE")
        if (length(titleIdx) == 0) {
            debugPrint("handleOpenFile: TITLE не найден")
            return()
        }
        closeParenIdx <- which(sapply(tokens, function(t) t$value) == ")")
        validClose <- closeParenIdx[closeParenIdx > titleIdx[1]]
        if (length(validClose) == 0) {
            debugPrint("handleOpenFile: закрывающая скобка не найдена")
            return()
        }
        fileNameToken <- tokens[[min(validClose) - 1]]
        fileName <- fileNameToken$value
        fileName <- gsub("^['\"](.*)['\"]$", "\\1", fileName)
        debugPrint("handleOpenFile: filename =", fileName)
        if (!file.exists(fileName)) {
            file.create(fileName)
            debugPrint("handleOpenFile: файл создан")
        } else {
            debugPrint("handleOpenFile: файл уже существует")
        }
    }

    if (!is.null(ast_obj$declarations)) {
        for (decl in ast_obj$declarations) {
            tokens <- decl$tokens
            if (length(tokens) < 1) next
            varName <- tokens[[1]]$value
            debugPrint("Обработка декларации:", varName)
            initVal <- NA
            inInit <- FALSE
            buf <- c()
            for (iTok in seq_along(tokens)) {
                t <- tokens[[iTok]]
                debugPrint("  Токен:", t$value, "тип:", t$type)
                if (t$type == "keyword" && toupper(t$value) == "INITIAL") {
                    inInit <- TRUE
                    next
                }
                if (inInit) {
                    if (t$type == "operator" && t$value == ")") {
                        inInit <- FALSE
                        raw <- paste0(buf, collapse = "")
                        raw <- trimws(raw)
                        raw <- gsub("^['\"](.*)['\"]$", "\\1", raw)
                        debugPrint("  Обнаружено значение:", raw)
                        maybeN <- parseNumeric(raw)
                        initVal <- if (!is.null(maybeN)) maybeN else raw
                        buf <- c()
                    } else if (!(t$type == "operator" && t$value == "(")) {
                        buf <- c(buf, t$value)
                    }
                }
            }
            setValue(varName, initVal)
            debugPrint("Установлено значение", varName, "=", getValue(varName))
        }
    }

    statements <- ast_obj$statements
    if (is.null(statements)) quit(save = "no")

    labelIndices <- list()
    for (i in seq_along(statements)) {
        toks <- statements[[i]]$tokens
        if (length(toks) >= 2 && toks[[2]]$value == ":") {
            labName <- toks[[1]]$value
            labelIndices[[labName]] <- i
            debugPrint("Найдена метка:", labName, "на позиции", i)
        }
    }

    evaluateCondition <- function(tokens) {
        debugPrint("evaluateCondition: tokens =", sapply(tokens, function(t) t$value))
        if (length(tokens) < 3) {
            return(FALSE)
        }
        leftVal <- parseLeftExpression(tokens, 1, 1)
        op <- tokens[[2]]$value
        rightVal <- parseCompareValue(tokens[[3]])
        debugPrint("evaluateCondition: leftVal =", leftVal, "op =", op, "rightVal =", rightVal)
        if (is.null(leftVal) || is.null(rightVal) || is.na(leftVal) || is.na(rightVal)) {
            return(FALSE)
        }
        if (op == "<") {
            return(leftVal < rightVal)
        }
        if (op == "<=") {
            return(leftVal <= rightVal)
        }
        if (op == ">") {
            return(leftVal > rightVal)
        }
        if (op == ">=") {
            return(leftVal >= rightVal)
        }
        if (op == "=") {
            return(leftVal == rightVal)
        }
        if (op == "<>") {
            return(leftVal != rightVal)
        }
        return(FALSE)
    }

    evaluateExpression <- function(tokens, env) {
        debugPrint("evaluateExpression: tokens =", sapply(tokens, function(t) t$value))
        if (length(tokens) == 0) {
            return(NULL)
        }
        result <- NULL
        current_op <- NULL
        for (token in tokens) {
            if (token$type %in% c("numeric_constant", "string_constant")) {
                value <- token$value
                if (token$type == "numeric_constant") {
                    value <- if (grepl("\\.", value)) as.numeric(value) else as.integer(value)
                }
                result <- if (is.null(result)) value else result + (if (!is.null(current_op) && current_op == "+") value else 0)
            } else if (token$type == "identifier") {
                value <- getValue(token$value)
                if (is.null(value) || is.na(value)) value <- 0
                result <- if (is.null(result)) value else result + (if (!is.null(current_op) && current_op == "+") value else 0)
            } else if (token$type == "operator" && token$value == "+") {
                current_op <- token$value
            }
        }
        debugPrint("evaluateExpression: result =", result)
        return(result)
    }

    parseCompareValue <- function(tok) {
        debugPrint("parseCompareValue: ток =", tok$value, "тип =", tok$type)
        if (tok$type == "numeric_constant") {
            maybe <- parseNumeric(tok$value)
            if (!is.null(maybe)) {
                return(maybe)
            } else {
                return(NA)
            }
        } else if (tok$type == "string_constant") {
            return(gsub("^['\"](.*)['\"]$", "\\1", tok$value))
        } else if (tok$type == "identifier") {
            return(getValue(tok$value))
        }
        return(NA)
    }

    parseLeftExpression <- function(tokens, startPos, endPos) {
        debugPrint("parseLeftExpression: tokens =", sapply(tokens[startPos:endPos], function(t) t$value))
        if (startPos > length(tokens) || startPos > endPos) {
            return(NA)
        }
        if (toupper(tokens[[startPos]]$value) == "MOD") {
            debugPrint("parseLeftExpression: обнаружен вызов MOD")
            if ((endPos - startPos + 1) < 6) {
                return(NA)
            }
            arg1 <- getValue(tokens[[4]]$value)
            arg2 <- parseNumeric(tokens[[6]]$value)
            debugPrint("MOD args:", arg1, arg2)
            if (is.null(arg1) || is.null(arg2)) {
                return(NA)
            }
            return(as.numeric(arg1) %% arg2)
        } else {
            varName <- tokens[[startPos]]$value
            v <- getValue(varName)
            maybeNum <- parseNumeric(v)
            debugPrint("parseLeftExpression: var", varName, "=", v)
            return(if (!is.null(maybeNum)) maybeNum else v)
        }
    }

    interpretIf <- function(tokens) {
        debugPrint("interpretIf: tokens =", sapply(tokens, function(t) t$value))
        condOpPos <- NA
        for (i in seq_along(tokens)) {
            if (tokens[[i]]$type == "operator" &&
                tokens[[i]]$value %in% c("=", "<", "<=", ">", ">=", "<>")) {
                condOpPos <- i
                break
            }
        }
        if (is.na(condOpPos) || condOpPos + 1 > length(tokens)) {
            return(list(action = "normal"))
        }
        rightVal <- parseCompareValue(tokens[[condOpPos + 1]])
        leftVal <- parseLeftExpression(tokens, 2, condOpPos - 1)
        op <- tokens[[condOpPos]]$value
        pass <- FALSE
        if (!is.null(leftVal) && !is.null(rightVal) && !is.na(leftVal) && !is.na(rightVal)) {
            if (op == "=") {
                pass <- (leftVal == rightVal)
            } else if (op == "<") {
                pass <- (leftVal < rightVal)
            } else if (op == "<=") {
                pass <- (leftVal <= rightVal)
            } else if (op == ">") {
                pass <- (leftVal > rightVal)
            } else if (op == ">=") {
                pass <- (leftVal >= rightVal)
            } else if (op == "<>") pass <- (leftVal != rightVal)
        }
        debugPrint("interpretIf: условие =", pass)
        idxThen <- which(sapply(tokens, function(t) t$value) == "THEN")
        if (length(idxThen) == 0) {
            return(list(action = "normal"))
        }
        idxThen <- idxThen[1]
        if (any(sapply(tokens, function(t) t$value) == "ELSE")) {
            idxElse <- which(sapply(tokens, function(t) t$value) == "ELSE")[1]
            branchTokens <- if (pass) tokens[(idxThen + 1):(idxElse - 1)] else tokens[(idxElse + 1):length(tokens)]
        } else {
            branchTokens <- if (pass) tokens[(idxThen + 1):length(tokens)] else list()
        }
        if (length(branchTokens) == 0) {
            return(list(action = "normal"))
        }
        firstWord <- toupper(trimws(branchTokens[[1]]$value))
        if (firstWord %in% c("LEAVE", "CONTINUE")) {
            return(list(action = tolower(firstWord)))
        }
        interpretPut(branchTokens)
        return(list(action = "normal"))
    }

    interpretPut <- function(tokens) {
        debugPrint("interpretPut: tokens =", sapply(tokens, function(t) t$value))
        openP <- which(sapply(tokens, function(t) t$value) == "(")[1]
        closeP <- which(sapply(tokens, function(t) t$value) == ")")[1]
        if (!is.na(openP) && !is.na(closeP) && openP < closeP) {
            mid <- tokens[(openP + 1):(closeP - 1)]
            out <- c()
            for (m in mid) {
                if (m$type == "string_constant") {
                    out <- c(out, gsub("^['\"](.*)['\"]$", "\\1", m$value))
                } else if (m$type == "identifier") {
                    out <- c(out, as.character(getValue(m$value)))
                }
            }
            emit(paste(out, collapse = ""))
        }
    }

    interpretAssignment <- function(tokens) {
        debugPrint("interpretAssignment: tokens =", sapply(tokens, function(t) t$value))
        lhs <- tokens[[1]]$value
        if (length(tokens) >= 5 && tokens[[4]]$value == "+") {
            varA <- tokens[[3]]$value
            plusVal <- tokens[[5]]$value
            L <- parseNumeric(getValue(varA))
            if (is.null(L)) L <- 0
            R <- parseNumeric(plusVal)
            if (is.null(R)) R <- 0
            setValue(lhs, L + R)
            debugPrint("interpretAssignment: присваиваем", lhs, "=", L + R)
        } else if (length(tokens) >= 3 && tokens[[3]]$value == "ADDR") {
            if (length(tokens) >= 6) {
                setPointer(lhs, tokens[[5]]$value)
                debugPrint("interpretAssignment: присвоен указатель", lhs, "->", tokens[[5]]$value)
            }
        } else {
            rhs <- tokens[[3]]$value
            maybeN <- parseNumeric(rhs)
            if (!is.null(maybeN)) {
                setValue(lhs, maybeN)
                debugPrint("interpretAssignment: присваиваем", lhs, "=", maybeN)
            } else {
                vR <- getValue(rhs)
                if (is.null(vR) || is.na(vR)) vR <- rhs
                setValue(lhs, vR)
                debugPrint("interpretAssignment: присваиваем", lhs, "=", vR)
            }
        }
    }

    interpretDeclare <- function(tokens) {
        debugPrint("interpretDeclare: tokens =", sapply(tokens, function(t) t$value))
        if (length(tokens) < 2) {
            return()
        }
        varN <- tokens[[2]]$value
        setValue(varN, NA)
        if (any(sapply(tokens, function(t) t$value) == "BASED")) {
            for (i in seq_along(tokens)) {
                t <- tokens[[i]]
                if (t$type == "keyword" && toupper(t$value) == "BASED" && i + 2 <= length(tokens)) {
                    ptrN <- tokens[[i + 2]]$value
                    declareBased(varN, ptrN)
                    debugPrint("interpretDeclare: BASED", varN, "->", ptrN)
                }
            }
        }
    }

    findMatchingEnd <- function(startPos) {
        for (i in seq(from = startPos, to = length(statements))) {
            t <- statements[[i]]$tokens
            if (length(t) == 1 && toupper(t[[1]]$value) == "END") {
                return(i)
            }
        }
        return(-1)
    }

    parseDoLine <- function(tokens) {
        headerWords <- sapply(tokens, function(t) t$value)
        debugPrint("parseDoLine: headerWords =", headerWords)

        if (any(headerWords == ",")) {
            loopVar <- tokens[[2]]$value
            splitIndex <- NA
            for (i in 4:(length(tokens) - 1)) {
                if (tokens[[i]]$value == loopVar && tokens[[i + 1]]$value == "=") {
                    splitIndex <- i
                    break
                }
            }
            if (is.na(splitIndex)) stop("Не найден разделитель между явным списком и диапазоном в цикле LIST.")
            explicitVals <- c()
            for (i in 4:(splitIndex - 1)) {
                if (tokens[[i]]$type == "numeric_constant") {
                    explicitVals <- c(explicitVals, as.numeric(tokens[[i]]$value))
                }
            }
            rangeTokens <- tokens[splitIndex:length(tokens)]
            if (length(rangeTokens) < 5) stop("Неверный формат диапазонной части цикла.")
            startVal <- as.numeric(rangeTokens[[3]]$value)
            endVal <- as.numeric(rangeTokens[[5]]$value)
            stepVal <- 1
            if ("BY" %in% toupper(sapply(rangeTokens, function(t) t$value))) {
                byIdx <- which(toupper(sapply(rangeTokens, function(t) t$value)) == "BY")[1]
                if (rangeTokens[[byIdx + 1]]$value == "-") {
                    stepVal <- -as.numeric(rangeTokens[[byIdx + 2]]$value)
                } else {
                    stepVal <- as.numeric(rangeTokens[[byIdx + 1]]$value)
                }
            }
            seqVals <- seq(from = startVal, to = endVal, by = stepVal)
            finalVals <- c(explicitVals, seqVals)
            debugPrint("parseDoLine (LIST): for variable", loopVar, "values =", finalVals)
            return(list(type = "LIST", var = loopVar, values = finalVals))
        }

        if (toupper(tokens[[2]]$value) == "WHILE") {
            startParen <- which(headerWords == "(")[1]
            endParen <- which(headerWords == ")")[1]
            conditionTokens <- tokens[(startParen + 1):(endParen - 1)]
            debugPrint("parseDoLine: WHILE condition tokens =", sapply(conditionTokens, function(t) t$value))
            return(list(type = "WHILE", condition = conditionTokens))
        }

        if ("REPEAT" %in% toupper(headerWords)) {
            eqIdx <- which(headerWords == "=")[1]
            repeatIdx <- which(toupper(headerWords) == "REPEAT")[1]
            loopVar <- tokens[[2]]$value
            startVal <- as.numeric(tokens[[eqIdx + 1]]$value)
            incVal <- 1
            if (repeatIdx + 1 <= length(tokens) && tokens[[repeatIdx + 1]]$value == "(") {
                startParen <- repeatIdx + 1
                endParen <- findMatchingParen(tokens, startParen)
                incTokens <- tokens[(startParen + 1):(endParen - 1)]
                rawInc <- evaluateExpression(incTokens, NULL)
                incVal <- rawInc - startVal
                if (is.null(incVal) || !is.numeric(incVal)) incVal <- 1
            }
            if ("WHILE" %in% toupper(headerWords)) {
                allParenIdx <- which(headerWords %in% c("(", ")"))
                if (length(allParenIdx) >= 4) {
                    condStart <- allParenIdx[3]
                    condEnd <- allParenIdx[4]
                    conditionTokens <- tokens[(condStart + 1):(condEnd - 1)]
                } else {
                    conditionTokens <- list()
                }
                return(list(type = "REPEAT_WHILE", var = loopVar, startVal = startVal, inc = incVal, condition = conditionTokens))
            } else {
                return(list(type = "REPEAT", var = loopVar, startVal = startVal, inc = incVal))
            }
        }

        if ("TO" %in% toupper(headerWords)) {
            eqIdx <- which(headerWords == "=")[1]
            toIdx <- which(toupper(headerWords) == "TO")[1]
            loopVar <- tokens[[2]]$value
            startVal <- as.numeric(tokens[[eqIdx + 1]]$value)
            endVal <- as.numeric(tokens[[toIdx + 1]]$value)
            stepVal <- 1
            if ("BY" %in% toupper(headerWords)) {
                byIdx <- which(toupper(headerWords) == "BY")[1]
                if (tokens[[byIdx + 1]]$value == "-") {
                    if (byIdx + 2 <= length(tokens)) stepVal <- -as.numeric(tokens[[byIdx + 2]]$value)
                } else {
                    stepVal <- as.numeric(tokens[[byIdx + 1]]$value)
                }
            }
            if ("WHILE" %in% toupper(headerWords)) {
                whIdx <- which(toupper(headerWords) == "WHILE")[1]
                condCandidates <- which(headerWords == "(" & seq_along(headerWords) > whIdx)
                condStart <- if (length(condCandidates) == 0) NA else condCandidates[1]
                condCandidates2 <- which(headerWords == ")" & seq_along(headerWords) > whIdx)
                condEnd <- if (length(condCandidates2) == 0) NA else condCandidates2[1]
                conditionTokens <- if (is.na(condStart) || is.na(condEnd)) list() else tokens[(condStart + 1):(condEnd - 1)]
                return(list(type = "TO_WHILE", var = loopVar, startVal = startVal, endVal = endVal, step = stepVal, condition = conditionTokens))
            } else {
                return(list(type = "TO", var = loopVar, startVal = startVal, endVal = endVal, step = stepVal))
            }
        }

        return(NULL)
    }

    runDoBlock <- function(def, bodyStart, bodyEnd) {
        debugPrint("runDoBlock: type =", def$type)
        if (def$type == "WHILE") {
            nIter <- 0
            while (TRUE) {
                nIter <- nIter + 1
                if (nIter > 1000) break
                cond <- evaluateCondition(def$condition)
                debugPrint("runDoBlock WHILE: condition =", cond)
                if (!isTRUE(cond)) break
                res <- interpretBlockSlice(bodyStart, bodyEnd)
                if (!is.null(res$action) && res$action == "leave") {
                    return("breakOuter")
                }
            }
        } else if (def$type == "REPEAT") {
            setValue(def$var, def$startVal)
            nIter <- 0
            repeat {
                nIter <- nIter + 1
                if (nIter > 1000) break
                res <- interpretBlockSlice(bodyStart, bodyEnd)
                if (!is.null(res$action) && res$action == "leave") {
                    return("breakOuter")
                }
                cur <- parseNumeric(getValue(def$var))
                if (is.null(cur)) break
                setValue(def$var, cur + def$inc)
            }
        } else if (def$type == "REPEAT_WHILE") {
            setValue(def$var, def$startVal)
            nIter <- 0
            repeat {
                nIter <- nIter + 1
                if (nIter > 1000) break
                cond <- evaluateCondition(def$condition)
                if (!isTRUE(cond)) break
                res <- interpretBlockSlice(bodyStart, bodyEnd)
                if (!is.null(res$action) && res$action == "leave") {
                    return("breakOuter")
                }
                cur <- parseNumeric(getValue(def$var))
                if (is.null(cur)) break
                setValue(def$var, cur + def$inc)
            }
        } else if (def$type == "TO") {
            setValue(def$var, def$startVal)
            nIter <- 0
            repeat {
                nIter <- nIter + 1
                cur <- parseNumeric(getValue(def$var))
                if (is.null(cur)) break
                if (def$startVal <= def$endVal) {
                    if (cur > def$endVal) break
                } else {
                    if (cur < def$endVal) break
                }
                res <- interpretBlockSlice(bodyStart, bodyEnd)
                if (!is.null(res$action) && res$action == "leave") {
                    return("breakOuter")
                }
                setValue(def$var, cur + def$step)
            }
        } else if (def$type == "TO_WHILE") {
            setValue(def$var, def$startVal)
            nIter <- 0
            repeat {
                nIter <- nIter + 1
                cur <- parseNumeric(getValue(def$var))
                if (is.null(cur)) break
                if (def$startVal <= def$endVal) {
                    if (cur > def$endVal) break
                } else {
                    if (cur < def$endVal) break
                }
                cond <- evaluateCondition(def$condition)
                if (!isTRUE(cond)) break
                res <- interpretBlockSlice(bodyStart, bodyEnd)
                if (!is.null(res$action) && res$action == "leave") {
                    return("breakOuter")
                }
                setValue(def$var, cur + def$step)
            }
        } else if (def$type == "LIST") {
            for (val in def$values) {
                setValue(def$var, val)
                res <- interpretBlockSlice(bodyStart, bodyEnd)
                if (!is.null(res$action) && res$action == "leave") {
                    return("breakOuter")
                }
            }
        }
        return("normal")
    }

    interpretBlockSlice <- function(from, to) {
        pc <- from
        while (pc <= to) {
            if (pc < to &&
                toupper(statements[[pc]]$tokens[[1]]$value) == "IF" &&
                toupper(statements[[pc + 1]]$tokens[[1]]$value) == "ELSE") {
                combinedTokens <- c(statements[[pc]]$tokens, statements[[pc + 1]]$tokens)
                debugPrint("interpretBlockSlice: объединяем IF и ELSE:", sapply(combinedTokens, function(t) t$value))
                r <- interpretIf(combinedTokens)
                if (!is.null(r$action) && r$action %in% c("jump", "continue", "leave")) {
                    return(r)
                }
                pc <- pc + 2
                next
            }
            toks <- statements[[pc]]$tokens
            debugPrint("interpretBlockSlice: pc =", pc, "toks =", sapply(toks, function(t) t$value))
            if (length(toks) == 1 && toupper(toks[[1]]$value) == "END") {
                return(list(action = "normal", nextPC = pc + 1))
            }
            if (length(toks) >= 2 && toks[[2]]$value == ":") {
                newTokens <- toks[-c(1, 2)]
                if (length(newTokens) > 0) {
                    r <- interpretOneStatementCustom(newTokens)
                    if (!is.null(r$action) && r$action == "jump") {
                        pc <- r$nextPC
                        next
                    }
                }
                pc <- pc + 1
                next
            }
            r <- interpretOneStatement(pc)
            if (!is.null(r$action) && r$action %in% c("jump", "continue", "leave")) {
                return(r)
            }
            pc <- r$nextPC
        }
        return(list(action = "normal", nextPC = pc))
    }

    interpretOneStatement <- function(pc) {
        tokens <- statements[[pc]]$tokens
        debugPrint("interpretOneStatement: pc =", pc, "tokens =", sapply(tokens, function(t) t$value))
        if (length(tokens) == 0) {
            return(list(action = "normal", nextPC = pc + 1))
        }
        cmd <- toupper(tokens[[1]]$value)
        if (cmd == "END") {
            return(list(action = "normal", nextPC = pc + 1))
        }
        if (cmd == "OPEN") {
            handleOpenFile(tokens)
            return(list(action = "normal", nextPC = pc + 1))
        }
        if (cmd == "IF") {
            r <- interpretIf(tokens)
            return(list(action = r$action, nextPC = pc + 1))
        }
        if (cmd == "PUT") {
            interpretPut(tokens)
            return(list(action = "normal", nextPC = pc + 1))
        }
        if (cmd == "GOTO") {
            labVar <- tokens[[2]]$value
            labVal <- getValue(labVar)
            debugPrint("GOTO: labVar =", labVar, "labVal =", labVal)
            if (!is.null(labVal) && !is.na(labVal)) {
                jumpIdx <- labelIndices[[as.character(labVal)]]
                if (!is.null(jumpIdx)) {
                    return(list(action = "jump", nextPC = jumpIdx))
                }
            }
            return(list(action = "normal", nextPC = pc + 1))
        }
        if (cmd == "CALL") {
            procName <- tokens[[2]]$value
            pval <- getValue(procName)
            if (!is.null(pval) && !is.na(pval)) {
                j <- labelIndices[[as.character(pval)]]
                if (!is.null(j)) {
                    return(list(action = "jump", nextPC = j))
                }
            }
            return(list(action = "normal", nextPC = pc + 1))
        }
        if (cmd == "DECLARE") {
            interpretDeclare(tokens)
            return(list(action = "normal", nextPC = pc + 1))
        }
        if (cmd %in% c("CONTINUE", "LEAVE")) {
            return(list(action = tolower(cmd), nextPC = pc + 1))
        }
        if (tokens[[1]]$type == "identifier" && length(tokens) >= 3 && tokens[[2]]$value == "=") {
            interpretAssignment(tokens)
            return(list(action = "normal", nextPC = pc + 1))
        }
        if (cmd == "DO") {
            endPos <- findMatchingEnd(pc + 1)
            debugPrint("DO: найден END на позиции", endPos)
            if (endPos < 0) {
                return(list(action = "normal", nextPC = pc + 1))
            }
            def <- parseDoLine(tokens)
            debugPrint("DO: def =", def)
            res <- runDoBlock(def, pc + 1, endPos - 1)
            return(list(action = "normal", nextPC = endPos + 1))
        }
        return(list(action = "normal", nextPC = pc + 1))
    }

    interpretOneStatementCustom <- function(tokens) {
        if (length(tokens) == 0) {
            return(list(action = "normal", nextPC = NA))
        }
        cmd <- toupper(tokens[[1]]$value)
        if (cmd == "END") {
            return(list(action = "normal", nextPC = NA))
        }
        if (cmd == "ELSE") {
            return(interpretOneStatementCustom(tokens[-1]))
        }
        if (cmd == "IF") {
            r <- interpretIf(tokens)
            return(list(action = r$action, nextPC = NA))
        }
        if (cmd == "PUT") {
            interpretPut(tokens)
            return(list(action = "normal", nextPC = NA))
        }
        if (cmd == "GOTO") {
            labVar <- tokens[[2]]$value
            labVal <- getValue(labVar)
            if (!is.null(labVal) && !is.na(labVal)) {
                jumpIdx <- labelIndices[[as.character(labVal)]]
                if (!is.null(jumpIdx)) {
                    return(list(action = "jump", nextPC = jumpIdx))
                }
            }
            return(list(action = "normal", nextPC = NA))
        }
        if (cmd == "CALL") {
            procName <- tokens[[2]]$value
            pval <- getValue(procName)
            if (!is.null(pval) && !is.na(pval)) {
                j <- labelIndices[[as.character(pval)]]
                if (!is.null(j)) {
                    return(list(action = "jump", nextPC = j))
                }
            }
            return(list(action = "normal", nextPC = NA))
        }
        if (cmd == "DECLARE") {
            interpretDeclare(tokens)
            return(list(action = "normal", nextPC = NA))
        }
        if (cmd %in% c("CONTINUE", "LEAVE")) {
            return(list(action = tolower(cmd), nextPC = NA))
        }
        if (tokens[[1]]$type == "identifier" && length(tokens) >= 3 && tokens[[2]]$value == "=") {
            interpretAssignment(tokens)
            return(list(action = "normal", nextPC = NA))
        }
        return(list(action = "normal", nextPC = NA))
    }

    interpretBlock <- function(startPC = 1) {
        pc <- startPC
        while (pc <= length(statements)) {
            tokens <- statements[[pc]]$tokens
            debugPrint("interpretBlock: pc =", pc, "tokens =", sapply(tokens, function(t) t$value))
            if (length(tokens) == 0) {
                pc <- pc + 1
                next
            }
            if (length(tokens) == 1 && toupper(tokens[[1]]$value) == "END") {
                return(list(action = "normal", pc = pc + 1))
            }
            if (length(tokens) >= 2 && tokens[[2]]$value == ":") {
                newTokens <- tokens[-c(1, 2)]
                if (length(newTokens) > 0) {
                    r <- interpretOneStatementCustom(newTokens)
                    if (!is.null(r$action) && r$action == "jump") {
                        pc <- r$nextPC
                        next
                    }
                }
                pc <- pc + 1
                next
            }
            r <- interpretOneStatement(pc)
            if (r$action == "jump") {
                pc <- r$nextPC
                next
            }
            if (r$action %in% c("continue", "leave")) {
                return(r)
            }
            pc <- r$nextPC
        }
        return(list(action = "normal", pc = pc))
    }

    if (is.list(ast_obj) && !is.null(ast_obj$type)) {
        statements <- ast_obj$statements
        invisible(interpretBlock(1))
    } else if (is.list(ast_obj) && length(ast_obj) > 0) {
        statements <- ast_obj[[1]]$statements
        invisible(interpretBlock(1))
    }

    emit("=== Интерпретация завершена ===")

    return("Code 1")
}

codeType <- determineCodeType(ast_obj)