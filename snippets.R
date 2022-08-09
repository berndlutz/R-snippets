#
# @TODO
# * Umgang mit "NA" (nicht verfügbar von Werten)

library(stringr)
library(lpSolve)
library("irr")

B_countIf <- function(col, selector) {
  n = 0
  for (val in col) {
    if (!is.na(val) && val == selector) {
      n = n + 1
    }
  }

  percentage = round(n/length(col), digit=3) * 100

  return(list(
    n = n,
    percentage = percentage,
    desc = str_glue("{n} ({percentage}%)")
  ))
}

# e.g. B_countIfs(d$a, 1, d$b, 3) => +1 für a=1 and b=3
B_countIfs <- function(a, ...) {
  n = 0
  r = 1
  p = list(a, ...)
  rows = length(p[[1]])
  while (r < rows) {
    match = TRUE
    pCond = 1
    pCondLength = length(p)
    while (pCond < pCondLength) {
      if (is.na(p[[pCond]][r]) || p[[pCond]][r] != p[pCond+1]) {
        match = FALSE
      }
      pCond = pCond + 2
    }
    if (match == TRUE) {
      n = n + 1
    }
    r = r + 1
  }

  percentage = round(n/length(p[[1]]), digit=3) * 100

  return(list(
    n = n,
    percentage = percentage,
    desc = str_glue("{n} ({percentage}%)")
  ))
}

B_mean <- function(col, digit=1, narm = FALSE) {
  mean = round(mean(col, na.rm = narm), digit=digit)
  sd = round(sd(col, na.rm = narm), digit=digit)
  desc = str_glue("{mean}±{sd}")
  return(list(
    mean = mean,
    sd = sd,
    desc = desc
  ))
}

B_meanStepwise <- function(cols, roundDigit=1) {
  tmpCol = c()
  i = 1
  nRows = length(cols[[1]])

  while (i <= nRows) {
    rowSum = 0
    rowN = 0
    for (col in cols) {
      if (!is.na(col[i])) {
        rowSum = rowSum + col[i]
        rowN = rowN + 1
      }
    }

    if (rowN > 0) {
      av = rowSum / rowN
      tmpCol = append(tmpCol, round(av, digits=roundDigit + 1))
    }
    i = i + 1
  }

  mean = round(mean(tmpCol, na.rm = TRUE), digit=roundDigit)
  sd = round(sd(tmpCol, na.rm = TRUE), digit=roundDigit)
  desc = str_glue("{mean}±{sd}")
  return(list(
    mean = mean,
    sd = sd,
    col = tmpCol,
    desc = desc
  ))
}

B_meanIf <- function(colVal, colFilter, filterSel, digit=1, narm = FALSE) {

  i = 1
  values = c()
  n = length(colVal)

  while (i < n) {
    if (!is.na(colVal[i]) && !is.na(colFilter[i]) && colFilter[i] == filterSel) {
      values = append(values, colVal[i])
    }
    i = i + 1
  }

  if (length(values) > 0) {
    return(B_mean(values, digit = digit))
  }
  else {
    return(list(
      mean = "-",
      sd = "-",
      desc = "-"
    ))
  }
}

B_dateFromGermanFormat <- function(d) {
  p = strsplit(c(d), "[.]")
  p = unlist(p)
  dFormated = paste(p[3], p[2], p[1], sep = "-")
  return(dFormated)
}

B_dateDiff <- function(d1, d2, units="days") {
  years = FALSE
  if (units == "years") {
    years = TRUE
    units = "days"
  }

  diff = as.numeric(difftime(B_dateFromGermanFormat(d2), B_dateFromGermanFormat(d1), units = units))

  if (years == TRUE) {
    diff = round(diff/365, digits = 3)
  }

  return(diff)
}

B_dateDiffCol <- function(col1, col2, units = "days") {
  col = c()
  nCol1 = length(col1)
  i = 1
  while (i <= nCol1) {
    col = append(col, B_dateDiff(col1[i], col2[i], units))
    i = i + 1
  }
  return(col)
}


B_icc <- function(df) {
  r = icc(df, model="twoway", type="agreement", unit="single")
  value = round(r$value, digits = 2)
  pValue = round(r$p.value, digits = 2)
  pDesc = str_glue("p={pValue}")
  if (pValue < 0.01) {
    pValue = "< 0,01"
    pDesc = "p<0,01"
  }
  return(list(
    value = value,
    pValue = pValue,
    desc = str_glue("{value} ({pDesc})")
  ))
}

B_addCol <- function(df, name, indexCol, func) {
  i = 1
  rows = length(df[[indexCol]])
  col = c()
  while (i <= rows) {
    col = append(col, func(df, i))
    i = i + 1
  }

  df[name] = col

  return(df)
}

B_filterDataFrame <- function(df, func, rt, indexCol) {
  i = 1
  rows = length(df[[indexCol]])
  while (i <= rows) {
    rt = func(df, i, rt)
    i = i + 1
  }

  return(rt)
}
