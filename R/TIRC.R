#' HTML Table With Some Extras
#'
#' generic function to print html table.
#' @param x Matrix, data.frame, or table with the data
#' @param title String containing title for table
#' @param align defines the vertical alignment within cells (default All left alligned)
#' @param halign defines the horizontal alignment within cells (default All lower aligned)
#' @param cgroup defines the groupings for columns
#' @param n.cgroup defines the number of columns each cgroup should span. Must sum to total number of columns
#' @param cgroup.just defines horizontal alignment in cell for cgroup
#' @param rgroup defines the groupings for rows. 
#' @param n.rgroup defines the number of rows each rgroup should span.
#' @param rgroupCSSstyle adds css to rgroup
#' @param rgroupCSSseparator css separator for rgroups
#' @param tspanner table spanner
#' @param n.tspanner counts for each table spanner
#' @param tspannerCSSseparator css to be added between table spanners
#' @param rowlabel string to be placed above rownames
#' @param rowlabel.pos defines the position of the row label
#' @param headLines defines how many horizontal lines start the table
#' @param compatibility some html tweaks for different systems
#' @param rnames defines row names. If a single string is given and it matches a column name then that column is used.
#' @param caption string used as caption
#' @param tfoot string used as footer for table
#' @param label adds a css id to table
#' @param zebra highlights alternate rows if TRUE (default FALSE)
#' @param highrows logical indicating rows to highlight or string indicating column containing highlight column
#' @param rgroup_col name of column to use instead of rgroup and n.rgroup
#' @keywords htmlTable html table
#' @export
#' @examples
#' tab_data <- data.frame(rgrp=c("Early", "Early", "Late"),
#'                        rnms = c("A", "B", "C"), 
#'                        Red=c("10 (40%)", "10 (40%)", "5 (20%)"), 
#'                        Green=c("20 (50%)", "16 (40%)", "4 (10%)"))
#'
#' TIRC(tab_data, rnames=F)
#' TIRC(tab_data, rnames="rnms")
#' TIRC(tab_data, rnames="rnms", rgroup_col = "rgrp")
#' 
#' tab_data2 <- data.frame(col=sample(c("Red", "Green"), 100, replace=T), 
#'                         shape=sample(c("Round", "Square"), 100, replace=T))
#' 
#' tab <- with(tab_data2, table(col, shape))
#' TIRC(tab)

TIRC <- function(x, ...) {
  UseMethod("TIRC")
}
## old align: paste(c("l", rep("c", ncol(x) - 1))
## old title: deparse(substitute(x))
## headLines can be single, double or none

#' @rdname TIRC
#' @importFrom stringr str_trim
#' @importFrom stringr str_replace
#' @export

TIRC.default <- function(x, title = "", header, 
                         align = paste(rep("l", ncol(x)), collapse=""), 
                         halign = paste(rep("l", ncol(x)), collapse = ""), cgroup, 
                         n.cgroup, cgroup.just, rgroup, n.rgroup, rgroupCSSstyle = "font-weight: bold", 
                         rgroupCSSseparator = "", tspanner, n.tspanner, tspannerCSSstyle = "font-weight: 900; text-transform:capitalize; text-align: center;", 
                         tspannerCSSseparator = "border-top: 1px solid grey;", rowlabel=title, 
                         rowlabel.pos = "bottom", headLines = "single", compatibility = "LibreOffice", 
                         rnames, caption, caption.loc = "top", tfoot, label, zebra=F, highrows, TURK=F, rgroup_col,...)
{
  if (length(dim(x)) != 2) 
    stop("Your table variable seems to have the wrong dimension, length(dim(x)) = ", 
         length(dim(x)), " != 2")
  if (nrow(x) == 0)
    x[1,] <- NA
  
  fix_name <- function(nm, all_names){
    if(nm %in% all_names) {
      nm <- paste0(nm, " ")
      fix_name(nm, all_names)
    }
    else nm
  }
  
  if(!missing(rgroup_col)){
    if(rgroup_col %in% names(x)) {
      rgroup_var <- as.character(x[[rgroup_col]])
      unique_grps <- c()
      unique_grp_counts <- c()
      in_grp <- F
      tmp_count <- 0
      new_name = ""
      set_new_names = F
      for(i in 1:length(rgroup_var)){
        if(!in_grp){
          tmp_count = 1
          if(rgroup_var[i] %in% unique_grps) {
            set_new_names = T
            new_name <- fix_name(rgroup_var[i], unique_grps)
            x[i, rgroup_col] <- new_name
            unique_grps <- c(unique_grps, new_name)
          } else {
            unique_grps <- c(unique_grps, rgroup_var[i])
          }
        } else {
          tmp_count = tmp_count + 1
          if(set_new_names){
            x[i, rgroup_col] <- new_name
          }
        }
        if(i < length(rgroup_var) & rgroup_var[i+1] != rgroup_var[i]){
          in_grp=F
          set_new_names = F
          unique_grp_counts <- c(unique_grp_counts, tmp_count)
        } else in_grp=T
      }
      unique_grp_counts <- c(unique_grp_counts, tmp_count)
      rgroup <- unique_grps
      n.rgroup <- unique_grp_counts
      x <- x[!names(x) == rgroup_col]
    }
  }
  
  if(!missing(highrows)){
    if(class(highrows)=="character" & length(highrows)==1){
      highrows = x[[highrows]]
      x[[highrows]] <- NULL
    }
  }
  
  if (missing(rnames)) {
    if (any(is.null(rownames(x)) == FALSE)) 
      rnames <- rownames(x)
  } else if (length(rnames)==1) {
    if(rnames == F) {
      rnames <- rep("", nrow(x))
    } else if(rnames %in% names(x)){
      tmp_rnames <- rnames
      rnames <- as.character(x[[rnames]])
      x <- x[names(x) != tmp_rnames]
    }
  } else if (any(is.null(rownames(x))) && !missing(rgroup)) 
    warning("You have not specified rownames but you seem to have rgroups.", 
            "If you have the first column as rnames but you want the rgroups", 
            "to result in subhedings with indentation below then", 
            "you should change the rownames to the first column and then", 
            "remove it from the table matrix (the x argument object).")
  if (!missing(rnames)) 
    set_rownames <- TRUE
  else set_rownames <- FALSE
  
  if (missing(header))
    header = colnames(x)
  
  if (length(align) > 1) 
    align <- paste(align, collapse = "")
  if (tolower(compatibility) %in% c("libreoffice", "libre office", 
                                    "open office", "openoffice", "word", "ms word", "msword")) 
    compatibility <- "LibreOffice"
  tc <- getOption("table_counter")
  if (is.null(tc)) {
    tc_string <- ""
  }
  else {
    if (is.numeric(tc)) 
      tc <- tc + 1
    else tc <- 1
    options(table_counter = tc)
    table_template <- getOption("table_counter_str", "Table %s: ")
    tc_string <- sprintf(table_template, ifelse(getOption("table_counter_roman", 
                                                          FALSE), as.character(as.roman(tc)), as.character(tc)))
  }
  addSemicolon2StrEnd <- function(my_str) {
    my_str <- str_trim(my_str)
    if (substr(my_str, nchar(my_str), nchar(my_str) + 1) != 
        ";") 
      my_str <- sprintf("%s;", my_str)
    my_str <- gsub(";;+", ";", my_str)
    if (my_str == ";") 
      return("")
    return(my_str)
  }
  addAlign2Style <- function(style, align) {
    if (grepl("text-align", style)) {
      return(gsub("text-align[ ]*:([^;]+)", paste0("text-align: ", 
                                                   align), style))
    }
    else {
      if (nchar(style) == 0) 
        return(sprintf("text-align: %s;", align))
      if (!grepl(";$", style)) 
        style <- sprintf("%s;", style)
      return(sprintf("%s text-align: %s;", style, align))
    }
  }
  addCells <- function(table_str, rowcells, cellcode, align, 
                       style = "") {
    cellcode.end <- ifelse(cellcode=="td class='subLast'", "td", cellcode)
    style = addSemicolon2StrEnd(style)
    for (nr in 1:length(rowcells)) {
      cell_value <- rowcells[[nr]]
      if (is.na(cell_value)) 
        cell_value <- ""
      table_str <- sprintf("%s\n<%s style='%s'>%s</%s>", 
                           table_str, cellcode, addAlign2Style(style, align[nr]), 
                           cell_value, cellcode.end)
      if (nr != length(rowcells) && cgroup_spacer_cells[nr] > 
          0) {
        table_str <- sprintf("%s\n<%s style='%s' colspan='%d'>&nbsp;</%s>", 
                             table_str, cellcode, style, cgroup_spacer_cells[nr], 
                             cellcode.end)
      }
    }
    return(table_str)
  }
  getAlign <- function(align_req) {
    tmp_align_req <- strsplit(align_req, "")[[1]]
    if (length(grep("[|]", tmp_align_req)) > 0) {
      tmp_align_req <- tmp_align_req[-grep("[|]", tmp_align_req)]
    }
    sapply(tmp_align_req, function(f) c("center", "right", 
                                        "left")[grep(f, c("c", "r", "l"))], USE.NAMES = FALSE)
  }
  setRowLabel <- function() {
    if (set_rownames && length(rowlabel) > 0 && nchar(rowlabel)) 
      return(TRUE)
    else return(FALSE)
  }
  getCgroupHeader <- function(cgroup_vec, n.cgroup_vec, cgroup_vec.just, 
                              top_row = TRUE, row_no) {
    header_str <- "\n<tr>"
    if (top_row) 
      ts <- top_row_style
    else ts <- ""
    if (setRowLabel()) {
      if (row_no == rowlabel.pos) 
        header_str <- sprintf("%s\n<th style='%s'>%s</th>", 
                              header_str, ts, rowlabel)
      else header_str <- sprintf("%s\n<th style='%s'></th>", 
                                 header_str, ts)
    }
    else if (set_rownames) {
      header_str <- sprintf("%s\n<th style='%s'></th>", 
                            header_str, ts)
    }
    for (i in 1:length(cgroup_vec)) {
      if (!is.na(n.cgroup_vec[i])) {
        start_column <- ifelse(i == 1, 1, sum(n.cgroup_vec[1:(i - 
                                                                1)], na.rm = TRUE) + 1)
        colspan <- n.cgroup_vec[i] + ifelse(start_column > 
                                              length(cgroup_spacer_cells) || n.cgroup_vec[i] == 
                                              1, 0, ifelse(start_column == 1, sum(cgroup_spacer_cells[1:(n.cgroup_vec[i] - 
                                                                                                           1)]), ifelse(sum(n.cgroup_vec[1:i], na.rm = TRUE) == 
                                                                                                                          ncol(x), sum(cgroup_spacer_cells[start_column:length(cgroup_spacer_cells)]), 
                                                                                                                        sum(cgroup_spacer_cells[start_column:((start_column - 
                                                                                                                                                                 1) + (n.cgroup_vec[i] - 1))]))))
        if (nchar(cgroup_vec[i]) == 0) 
          header_str <- sprintf("%s\n<th colspan='%d' style='%s'>&nbsp;</th>", 
                                header_str, colspan, addAlign2Style(sprintf("%s", 
                                                                            ts), getAlign(strsplit(cgroup_vec.just, 
                                                                                                   "")[[1]][i])))
        else header_str <- sprintf("%s\n<th colspan='%d' style='border-bottom: 1px solid grey; %s'>%s</th>", 
                                   header_str, colspan, ts, cgroup_vec[i])
        if (i != sum(!is.na(cgroup_vec))) 
          header_str <- sprintf("%s<th style='%s; border-bottom: hidden;'>&nbsp;</th>", 
                                header_str, ts)
      }
    }
    header_str <- sprintf("%s\n</tr>", header_str)
    return(header_str)
  }
  
  if (!missing(rgroup)) {
    if (missing(n.rgroup)) 
      stop("You need to specify the argument n.rgroup if you want to use rgroups")
    if (sum(n.rgroup) != nrow(x)) 
      stop(sprintf("Your rows don't match in the n.rgroup, i.e. %d != %d", 
                   sum(n.rgroup), nrow(x)))
    if (length(rgroupCSSstyle) > 1 && length(rgroupCSSstyle) != 
        length(rgroup)) 
      stop(sprintf("You must provide the same number of styles as the rgroups, %d != %d", 
                   length(rgroupCSSstyle), length(rgroup)))
    else if (length(rgroupCSSstyle) == 1) {
      rgroupCSSstyle <- addSemicolon2StrEnd(rgroupCSSstyle)
      if (length(rgroup) > 0) 
        rgroupCSSstyle <- rep(rgroupCSSstyle, length.out = length(rgroup))
    }
    else {
      for (i in 1:length(rgroupCSSstyle)) rgroupCSSstyle[i] <- addSemicolon2StrEnd(rgroupCSSstyle[i])
    }
    if (length(rgroupCSSseparator) > 1 && length(rgroupCSSseparator) != 
        length(rgroup) - 1) 
      stop(sprintf("You must provide the same number of separators as the rgroups - 1, %d != %d", 
                   length(rgroupCSSseparator), length(rgroup) - 
                     1))
    else if (length(rgroupCSSseparator) == 1) {
      rgroupCSSseparator <- addSemicolon2StrEnd(rgroupCSSseparator)
      if (length(rgroup) > 0) 
        rgroupCSSseparator <- rep(rgroupCSSseparator, 
                                  length.out = length(rgroup))
    }
    else {
      for (i in 1:length(rgroupCSSseparator)) rgroupCSSseparator[i] <- addSemicolon2StrEnd(rgroupCSSseparator[i])
    }
  }
  if (!missing(tspanner)) {
    if (length(tspannerCSSstyle) > 1 && length(tspannerCSSstyle) != 
        length(tspanner)) 
      stop(sprintf("You must provide the same number of styles as the tspanners, %d != %d", 
                   length(tspannerCSSstyle), length(tspanner)))
    else if (length(tspannerCSSstyle) == 1) {
      tspannerCSSstyle <- addSemicolon2StrEnd(tspannerCSSstyle)
      if (length(tspanner) > 0) 
        tspannerCSSstyle <- rep(tspannerCSSstyle, length.out = length(tspanner))
    }
    else {
      for (i in 1:length(tspannerCSSstyle)) tspannerCSSstyle[i] <- addSemicolon2StrEnd(tspannerCSSstyle[i])
    }
    if (length(tspannerCSSseparator) > 1 && length(tspannerCSSseparator) != 
        length(tspanner) - 1) 
      stop(sprintf("You must provide the same number of separators as the tspanners - 1, %d != %d", 
                   length(tspannerCSSseparator), length(tspanner) - 
                     1))
    else if (length(tspannerCSSseparator) == 1) {
      tspannerCSSseparator <- addSemicolon2StrEnd(tspannerCSSseparator)
      if (length(tspanner) > 0) 
        tspannerCSSseparator <- rep(tspannerCSSseparator, 
                                    length.out = length(tspanner))
    }
    else {
      for (i in 1:length(tspannerCSSseparator)) tspannerCSSseparator[i] <- addSemicolon2StrEnd(tspannerCSSseparator[i])
    }
  }
  if (!missing(tspanner)) {
    if (missing(n.tspanner)) 
      stop("You need to specify the argument n.tspanner if you want to use table spanners")
    if (sum(n.tspanner) != nrow(x)) 
      stop(sprintf("Your rows don't match in the n.tspanner, i.e. %d != %d", 
                   sum(n.rgroup), nrow(x)))
    if (!missing(n.rgroup)) {
      for (i in 1:length(n.tspanner)) {
        rows <- sum(n.tspanner[1:i])
        if (!rows %in% cumsum(n.rgroup)) 
          stop("There is no splitter that matches the table spanner ", 
               tspanner[i], " (no. ", i, ") with rgroup splits.", 
               " The missing row splitter should be on row number ", 
               rows, " and is not in the n.rgroup list: ", 
               paste(n.rgroup, collapse = ", "), " note, it should match the cumulative sum n.rgroup", 
               paste(cumsum(n.rgroup), collapse = ", "))
      }
    }
  }
  cgroup_spacer_cells <- rep(0, times = (ncol(x) - 1))
  if (!missing(cgroup)) {
    if (!is.matrix(cgroup)) {
      cgroup <- matrix(cgroup, nrow = 1)
      if (missing(n.cgroup)) 
        n.cgroup <- matrix(NA, nrow = 1)
      else {
        if (ncol(cgroup) != length(n.cgroup)) {
          n.cgroup <- n.cgroup[n.cgroup > 0]
          if (ncol(cgroup) != length(n.cgroup)) 
            stop("You have provided invalid n.cgroup,", 
                 " it should have the same length as the cgroup (", 
                 ncol(cgroup), ")", " but it has the length of ", 
                 length(n.cgroup))
        }
        n.cgroup <- matrix(n.cgroup, nrow = 1)
      }
    }
    else if (missing(n.cgroup)) {
      stop("If you specify the cgroup argument as a matrix you have to", 
           " at the same time specify the n.cgroup argument.")
    }
    if (missing(cgroup.just)) {
      cgroup.just <- matrix(paste(rep("c", times = length(n.cgroup)), 
                                  collapse = ""), nrow = nrow(n.cgroup))
    }
    else {
      if (!is.matrix(cgroup.just)) 
        cgroup.just <- matrix(cgroup.just, ncol = 1)
      if (nrow(cgroup.just) != nrow(n.cgroup)) 
        stop("You have different dimensions for your cgroup.just and your cgroups, ", 
             nchar(sub("\\|", "", cgroup.just[1, 1])), "x", 
             ncol(cgroup.just), " for the just while the cgroup has ", 
             nrow(cgroup), "x", ncol(cgroup))
      if (ncol(cgroup.just) > 1) 
        cgroup.just <- as.matrix(apply(cgroup.just, 1, 
                                       function(x) paste(ifelse(is.na(x), "", x), 
                                                         collapse = "")), ncol = 1)
      discrepancies <- which(apply(cgroup.just, 1, function(x) nchar(sub("|", 
                                                                         "", x))) != rowSums(!is.na(cgroup)))
      if (length(discrepancies) > 0) 
        stop("You seem to have different number of justifications in your cgroup.just as compared to your cgroup variable.", 
             " There is a discrepancy regarding rows: ", 
             paste(discrepancies, collapse = ", "))
    }
    for (i in nrow(cgroup):1) {
      if (all(is.na(n.cgroup[i, ])) && ncol(x)%%length(cgroup[i, 
                                                              ]) == 0) {
        n.cgroup[i, ] <- rep(ncol(x)/length(cgroup[i, 
                                                   ]), times = length(cgroup[i, ]))
      }
      else if (sum(n.cgroup[i, ], na.rm = TRUE) != ncol(x)) {
        ncgroupFixFromBelowGroup <- function(nc, i) {
          if (i + 1 > nrow(nc)) 
            stop("You have provided an invalid nc", " where it has fewer rows than the one of interest")
          row_below <- nc[i + 1, !is.na(nc[i + 1, ])]
          start_pos <- 1
          for (ii in 1:ncol(nc)) {
            if (!is.na(nc[i, ii])) {
              pos <- ifelse(any(start_pos > cumsum(row_below)), 
                            tail(which(start_pos > cumsum(row_below)), 
                                 1) + 1, 1)
              nc[i, ii] <- sum(row_below[pos:(pos + nc[i, 
                                                       ii] - 1)])
              start_pos <- start_pos + nc[i, ii]
            }
          }
          return(nc)
        }
        if (i < nrow(cgroup) && sum(n.cgroup[i, ], na.rm = TRUE) == 
            sum(!is.na(n.cgroup[i + 1, ]))) {
          n.cgroup <- ncgroupFixFromBelowGroup(n.cgroup, 
                                               i)
        }
        else {
          stop(sprintf("Your columns don't match in the n.cgroup for the %d header row, i.e. %d != %d", 
                       i, sum(n.cgroup[i, ], na.rm = TRUE), ncol(x)))
        }
      }
      if (!all(is.na(n.cgroup[i, ]) == is.na(cgroup[i, 
                                                    ]))) {
        stop("On header row (the cgroup argument) no ", 
             i, " you fail to get the NA's matching.", "\n  The n.cgroup has elements no:", 
             sprintf(" '%s'", paste(which(is.na(n.cgroup[i, 
                                                         ])), collapse = ", ")), " missing while cgroup has elements no:", 
             sprintf(" '%s'", paste(which(is.na(cgroup[i, 
                                                       ])), collapse = ", ")), " missing.", "\n If the NA's don't occur at the same point", 
             " the software can't decide what belongs where.", 
             "\n The full cgroup row: ", paste(cgroup[i, 
                                                      ], collapse = ", "), "\n The full n.cgroup row: ", 
             paste(n.cgroup[i, ], collapse = ", "), "\n Example: for a two row cgroup it would be:", 
             " n.cgroup = rbind(c(1, NA), c(2, 1)) and", 
             " cgroup = rbind(c('a', NA), c('b', 'c'))")
      }
      for (ii in 1:(length(n.cgroup[i, ]) - 1)) {
        if (!is.na(n.cgroup[i, ii]) && sum(n.cgroup[i, 
                                                    1:ii], na.rm = TRUE) <= length(cgroup_spacer_cells)) 
          cgroup_spacer_cells[sum(n.cgroup[i, 1:ii], 
                                  na.rm = TRUE)] <- 1
      }
    }
  }
  no_cgroup_rows <- ifelse(!missing(cgroup), nrow(cgroup), 
                           0)
  if (is.numeric(rowlabel.pos)) {
    if (rowlabel.pos < 1) 
      stop("You have specified a rowlabel.pos that is less than 1: ", 
           rowlabel.pos)
    else if (rowlabel.pos > no_cgroup_rows + (!missing(header)) * 
             1) 
      stop("You have specified a rowlabel.pos that more than the max limit, ", 
           no_cgroup_rows + (!missing(header)) * 1, ", you have provided: ", 
           rowlabel.pos)
  }
  else {
    rowlabel.pos <- tolower(rowlabel.pos)
    if (rowlabel.pos %in% c("top")) 
      rowlabel.pos <- 1
    else if (rowlabel.pos %in% c("bottom", "header")) 
      rowlabel.pos <- no_cgroup_rows + (!missing(header)) * 
        1
    else stop("You have provided an invalid rowlabel.pos text, only 'top', 'bottom' or 'header' are allowed, can't interpret '", 
              rowlabel.pos, "'")
  }
  #     if (length(list(...)) > 0) 
  #         x <- format.df(x, numeric.dollar = FALSE, ...)
  if (is.character(x)) 
    x <- matrix(str_replace(x, "\\\\%", "%"), ncol = ncol(x))
  table_id <- ""
  if (!missing(label)) {
    table_id <- sprintf(" id='%s'", label)
  }
  else if (is.numeric(tc)) {
    table_id <- sprintf(" id='table_%d'", tc)
  }
  total_columns <- ncol(x) + set_rownames
  if (!missing(cgroup)) {
    if (!is.matrix(cgroup)) {
      total_columns <- total_columns + length(cgroup) - 1
    }
    else {
      total_columns <- total_columns + sum(cgroup_spacer_cells)
    }
  }
  
  zebra_str <- "table.zebra tr:nth-of-type(even) td {background-color: #DDE8F0;}
  table.zebra tr:nth-of-type(even) td[class='rgHead'] {background-color: transparent}
  table.zebra tr td[class='subLast']{border-bottom: 1px solid black}"
  
  table_str <- paste("<style type='text/css'>", ifelse(zebra, zebra_str,""), "</style>", sep="\n")
  
  styl=paste0("'tirc_table", ifelse(zebra, " zebra",""), "'")
  
  table_str <- sprintf("%s<table class=%s style='border-collapse: collapse;' %s>", 
                       table_str, styl, table_id)
  first_row = TRUE
  if (headLines == "none") {
    top_row_style = "border-top: none;"
    bottom_row_style = "border-bottom: 1px solid grey;"
  } else if (headLines == "double") {
    top_row_style = "border-top: 4px double grey;"
    bottom_row_style = "border-bottom: 1px solid grey;"
  } else {
    top_row_style = "border-top: 2px solid grey;"
    bottom_row_style = "border-bottom: 2px solid grey;"
  }
  if (!missing(caption)) {
    caption <- sprintf("\n%s%s", tc_string, caption)
  }
  if (!missing(caption) & compatibility != "LibreOffice") {
    if (caption.loc == "bottom") {
      table_str <- sprintf("%s\n<caption style='caption-side: bottom'>", 
                           table_str)
    }
    else {
      table_str <- sprintf("%s\n<caption style='caption-side: top'>", 
                           table_str)
    }
    table_str <- sprintf("%s%s</caption>", table_str, caption)
  }
  rowname_align <- getAlign("l")
  if (set_rownames && nchar(align) - 1 == ncol(x)) {
    rowname_align <- getAlign(substr(align, 1, 1))
    align <- substring(align, 2)
  }
  table_str <- sprintf("%s\n<thead class=tirc_head>", table_str)
  if (!missing(caption) & compatibility == "LibreOffice" & 
      caption.loc != "bottom") {
    table_str <- sprintf("%s\n<tr><td colspan='%d' style='text-align: left;'>%s</td></tr>", 
                         table_str, total_columns, caption)
  }
  if (!missing(cgroup)) {
    for (i in 1:nrow(cgroup)) {
      table_str <- sprintf("%s%s", table_str, getCgroupHeader(cgroup_vec = cgroup[i, 
                                                                                  ], n.cgroup_vec = n.cgroup[i, ], cgroup_vec.just = cgroup.just[i, 
                                                                                                                                                 ], top_row = (i == 1), row_no = i))
    }
    first_row = FALSE
  }
  if (!missing(header)) {
    table_str <- sprintf("%s\n<tr>", table_str)
    ts <- ifelse(no_cgroup_rows > 0, "", top_row_style)
    if (setRowLabel() && rowlabel.pos == no_cgroup_rows + 
        1) {
      table_str <- sprintf("%s\n<th style='border-bottom: 1px solid grey; %s'>%s</th>", 
                           table_str, ts, rowlabel)
    }
    else if (set_rownames) {
      table_str <- sprintf("%s\n<th style='border-bottom: 1px solid grey; %s'>&nbsp;</th>", 
                           table_str, ts)
    }
    cell_style = "border-bottom: 1px solid grey;"
    if (first_row) {
      cell_style = sprintf("%s %s", cell_style, top_row_style)
    }
    table_str <- addCells(table_str = table_str, rowcells = header, 
                          cellcode = "th", align = getAlign(halign), style = cell_style)
    table_str <- sprintf("%s\n</tr>", table_str)
    first_row = FALSE
  }
  table_str <- sprintf("%s\n</thead><tbody>", table_str)
  rgroup_iterator <- 0
  tspanner_iterator <- 0
  for (row_nr in 1:nrow(x)) {
    
    if (!missing(tspanner) && (row_nr == 1 || row_nr > sum(n.tspanner[1:tspanner_iterator]))) {
      tspanner_iterator = tspanner_iterator + 1
      rs <- tspannerCSSstyle[tspanner_iterator]
      if (tspanner_iterator > 1) {
        rs <- sprintf("%s %s", rs, tspannerCSSseparator[tspanner_iterator - 1])
      }
      table_str <- sprintf("%s\n<tr><td colspan='%d' style='%s'>%s</td></tr>", 
                           table_str, total_columns, rs, tspanner[tspanner_iterator])
    }
    if (!missing(rgroup) && (row_nr == 1 || row_nr > sum(n.rgroup[1:rgroup_iterator]))) {
      nexttd <- "td class='sub'"
      rgroup_iterator = rgroup_iterator + 1
      rs <- rgroupCSSstyle[rgroup_iterator]
      if (rgroup_iterator > 1) {
        rs <- sprintf("%s %s", rs, rgroupCSSseparator[rgroup_iterator - 
                                                        1])
      }
      if (is.na(rgroup[rgroup_iterator]) == FALSE && rgroup[rgroup_iterator] != 
          "") {
        table_str <- sprintf("%s\n<tr><td class='rgHead' colspan='%d' style='%s'>%s</td></tr>", 
                             table_str, total_columns, rs, rgroup[rgroup_iterator])
      }
    } else nexttd <- "td"
    if (!missing(rgroup) && (row_nr == cumsum(n.rgroup)[rgroup_iterator])) {
      nexttd <- "td class='subLast'"
    } else nexttd <- "td"
    # table_str <- sprintf("%s\n%s", table_str,nexttr)
    high <- ifelse(missing(highrows), "", ifelse(highrows[row_nr], "class=high_row", ""))
    table_str <- sprintf("%s\n<tr %s>", table_str, high)
    cell_style = ""
    if (row_nr == nrow(x)) 
      cell_style = bottom_row_style
    if (set_rownames) {
      if (!missing(rgroup) && rgroup_iterator > 0 && is.na(rgroup[rgroup_iterator]) == 
          FALSE && rgroup[rgroup_iterator] != "") {
        table_str <- sprintf("%s\n<%s style='padding-left:16px; %s'>%s</td>", 
                             table_str, nexttd, addAlign2Style(cell_style, rowname_align), 
                             rnames[row_nr])
      }
      else table_str <- sprintf("%s\n<%s style='%s'>%s</td>", 
                                table_str, nexttd, addAlign2Style(cell_style, rowname_align), 
                                rnames[row_nr])
    }
    table_str <- addCells(table_str = table_str, rowcells = x[row_nr,], 
                          cellcode = nexttd, align = getAlign(align), style = cell_style)
    table_str <- sprintf("%s\n</tr>", table_str)
  }
  table_str <- sprintf("%s\n</tbody>", table_str)
  if (!missing(caption) & compatibility == "LibreOffice" & 
      caption.loc == "bottom") {
    table_str <- sprintf("%s\n<tr><td colspan='%d' style='text-align: left;'>%s</td></tr>", 
                         table_str, total_columns, caption)
  }
  if (!missing(tfoot)) {
    table_str <- sprintf("%s\n<tfoot><tr><td colspan=%d>", 
                         table_str, total_columns)
    table_str <- sprintf("%s\n%s", table_str, tfoot)
    table_str <- sprintf("%s</td></tr></tfoot>", table_str)
  }
  table_str <- sprintf("%s\n</table>", table_str)
  class(table_str) <- c("TIRC", class(table_str))
  return(table_str)
}


#' @rdname TIRC
#' @export

TIRC.table <- function(x, title = "", header, 
                       align = paste(rep("l", ncol(x)), collapse=""), 
                       halign = paste(rep("l", ncol(x)), collapse = ""), cgroup, 
                       n.cgroup, cgroup.just, rgroup, n.rgroup, rgroupCSSstyle = "font-weight: bold", 
                       rgroupCSSseparator = "", tspanner, n.tspanner, tspannerCSSstyle = "font-weight: 900; text-transform:capitalize; text-align: center;", 
                       tspannerCSSseparator = "border-top: 1px solid grey;", rowlabel=title, 
                       rowlabel.pos = "bottom", headLines = "single", compatibility = "LibreOffice", 
                       rnames, caption, caption.loc = "top", tfoot, label, zebra=F, highrows, TURK=F,...) {
  if(length(dim(x)) == 1){
    ## a single vector table will be printed long.
    tmp <- data.frame(x)
    TIRC.default(tmp, rnames="Var1") ## calculated variables won't work because number of columns has changed.
    # NextMethod("TIRC", x=tmp, rnames="Var1")
  } else {
    tmp <- as.data.frame.matrix(x, stringsAsFactors = F)
    tmpnames <- names(dimnames(x))
    
    if(missing(rowlabel)) rowlabel <- tmpnames[1]
    if(missing(cgroup)) {
      cgroup <- tmpnames[2]
      n.cgroup <- ncol(tmp)
    }
    NextMethod("TIRC", x=tmp, rowlabel=rowlabel, cgroup=cgroup, n.cgroup=n.cgroup, rnames=rownames(tmp))
  }
}

#' @rdname TIRC
#' @export
#' @importFrom utils browseURL

print.TIRC <- function(x, useViewer, ...){
  args <- attr(x, "...")
  # Use the latest ... from the print call
  # and override the original htmlTable call ...
  # if there is a conflict
  print_args <- list(...)
  for (n in names(print_args)){
    args[[n]] <- print_args[[n]]
  }
  
  # Since the print may be called from another print function
  # it may be handy to allow functions to use attributes for the
  # useViewer parameter
  if (missing(useViewer)){
    if ("useViewer" %in% names(args) &&
        (is.logical(args$useViewer) ||
         is.function(args$useViewer))){
      useViewer <- args$useViewer
      args$useViewer <- NULL
    }else{
      useViewer <- TRUE
    }
  }
  
  if (interactive() &&
      !getOption("htmlTable.cat", FALSE) &&
      (is.function(useViewer) ||
       useViewer != FALSE))
  {
    if (is.null(args$file)){
      args$file <- tempfile(fileext=".html")
    }
    
    spacing_styl <- "
    <style type='text/css'>
    table, td, th {border: none; padding-right: 6px; padding-left: 6px;}
    </style>"
    
    htmlPage <- paste(spacing_styl,
                      "<html>",
                      "<head>",
                      "<meta http-equiv=\"Content-type\" content=\"text/html; charset=UTF-8\">",
                      "</head>",
                      "<body>",
                      "<div style=\"margin: 0 auto; display: table; margin-top: 1em; padding-top: 10px;\">",
                      x,
                      "</div>",
                      "</body>",
                      "</html>", sep="\n")
    # We only want to use those arguments that are actually in cat
    # anything else that may have inadvertadly slipped in should
    # be ignored or it will be added to the output
    cat_args <- args
    cat_args <- cat_args[names(cat_args) %in% names(formals(cat))[-1]]
    cat_args$html <- htmlPage
    do.call(cat, cat_args)
    
    if (is.function(useViewer)){
      useViewer(args$file)
    }else{
      viewer <- getOption("viewer")
      if (!is.null(viewer) &&
          is.function(viewer)){
        # (code to write some content to the file)
        viewer(args$file)
      }else{
        utils::browseURL(args$file)
      }
    }
  }else{
    
    cat_args <- args
    cat_args <- cat_args[names(cat_args) %in% names(formals(cat))[-1]]
    cat_args$html <- x
    do.call(cat, cat_args)
  }
  
  invisible(x)
}
