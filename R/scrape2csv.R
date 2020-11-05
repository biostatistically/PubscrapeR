#' Scrape PubMed, Compile information, and Export to csv
#'
#' Queries PubMed using the extract_PubMed_data from the businessPubMed package.
#' If a journal list is defined, the query will be performed iteratively through a loop.
#' Results are compiled and exported to csv.
#'
#' @param narrow Character vector of length 1: key search terms in double quotes using AND as the operator between terms 
#'   all enclosed within parentheses
#' @param broad Character vector of length 1: key search terms in quotes using OR as the operator between terms
#'   all enclosed within parentheses
#' @param operator Character string: Defines the boolean operator (AND, OR, NOT) between narrow and broad 
#'   (AND is default)
#' @param journals Character vector of length j: journals to query search terms through a loop
#' @param start Character string: start publication date for the query YYYY, or YYYY/MM, or YYYY/MM/DD format
#' @param end Character string: end publication date for the query YYYY, or YYYY/MM, or YYYY/MM/DD format 
#'   (default is 3000/12/31)
#' @param title Character string: title of the search,`used as prefix to output
#' @param outpath Character string: path for the output
#' @param newfolder If `TRUE`, a new folder is created for the output (default is TRUE)
#' @param raw If `TRUE`, raw query results are exported to csv (default is TRUE)
#' @param clist If `TRUE`, a list of author email contacts cleaned of duplicates is exported
#'   (default is TRUE)
#' @param alist If `TRUE`, a list of authors resulting from the query is exported (default is TRUE)
#' @param plist If `TRUE`, a list of publication titles resulting from the query is exported (default is TRUE)
#' @param jlist If `TRUE`, and `journals` is not specified, a list of journals resulting from the query
#'   is exported (default is FALSE)
#' @param report If `TRUE`, a query report is exported to csv (default is TRUE)
#' @import XML
#' @import businessPubMed
#' @import easyPubMed
#' @import dplyr
#' @importFrom tidyr drop_na
#' @importFrom here here
#' @export
#' @examples 
#' my.keywords = '("COVID-19" AND "coronavirus")'
#' my.keyphrases = '("ARDS" OR "acute respiratory distress syndrome")'
#' my.journals = c("JAMA Cardiol", "Allergy Asthma Clin Immunol", "N Engl J Med")
#' my.title = "Covid"
#' my.start = "2019"
#' scrape2csv(narrow = my.keywords, broad = my.keyphrases, operator = "OR", journals = my.journals, 
#'   start = my.start, title = my.title)
#' # searches (("COVID-19" AND "coronavirus") OR ("ARDS" OR "acute respiratory distress syndrome")) 
#' #   from 2019 on in 3 selected journals
scrape2csv <- function(narrow, 
                       broad=NULL,
                       operator=NULL,
                       journals=NULL, 
                       start, 
                       end=NULL, 
                       title=NULL, 
                       outpath=NULL, 
                       newfolder=TRUE, 
                       raw=TRUE,
                       clist=TRUE,
                       alist=FALSE,
                       plist=FALSE,
                       jlist=FALSE,
                       report=TRUE) 
{
  if (is.null(narrow) | length(narrow) == 0) {
    stop("`narrow` is required. `broad` is optional.")
  }
  if (is.null(operator)) {
    operator <- "AND"
  }
  if (is.null(title)) {
    title <- "Untitled"
  }
  if (is.null(end)) {
    end <- "3000/12/31" # This is the PubMed default
  }
  # set a timestamp
  stime <- Sys.time()
  
  ## BEGIN SINGLE QUERY WITHOUT SPECIFICATION OF JOURNALS
  if (is.null(journals) | length(journals) == 0) {
    if (is.null(broad)) {
      my.query <- paste(narrow, ' AND (', start, ':', end,'[dp])"', sep = '')
    } else {
      my.query <- paste('(', narrow, ' ', operator,' ', broad, ') ', 'AND (', start, ':', end,'[dp])"', sep = '')
    }
    cat("\n", "Querying PubMed for", my.query)
    
    t_init <- Sys.time()
    stop_process <- FALSE
    my.data <- tryCatch({
      businessPubMed::extract_pubMed_data(pubMed_query = my.query,
                                          batch_size = 1000,
                                          affi_regex_exclude = NULL)
    },
    error = function(e) {
      message(paste0("\nQuery produces 0 results."))
      stop_process <<- TRUE
    })
    
    if (stop_process) { 
      stop("Refine search terms and try again.") 
    }
    
    t_final <- Sys.time()
    t_diff <- t_final - t_init
    
    # Number of unique journals?
    n.journals <- sum(!is.na(unique(my.data$data$journal)))
    
    # Number of unique publication titles?
    n.articles <- sum(!is.na(unique(my.data$data$title)))
    
    # Number of unique authors?
    n.authors <- sum(!is.na(unique(paste(my.data$data$firstname, 
                                         my.data$data$lastname, sep = " "))))
    
    # How many unique affiliations?
    n.affiliations <- sum(!is.na(unique(my.data$data$address)))
    
    # How many unique emails?
    n.emails <- sum(!is.na(unique(my.data$data$email)))
    
    # How many unique contacts?
    n.contacts <- sum(!is.na(unique(ifelse(is.na(my.data$data$email), NA, paste(my.data$data$firstname, 
                                                                                my.data$data$lastname, 
                                                                                my.data$data$email, 
                                                                                sep = " ")))))
    
    stats_report <- data.frame("status" = "Success",
                               "search.time" = t_diff,
                               "n.journals" = n.journals,
                               "n.articles" = n.articles, 
                               "n.authors" = n.authors, 
                               "n.affiliations" = n.affiliations, 
                               "n.emails" = n.emails,
                               "n.contacts" = n.contacts) 
    
    if (is.null(outpath)) {
      outpath <- here::here()
      if (newfolder) {
        suppressWarnings(dir.create(here::here(title)))
        outdir <- here::here(title)
      }
    } else {
      outpath <- normalizePath(outpath)
      if (newfolder) {
        suppressWarnings(dir.create(file.path(outpath, title)))
        outdir <- normalizePath(file.path(outpath, title))
      } else {
        outdir <- outpath
      }
    }
    rawdir <- outdir
    
    today <- gsub("-", "", Sys.Date())
    rawfilename <- file.path(rawdir, paste0(title, "_raw_", today, ".csv"))
    reportfilename <- file.path(outdir, paste0(title, "_report_", today, ".csv"))
    articlesfilename <- file.path(outdir, paste0(title, "_publications_", today, ".csv"))
    authorsfilename <- file.path(outdir, paste0(title, "_authors_", today, ".csv"))
    contactsfilename <- file.path(outdir, paste0(title, "_contacts_", today, ".csv"))
    journalsfilename <- file.path(outdir, paste0(title, "_journals_", today, ".csv"))
    
    if (raw) {
      write.csv(my.data$data, file = rawfilename)
      cat(paste0("\n", "\033[0;34m", "Raw results for ", title, " query have been exported to ", 
                 rawfilename, "\033[0m", "\n"))
    }
    if (plist) {
      my.data$data %>%
        dplyr::select(lastname, firstname, title, journal, year) %>%
        dplyr::distinct(title, .keep_all = TRUE) %>%
        write.csv(file = articlesfilename)
      cat(paste0("\n", "\033[0;34m", "Publication titles for ", title, " query has been exported to ", 
                 articlesfilename, "\033[0m", "\n"))
    }
    if (jlist) {
      my.data$data %>%
        dplyr::select(journal) %>%
        dplyr::add_count(journal, name = "n.articles") %>%
        dplyr::distinct(journal, n.articles) %>%
        dplyr::arrange(-n.articles) %>%
        write.csv(file = journalsfilename)
      cat(paste0("\n", "\033[0;34m", "Journals for ", title, " query has been exported to ", 
                 journalsfilename, "\033[0m", "\n"))
    }
    if (alist | clist) {
      #count how many articles per author, and how many journals per author. 
      #then clean list of authors of duplicates
      authors <- my.data$data %>%
        dplyr::group_by(lastname, firstname) %>%
        dplyr::add_count(name = "n.articles") %>%
        dplyr::ungroup() %>%
        dplyr::distinct(lastname, firstname, email, journal, .keep_all = TRUE) %>%
        dplyr::group_by(lastname, firstname) %>%
        dplyr::add_count(name = "n.journals") %>%
        dplyr::ungroup() %>%
        dplyr::select(lastname, firstname, affiliation = address, email, year, n.articles, n.journals) %>%
        dplyr::distinct(lastname, firstname, email, .keep_all = TRUE) 
      if (alist) {
        authors %>% select(-email, -year) %>%
          dplyr::distinct(lastname, firstname, .keep_all = TRUE) %>%
          dplyr::arrange(-n.articles) %>%
          write.csv(file = authorsfilename) 
        cat(paste0("\n", "\033[0;34m", "Authors for ", title, " query have been exported to ", 
                   authorsfilename, "\033[0m", "\n"))
      }
      if (clist) {
        # drop obs that have NAs for emails for contact list
        authors %>% tidyr::drop_na(email) %>%
          dplyr::distinct(lastname, firstname, email, .keep_all = TRUE) %>%
          write.csv(file = contactsfilename) 
        cat(paste0("\n", "\033[0;34m", "Contacts for ", title, " query have been exported to ", 
                   contactsfilename, "\033[0m", "\n"))
      }
    }
    
    cat("\nStats Report - status, search time, and counts of unique observations\n", "\n")
    print(stats_report)
    if (report) {
      write.csv(stats_report, file = reportfilename)
      cat(paste0("\n", "\033[0;34m", "Report for ", title, " query has been exported to ", 
                 reportfilename, "\033[0m", "\n"))
      
    } 
    ## END OF SINGLE QUERY (e.g. journals = NULL)
    
    ## START DEFINED JOURNAL LIST QUERIES
  } else { 
    articles_combine <- list()
    authors_combine <- list()
    stats_combine <- list()
    reps <- length(journals)
    today <- gsub("-", "", Sys.Date())
    
    if (is.null(outpath)) {
      outpath <- here::here()
      if (newfolder) {
        suppressWarnings(dir.create(here::here(title)))
        outdir <- here::here(title)
      }
    } else {
      outpath <- normalizePath(outpath)
      if (newfolder) {
        suppressWarnings(dir.create(file.path(outpath, title)))
        outdir <- normalizePath(file.path(outpath, title))
      } else {
        outdir <- outpath
      }
    }
    if (raw & newfolder) {
      suppressWarnings(dir.create(file.path(outdir, paste0(title, "_raw"))))
      rawdir <- normalizePath(file.path(outdir, paste0(title, "_raw")))
    } else {
      rawdir <- outdir
    }
    
    for (i in 1:reps) {
      my.query <- paste('"', journals[i], '"[Journal] ', sep = '')
      if (is.null(broad)) {
        my.query <- paste(my.query, 
                          'AND ', 
                          narrow,
                          ' AND (',
                          start, ':', end,'[dp])"',
                          sep = '')
      } else {
        my.query <- paste(my.query, 'AND (',
                          narrow, ' ',
                          operator, ' ',
                          broad, ')',
                          ' AND (',
                          start, ':', end,'[dp])"',
                          sep = '')
      }
      cat("\n", "Querying PubMed for", my.query)
      
      skip_to_next <- FALSE
      t_init <- Sys.time()
      # extract data from PubMed and save search results to CSV
      my.data <- tryCatch({
        businessPubMed::extract_pubMed_data(pubMed_query = my.query,
                                            batch_size = 1000,
                                            affi_regex_exclude = NULL)
      },
      error = function(e) {
        message(paste(journals[i], "query produces 0 results. No output for this journal."))
        skip_to_next <<- TRUE
        stats_combine[[i]] <<- data.frame("journal" = journals[i], 
                                          status = "No results")
      })
      
      if (skip_to_next) { next }
      
      t_final <- Sys.time()
      t_diff <- t_final - t_init
      
      if (raw) {
        rawfilename <- file.path(rawdir, paste0(title, "_raw_", today, "_", journals[i], ".csv"))
        write.csv(my.data$data, file = rawfilename)
        cat(paste0("\n", "\033[0;34m", "Raw results for ", title, " query in ", 
                   journals[i], " have been exported to ", rawdir, "\033[0m", "\n"))
      }
      
      # Number of unique publication titles?
      n.articles <- sum(!is.na(unique(my.data$data$title)))
      
      # Number of unique authors?
      n.authors <- sum(!is.na(unique(paste(my.data$data$firstname, my.data$data$lastname, sep = " "))))
      
      # How many unique affiliations?
      n.affiliations <- sum(!is.na(unique(my.data$data$address)))
      
      # How many unique emails?
      n.emails <- sum(!is.na(unique(my.data$data$email)))
      
      # How many unique contacts?
      n.contacts <- sum(!is.na(unique(ifelse(is.na(my.data$data$email), NA, paste(my.data$data$firstname, 
                                                                                  my.data$data$lastname, 
                                                                                  my.data$data$email, 
                                                                                  sep = " ")))))
      # create stats dataframe for journal[i] for combining later
      stats_combine[[i]] <- data.frame("journal" = journals[i],
                                       "status" = "Success",
                                       "search.time" = t_diff,
                                       "n.articles" = n.articles, 
                                       "n.authors" = n.authors, 
                                       "n.affiliations" = n.affiliations, 
                                       "n.emails" = n.emails, 
                                       "n.contacts" = n.contacts)
      
      if (plist) {
        # get list of publications (i.e. articles) with first author name, journal, and year and save to combined articles list
        articles_combine[[i]] <- my.data$data %>%
          distinct(title, .keep_all = TRUE) %>%
          select(lastname, firstname, title, journal, year)
      }
      
      if (alist | clist) {
        # save to combined author list
        authors_combine[[i]] <- my.data$data %>%
          dplyr::rename(affiliation = address) 
      }
    }  # end of journal loop
    
    
    # combine all stats reports and create some helper variables
    stats_report <- as.data.frame(dplyr::bind_rows(stats_combine))
    N_success <- sum(stats_report$status == 'Success')
    N_contacts <- sum(!is.na(stats_report$n.contacts) & stats_report$n.contacts > 0)
    
    # if no successes and newfolder is created, then delete created folder only if empty and created during run
    if (newfolder & N_success == 0) {
      ctime <- file.info(outdir)$ctime # find "new" folder creation time
      difftime <- as.POSIXct(ctime) - as.POSIXct(stime) # difference between new folder creating time and start time of this run
      rawctime <- file.info(rawdir)$ctime # find "new" folder creation time
      rawdifftime <- as.POSIXct(rawctime) - as.POSIXct(stime) # difference between new folder creating time and start time of this run
      # can delete new folder if difftime is greater than 0 (i.e. folder created after start time of this run)
      if (raw & length(list.files(rawdir)) == 0 & rawdifftime > 0) {
        unlink(rawdir, recursive = TRUE)
        message(paste0("\n", "newfolder = TRUE: Empty folder '", title, "_raw' has been deleted from", outdir))
        if (!report & !jlist & length(list.files(outdir)) == 0 & difftime > 0) {
          unlink(outdir, recursive = TRUE)
          message(paste0("\n", "newfolder = TRUE: Empty folder '", title, "' has been deleted from", outpath))
        }
      }
    }
    
    # combine all articles by journal together; write non-duplicated results to csv file
    if (plist) {
      if (N_success > 0) {
        articlesfilename <- file.path(outdir, paste0(title, "_publications_", today, ".csv"))
        dplyr::bind_rows(articles_combine) %>%
          write.csv(file = articlesfilename) 
        cat(paste0("\n", "\033[0;34m", "Publication titles for ", title, " query in ", N_success, " of ", 
                   reps," selected journals have been exported to ", outdir, "\033[0m", "\n"))
      } else {
        cat(paste0("\n", "\033[0;31m", "No results found for ", title, " query within the ", 
                   reps," selected journals.", "\033[0m", "\n"))
      }
    }
    
    if (alist | clist) {
      if (N_success > 0) {
        # combine all journal search results together and check for author last, first and email combination; 
        # export non-duplicated results to csv file
        authors <- dplyr::bind_rows(authors_combine) %>%
          dplyr::group_by(lastname, firstname) %>%
          dplyr::add_count(name = "n.articles") %>%
          dplyr::ungroup() %>%
          dplyr::select(lastname, firstname, affiliation, email, year, n.articles) %>%
          dplyr::distinct(lastname, firstname, email, .keep_all = TRUE) 
        if (alist) {
          authorsfilename <- file.path(outdir, paste0(title, "_authors_", today, ".csv"))
          authors %>% select(-email, -year) %>%
            dplyr::distinct(lastname, firstname, .keep_all = TRUE) %>%
            dplyr::arrange(-n.articles) %>%
            write.csv(file = authorsfilename) 
          cat(paste0("\n", "\033[0;34m", "Authors for ", title, " query in ", N_success, " of ", 
                     reps," selected journals have been exported to ", outdir, "\033[0m", "\n"))
        } 
        if (clist) {
          if (N_contacts > 0) {
            contactsfilename <- file.path(outdir, paste0(title, "_contacts_", today, ".csv"))
            authors %>% tidyr::drop_na(email) %>% write.csv(file = contactsfilename) 
          } else {
            cat(paste0("\n", "\033[0;31m", "No email contacts found for ", title, " query within the ", 
                       reps," selected journals.", "\033[0m", "\n"))
          }
        } else {
          cat(paste0("\n", "\033[0;31m", "No results found for ", title, " query within the ", 
                     reps," selected journals.", "\033[0m", "\n"))
        }
      }
    }
    
    if (jlist) {
      if (!report) {
        reportfilename <- file.path(outdir, paste0(title, "_report_", today, ".csv"))
        write.csv(stats_report, file = reportfilename)
        cat(paste0("\n", "\033[0;31m", "See report for journal stats. No additional file to export.", "\033[0m", "\n"))
      }
    }
    
    cat("\nStats Report by Journal - status, search time, and counts of unique observations\n", "\n")
    print(stats_report)
    if (report) {
      reportfilename <- file.path(outdir, paste0(title, "_report_", today, ".csv"))
      write.csv(stats_report, file = reportfilename)
      cat(paste0("\n", "\033[0;34m", "Report for ", title, " query has been exported to ", 
                 outdir, "\033[0m", "\n"))
    }
  }
}