library(RSelenium)
library(wdman)
library(stringr)

setwd("")

if (file.exists('min_int.RData')) {
  load('min_int.RData')
} else {
  minint_2018_results <- list()
}

if (!exists('errors')) {
  errors <- list()
}


url_senato <- 'http://elezioni.interno.gov.it/senato/scrutini/20180304/scrutiniSI'
url_camera <- 'http://elezioni.interno.gov.it/camera/scrutini/20180304/scrutiniCI'

url_test <- 'http://elezioni.interno.gov.it/camera/scrutini/20180304/scrutiniCI25110000000'

rD <- rsDriver()
remDr <- rD[["client"]]

require(XML)

getDropdown <- function(name) {
  webElem <- remDr$findElement("xpath", sprintf("//*/select[@name='%s']", name))
  options <- webElem$getElementAttribute("outerHTML")[[1]]
  options <- htmlParse(options)
  options_values <- unlist(options["//option", fun = function(x) xmlGetAttr(x, "value")])
  options_labels <- unlist(options["//option", fun = function(x) xmlValue(x)])
  return(setNames(options_values[2:length(options_values)], nm = options_labels[2:length(options_values)]))
}

doClick <- function(name, option, options) {
  # print(which(option == options)+1)
  optionElem  <- 
    remDr$findElement(using = "xpath", 
                      sprintf("//*/select[@name = '%s']/option[%s]",
                              name, which(option == options)+1))
  Sys.sleep(2)
  optionElem$clickElement()
}

doDirectRequest <- function() {
  scrutini<- 
    unlist(lapply(minint_2018_results, FUN = function(x) 
      as.numeric(gsub("[^0-9]", "", x$sezioni[1])) / as.numeric(gsub("[^0-9]", "", x$sezioni[2]))))
  options <- 
    unlist(lapply(minint_2018_results,  FUN = function(x) x$dropdown_option))
  names(options) <- gsub("^(.*)\\.", "", names(options))
  
  ente2 <- 
    unlist(lapply(minint_2018_results,  FUN = function(x) x$ente2))
  
  # scrutini <- scrutini[!grepl("Comune di", ente2)]
  # options <- options[!grepl("Comune di", ente2)]
  
  for (i in 1:length(scrutini)) {
    if (scrutini[i] < 1) {
      
      this_url <- paste0('http://elezioni.interno.gov.it/%s/scrutini/20180304/', names(scrutini[i]))
      if (grepl('scrutiniSI', names(scrutini[i]))) {
        this_url <- sprintf(this_url, 'senato')
      } else {
        this_url <- sprintf(this_url, 'camera')
      }
      
      remDr$navigate(this_url)
      Sys.sleep(2)
      html_source <- remDr$getPageSource()
      print(names(scrutini[i]))
      minint_2018_results[[names(scrutini[i])]] <<- getData(options[i], html_source)
      save(minint_2018_results, errors, file = 'min_int.RData')
    }
  }
}

doLoop <- function() {
  
  # Circoscrizione
  options1 <- getDropdown('sel_circoscrizioni')
  for (i in 1:length(options1)) {
    # for (i in 26:28) {
    
    doClick('sel_circoscrizioni', options1[i], options1)
    Sys.sleep(2)
    
    # Collegio plurinominale
    options2 <- getDropdown('sel_plurinominali')
    for (j in 1:length(options2)) {
      
      if (!is.na(options2[j])) {
        doClick('sel_plurinominali', options2[j], options2)
        Sys.sleep(2) 
      }
      
      # Collegio uninominale
      options3 <- getDropdown('sel_uninominali')
      # print(options3)
      for (k in 1:length(options3)) {
        
        print(paste0(names(options2[j]), " : ", names(options3[k])))
        res <- try({
          doClick('sel_uninominali', options3[k], options3)
        }) 
        if (class(res) == 'try-error') {
          errors[[length(errors)+1]] <- c(options3[k], NA)
        }
        
        Sys.sleep(2)
        # html_source <- remDr$getPageSource()
        # min_int_url_code <-
        #   gsub("http(s)?://elezioni\\.interno\\.gov\\.it/(camera|senato)/scrutini/20180304/",
        #        "",
        #        remDr$getCurrentUrl()[[1]])
        # print(min_int_url_code)
        # minint_2018_results[[min_int_url_code]] <<- getData(options3[k], html_source)
        # 
        # Comune
        options4 <- getDropdown('sel_comuni')
        # print(options4)
        for (l in 1:length(options4)) {
          
          if(any(names(options4[l]) %in%  
                    lapply(minint_2018_results, FUN = function(x) names(x$dropdown_option)))) {
            next
          }

          print(paste0(names(options3[k]), " : ", names(options4[l])))
          
          try({
            doClick('sel_comuni', options4[l], options4)
            Sys.sleep(2)
            html_source <- remDr$getPageSource()
            min_int_url_code <-
              gsub("http(s)?://elezioni\\.interno\\.gov\\.it/(camera|senato)/scrutini/20180304/",
                   "",
                   remDr$getCurrentUrl()[[1]])
            print(min_int_url_code)
            minint_2018_results[[min_int_url_code]] <<- getData(options4[l], html_source)
          }) 
          if (class(res) == 'try-error') {
            errors[[length(errors)+1]] <- c(options3[k], options4[l])
          }
          save(minint_2018_results, errors, file = 'min_int.RData')
        }
        
        save(minint_2018_results, errors, file = 'min_int.RData')
        
      }
      if (is.na(options2[j])) break
    }
  }
}

getData <- function(option, this_page_source) {
  doc <- htmlParse(this_page_source[[1]])
  raw_df <- as.data.frame(readHTMLTable(doc))
  # print(nrow(raw_df))
  raw_df <- raw_df[-seq(from=which(grepl("TOTALE",raw_df$NULL.V1)),to=nrow(raw_df)),]
  
  which_candidate <- c(1,which(raw_df$NULL.V2=='')+1)
  which_candidate <- which_candidate[1:(length(which_candidate)-1)]
  which_party <- 
    which(!(1:nrow(raw_df) %in% which(1:nrow(raw_df) %in% which_candidate | raw_df$NULL.V2=="")))
  
  if (ncol(raw_df) == 4) {
    raw_uninom_candidato <- raw_df[which_candidate,2:4]
    raw_uninom_partito <- raw_df[which_party,2:4]
  } else {
    raw_uninom_candidato <- raw_df[which_candidate,c(2,4,5)]
    raw_uninom_partito <- raw_df[which_party,c(2,4,5)]
  }
  names(raw_uninom_candidato) <- c('candidato','votes','perc')
  names(raw_uninom_partito) <- c('partito','votes','perc')
  
  raw_uninom_candidato_partito <- data.frame()
  
  for (i in which_candidate) {
    j <- i
    while(TRUE) {
      j <- j + 1
      if (raw_df$NULL.V2[j]=='') break
      raw_uninom_candidato_partito <- 
        rbind(raw_uninom_candidato_partito, 
              data.frame(candidato = raw_df$NULL.V2[i],
                         partito = raw_df$NULL.V2[j]))
    }
  }
  
  raw_elettori <- remDr$findElement(using = 'xpath', '//*/div[@class="info_aggiornamento"]')
  raw_elettori <- gsub("[^0-9]", "", 
                       gsub("\\|(.*)$", "", raw_elettori$getElementText()[[1]]))
  
  raw_sezioni <- remDr$findElement(using = 'xpath', '//*/div[@class="sezioni_perv"]')
  raw_sezioni <- str_extract_all(raw_sezioni$getElementText()[[1]], "[0-9]+")[[1]]
  
  raw_ente1 <- 
    remDr$findElements(using = 'xpath', '//*/div[@id="headEnti"]/h3')[[1]]$getElementText()[[1]]
  raw_ente2 <-
    remDr$findElements(using = 'xpath', '//*/div[@id="headEnti"]/h3')[[2]]$getElementText()[[1]]
  
  return(list('candidato' = raw_uninom_candidato,
              'partito' = raw_uninom_partito,
              'candidato_partito' = raw_uninom_candidato_partito,
              'elettori' = raw_elettori,
              'sezioni' = raw_sezioni,
              'ente1' = raw_ente1,
              'ente2' = raw_ente2,
              'dropdown_option' = option))
}

# MAIN

remDr$navigate(url_senato)
doLoop()
remDr$navigate(url_camera)
doLoop()

while(TRUE) {
  doDirectRequest()
}


