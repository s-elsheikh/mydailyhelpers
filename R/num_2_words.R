#' Changing numbers to Written letters
#' In formal writing numbers at the beginning of sentences showed be written in letters.
#' This functions does this. I claim no credit. Credits are in comments in the source code
#'
#' @param x A number
#'
#' @return String with the number x written out
#' @export
#'
#' @examples
#' num_2_words(5)
num_2_words <- function(x){
    ## Function by John Fox found here:
    ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
    ## Tweaks by AJH to add commas and "and"

    #Fork of https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r

    # downloaded from this link:

    #link <- "https://gist.githubusercontent.com/hack-r/22104543e2151519c41a8f0ce042b31c/raw/01731f3176b4ee471785639533d38eda4790ab77/numbers2words.r"




    if(x==0){
        print( "zero")
    } else{
        helper <- function(x){

            digits <- rev(strsplit(as.character(x), "")[[1]])
            nDigits <- length(digits)
            if (nDigits == 1) as.vector(ones[digits])
            else if (nDigits == 2)
                if (x <= 19) as.vector(teens[digits[1]])
            else trim(paste(tens[digits[2]],
                            Recall(as.numeric(digits[1]))))
            else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and",
                                              Recall(makeNumber(digits[2:1]))))
            else {
                nSuffix <- ((nDigits + 2) %/% 3) - 1
                if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
                trim(paste(Recall(makeNumber(digits[
                    nDigits:(3*nSuffix + 1)])),
                    suffixes[nSuffix],"," ,
                    Recall(makeNumber(digits[(3*nSuffix):1]))))
            }
        }
        trim <- function(text){
            #Tidy leading/trailing whitespace, space before comma
            text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
            #Clear any trailing " and"
            text=gsub(" and$","",text)
            #Clear any trailing comma
            gsub("\ *,$","",text)
        }
        makeNumber <- function(...) as.numeric(paste(..., collapse=""))
        #Disable scientific notation
        opts <- options(scipen=100)
        on.exit(options(opts))
        ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
                  "eight", "nine")
        names(ones) <- 0:9
        teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
                   "sixteen", " seventeen", "eighteen", "nineteen")
        names(teens) <- 0:9
        tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
                  "ninety")
        names(tens) <- 2:9
        x <- round(x)
        suffixes <- c("thousand", "million", "billion", "trillion")
        if (length(x) > 1) return(trim(sapply(x, helper)))
        helper(x)
    }

}
