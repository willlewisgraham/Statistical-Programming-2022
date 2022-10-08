# Project 1
# Will Graham; Richelle Lee; Robin Lin
# 2022-10-06
### Group Contributions:
# Instead of splitting tasks between our group, we worked collaboratively on each step of the process.
# Contributions were 1/3 per team member.



### Loads the Bible text and removes the unecessary pages.

# User must set wd accordingly
a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers
### Giving punctuations their own indexes in bible text list.


# Separating words and punctuation marks.
indexes_split_punct <- function(string){
  indexes <- grep("[',','.','?','!',':',';']",a) ## obtain the indexes of words containing specified punctuations characters 
  return(indexes)
}


# Returns the last character in a string.
get_punct <- function(r){
  return(substr(r, nchar(r), nchar(r)))
}


# Splits punctuation marks from words, and inserts them into their correct positions in the final Bible text. Fills in remaining slots with words from the original Bible text.
split_punct <- function(b, test){
  punct_indexes <- indexes_split_punct(b)
  punct <- lapply(b[punct_indexes] , FUN = get_punct)
  new_b <- rep(0, (length(punct_indexes) + length(b)))
  final_punct_indexes <- punct_indexes + 1:length(punct_indexes) ## Pushes punctuation indexes forward by n slots, where n is an accummulation of the number of punctuation marks.
  new_b[final_punct_indexes] <- punct
  no_punct_list <- gsub('[[:punct:] ]+','',b)
  if (test == TRUE){ ## Used for testing. Reduces sample size to 100.
    b <- b[1:100]
    new_b <- new_b[1:115]
  }
  new_b[-final_punct_indexes] <- no_punct_list
  return(new_b)
}
### Lowering capitals and calculating the top 500 most common words.


new_b <- split_punct(a, FALSE)
lower_new_b <- tolower(new_b)
unique_words <- unique(lower_new_b)
index_match <- match(lower_new_b, unique_words) ## Mapping all Bible words to their unique entry in the unique list.
tabulate_index_match <- tabulate(index_match) ## Count the occurrences of the unique words in the Bible text.
sort_results <- sort(tabulate_index_match, index.return = TRUE, decreasing = TRUE)
top_500 <- sort_results$ix[1 : 500] ## Pull the top 500 most common words.
result_list <- unique_words[top_500]
### Saving: every occurrence of 3 consecutive common words, distinct combinations of two (and three) consecutive common words.
### Counts occurrences of distinct combinations of two (and three) consecutive common words.


# Saving every occurrence of 3 consecutive common words.
common_word_match <- match(lower_new_b, result_list)
first_column <- common_word_match[1 : (length(common_word_match) - 2)]
second_column <- first_column[2 : (length(common_word_match) - 1)]
third_column <- first_column[3 : length(common_word_match)]
T_array <- data.frame(first_column, second_column, third_column)
T_array <- T_array[is.na(rowSums(T_array)) == FALSE,] ## Drop rows that contain uncommon words.


# Maps indexes to common words
Index_to_common_words <- function(x){
  return(result_list[x])
}


# Calculating the probability of selecting any common words with no prior word.
matrix_x <- cbind(c(lapply(T_array$first_column, FUN = Index_to_common_words)), c(lapply(T_array$second_column, FUN = Index_to_common_words)), c(lapply(T_array$third_column, FUN = Index_to_common_words))) ## Converts indexes to words
Inital_W_map <- unique(T_array$first_column)
Intial_word_probability <- tabulate(match(T_array$first_column, unique(T_array$first_column)))
Initail_prob <- Intial_word_probability/sum(Intial_word_probability) ## Probability weights for selecting common words with no prior words.


# Counts occurrences of distinct combinations of two consecutive common words.
second_W_map <- unique(T_array[1:2]) ## All observed combinations of two consecutive common words.
second_W_map_conc <- paste(second_W_map[[1]], second_W_map[[2]]) ## Concatenated values for use in future mapping
T_array_conc <- paste(T_array[[1]], T_array[[2]])
df2 <- data.frame(c(second_W_map_conc), rep(0, length(second_W_map_conc[[1]]))) ## Initialises data frame to store distinct combination frequencies.
colnames(df2) <- c("Concat_w1_w2", "Freq")
match_indexes <- match(T_array_conc, second_W_map_conc)
tabulate_match_index <- tabulate(match_indexes) ## Count the occurrences of two consecutive common words in the Bible text via. T_array.
df2['Freq'] <- tabulate_match_index


# Counts occurrences of distinct combinations of three consecutive common words.
third_W_map <- unique(T_array[1:3]) ## All observed combinations of three consecutive common words.
third_W_map_conc <- paste(third_W_map[[1]], third_W_map[[2]], third_W_map[[3]]) ## Concatenated values for use in future mapping
T_array_conc2 <- paste(T_array[[1]], T_array[[2]], T_array[[3]])
df3 <- data.frame(c(third_W_map_conc), rep(0, length(third_W_map_conc[[1]]))) ## Initialises data frame to store distinct combination frequencies.
colnames(df3) <- c("Concat_w1_w2_w3", "Freq")
match_indexes2 <- match(T_array_conc2, third_W_map_conc)
tabulate_match_index2 <- tabulate(match_indexes2) ## Count the occurrences of two consecutive common words in the Bible text via. T_array.
df3['Freq'] <- tabulate_match_index2
### Defining functions to generate the next appropriate word in a Markov text model. 
### These functions handle cases when we have: no valid prior word, one valid prior word, and two valid prior word.


# Generates a random common word with probability weights proportinal to the frequency of each common word in the Bible text.
word_1 <- function(punctuation){
  PI <- grep("[',','.','?','!',':',';']", result_list) ## Fetches indexes of specified punctuation marks from list of 500 most common word.
  if (punctuation == TRUE){
    return(as.numeric((sample(Inital_W_map, 1, prob = Initail_prob))))
  }
  while (TRUE){ ## We do not want the first word to be a punctuation mark; pull another random word if a punctuation mark is selected.
    ret <- sample(Inital_W_map, 1, prob = Initail_prob)
    if (!(ret %in% PI)) {
      return(as.numeric(ret))
    }
  }
}


# Generates a random common word with probability weights proportinal to the frequency of possible word pairs (based on prior word) in the Bible text. 
word_2 <- function(w1){
  new_ops <- second_W_map[second_W_map[1] == w1,] ## Possible unique common word pairs
  freq <- c(1:length(new_ops[[1]]))
  for (i in c(1:length(new_ops[[1]]))){
    freq[i] <- subset(df2, df2$Concat_w1_w2 == paste(new_ops[i,1], new_ops[i,2]), 2)[[1]] ## Occurrences of common word pairs in the Bible text serve as probability weights.
  }
  sample_1 <- as.numeric(sample(as.numeric(as.character(as.factor(new_ops[,2]))), 1, prob = freq)) ## Return random word choice based on specified probabilities.
  return(sample_1)
}


# Generates a random common word with probability weights proportinal to the frequency of possible word triplets (based on prior word pairs) in the Bible text. 
word_3 <- function(w1, w2){
  new_ops <- third_W_map[third_W_map[1] == w1 & third_W_map[2] == w2,] ## Possible unique common words triplets
  freq <- c(1:length(new_ops[[1]]))
  for (i in c(1:length(new_ops[[1]]))){
    freq[i] <- subset(df3, df3$Concat_w1_w2_w3 == paste(new_ops[i,1], new_ops[i,2], new_ops[i,3]),2)[[1]] ## Occurrences of common word triplets in the Bible text serve as probability weights.
  }
  sample_1 <- as.numeric(sample(as.numeric(as.character(as.factor(new_ops[,3]))), 1, prob = freq)) ## Return random word choice based on specified probabilities.
  return(sample_1)
}
### Calculating proportion of capital occurrences for each common word in the Bible text.


# Counting the occurrences of common word capitals in the Bible text.
cap_words <- grep("[A-Z]", new_b, value = T)
common_cap_words <- toupper(cap_words[tolower(cap_words) %in% result_list]) ## If the word starts with a capital letter, capitalise the whole word and store it.
unique_cap_words <- unique(common_cap_words)
cap_index_match <- match(common_cap_words, unique_cap_words)
freq_cap_index_match <- tabulate(cap_index_match)
cap_freq_array <- data.frame(Unique_Cap_Words = unique_cap_words, Frequency = freq_cap_index_match) ## Store the number of times a common word appears with a capital letter in the Bible text.


# Counting the occurences of common words (irrespective of capitals) in the Bible text.
lower_index_match <- match(lower_new_b, result_list) ## Maps indexes to every word in the Bible from the top 500 most common word list (where possible)
freq_lower_index_match <- tabulate(lower_index_match)
lower_freq_array <- data.frame(Unique_Lower_Words = result_list, Frequency = freq_lower_index_match) ## Stores number of occurrences of top 500 most common words in the Bible text.
cp <- rep(0, length(result_list)) ## Initialise a list for storing capital common word proportions. 
for (i in 1:length(result_list)){
  if (!(toupper(result_list[i]) %in% cap_freq_array$Unique_Cap_Words)){
    cp[i] <- 0
  }else{
    ## Divide the number of times a common word appears as a capital in the Bible text by the number of times it appears, irrespective of capitalisation.
    cp[i] <- cap_freq_array[cap_freq_array$Unique_Cap_Words == toupper(result_list[i]),2]/lower_freq_array[lower_freq_array$Unique_Lower_Words == result_list[i],2]
  }
}
dfcp <- data.frame(result_list, cp) ## Store the ratio of the capital occurrences of common words.
### Generates a 50 word "paragraph" using a second order Markov model


words_generator_50 <- function(){
  sentence <- as.numeric(rep (0,50))
  w1 <- word_1(FALSE) ## Initialise the first word assuming no prior words.
  sentence[1] <- w1
  w2 <- word_2(w1) ## Generate the second word based on the first word.
  sentence[2] <- w2
  ## Attempt to generate the next word in the sequence based on preceding word pair; if impossible, attempt to generate the next word in sequence based on preceding word; if impossible, generate the next word assuming no prior words.
  for (i in c(3:length(sentence))){
    if(paste(sentence[i - 2], sentence[i - 1]) %in% df2[1]){ ## Check if preceding word pair starts any common word triplets in the Bible text. 
      sentence[i] <- word_3(sentence[i - 2], sentence[i - 1]) ## If yes, generate next word based on preceding word pair.
    } else if (sentence[i - 1] %in% second_W_map[[1]]){ ## Check if preceding word starts any common word pairs in the Bible text.
      sentence[i] <- word_2(sentence[i - 1]) ## If yes, generate next word based on preceding word.
    } else {
      sentence[i] <- word_1(FALSE) ## Default to random common word generation based on frequency in Bible text.
    }
  }
  
  
  # Convert common word indexes to their respective words.
  sentence2 <- c(1:50)
  for (i in c(1:length(sentence2))){
    if (dfcp[sentence[i],2] <= 0.5){ ## If the common word appears as a capital less than 50% of the time in the Bible text, store it as a lowercase.
      sentence2[i] <- result_list[as.numeric(sentence[i])]}
    else { ## Else, capitalises the first letter of the common word
      sentence2[i] <- paste(toupper(substr(result_list[as.numeric(sentence[i])], 1,1)), substr(result_list[as.numeric(sentence[i])], 2,nchar(result_list[as.numeric(sentence[i])])), sep ="")
    }
  }
  return(sentence2)
}
### Generates a 50 word "paragraph" using a 0th order Markov model


words_generator_50_random <- function(){
  sentence <- as.numeric(rep (0,50))
  for (i in 1:length(sentence)){ ## Instead of trying higher order prediction models, always assume no prior word.
    sentence[i] <- word_1(TRUE)
  }
  
  
  # Same process as previous common word conversion.
  sentence2 <- c(1:50)
  for (i in c(1:length(sentence2))){
    if (dfcp[sentence[i],2] <= 0.5){
      sentence2[i] <- result_list[as.numeric(sentence[i])]}
    else {
      sentence2[i] <- paste(toupper(substr(result_list[as.numeric(sentence[i])], 1,1)), substr(result_list[as.numeric(sentence[i])], 2,nchar(result_list[as.numeric(sentence[i])])), sep ="")
    }
  }
  return(sentence2)
}
### Generate the 50 word "paragraph" with a 2nd order Markov model.
test <- words_generator_50()
cat(test)
## daughter , stand in God ? do for the morning , or whether God : and they sat on the altar , and all the first day David arose , that the son of my chief priests which had no man among you . now , saying , and round
### Generate the 50 word "paragraph" with a 0th order Markov model.
test1 <- words_generator_50_random()
cat("\n", test1)
## 
##  , , . unto manner day fire rejoice the so be are the the and to therefore the you house do offer me and , , there : : gold , know blood and the me to ; under if land ? . of , send I evil let said

