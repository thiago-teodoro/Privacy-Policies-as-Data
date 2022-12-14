#TEST 1
#Assessing similarities and most frequently words:

#1) Importing Privacy.xlsx (Import Dataset)

Privacy <- read_excel("Privacy.xlsx")
View(Privacy)

#2) Install necessary packages:
library(quanteda)
library(quanteda.textstats)

#3) We need to construct a corpus for the excel file:

corpus_privacy <- corpus(Privacy, text_field = "policy")

#4) The function describes our file. We can see three policies under analysis:

summary(corpus_privacy, 3)

#5) Data Preparation to remove unnecessary items.

privacy_tokens <- quanteda::tokens(corpus_privacy,
                                     what = c("word"),
                                     remove_punct = TRUE, # default FALSE
                                     remove_symbols = TRUE, # default FALSE
                                     remove_numbers = FALSE,
                                     remove_url = TRUE, # default FALSE
                                     remove_separators = TRUE,
                                     split_hyphens = FALSE,
                                     include_docvars = TRUE,
                                     padding = FALSE,
                                     verbose = quanteda_options("verbose")
)

privacy_dtm <- quanteda::dfm(privacy_tokens, tolower=TRUE)
                     
                     
#6) We can remove stopwords (e.g. "the") and identify those words with the highest frequency:
  # I am listing 30 words for a cloud visualization.
                     
privacy_tokens_nostop <- privacy_tokens %>%
tokens_tolower() %>%
tokens_remove(stopwords('en'))
privacy_dtm_nostop <- dfm(privacy_tokens_nostop)
privacy_dtm_nostop %>%
textstat_frequency() %>% 
head(30)
                     
     
                     
#7) I will use the cosine method to verify the degree of similarities between the policies:
cos_privacy_dtm <- textstat_simil(privacy_dtm, method="cosine")

                     
#8) Based on the matrix below, we can identify the most similar documents:
                     
sort(cos_privacy_dtm[3:1],dec=T)


#9) I will use the "KWIC" to identify the context of how a given "word" shows in these policies. In this example, I will use the word "consent".

kwic(privacy_tokens,"consent", window=4) %>% 
  head(15)

#10)  We can use the "KWIC" also to compare in which context a given "word" receives in two different policies.
# I will compare the use of the term "data" in the Privacy Act of 1974, as amended, 5 U.S.C. § 552a, with the privacy policies under analysis.
# For the policies under analysis, we have the following:

kwic(privacy_tokens,"data", window=4) %>% 
  head(15)

#11) To obtain the results under the Privacy Act of 1974, as amended, 5 U.S.C. § 552a, we will need to perform the following:
library(readxl)
P1 <- read_excel("P1.xlsx")
View(P1)

# 11.1) Construct the corpus:
corpus_p1 <- corpus(P1, text_field = "policy")
summary(corpus_p1, 1)

# 11.2) Obtain the tokens:
p1_tokens <- quanteda::tokens(corpus_p1,
                              what = c("word"),
                              remove_punct = TRUE, # default FALSE
                              remove_symbols = TRUE, # default FALSE
                              remove_numbers = FALSE,
                              remove_url = TRUE, # default FALSE
                              remove_separators = TRUE,
                              split_hyphens = FALSE,
                              include_docvars = TRUE,
                              padding = FALSE,
                              verbose = quanteda_options("verbose")
)

#11.3) Process the KWIC for the Privacy Act of 1974:
kwic(p1_tokens,"data", window=4) %>% 
  head(15)

#12) To Assess the term frequency-inverse document frequency (TF-IDF), we will follow these steps:

privacy_dtm_freq <- dfm_tfidf(privacy_dtm)
privacy_dtm_freq
privacy_dtm_freq %>%
  textstat_frequency(force=TRUE) %>%
  head(10)

privacy_tfidf <- privacy_dtm_freq %>%
  as.numeric()
names(privacy_tfidf) <- colnames(privacy_dtm)
sort(privacy_tfidf,dec=T)[1:10]


#13) To compare the similarities of two privacy policies using cosine (Degree of similarity between Snap (Snapchat) and Google)

library(readxl)
SG <- read_excel("SG.xlsx")
View(SG)

#13.1) Prepare the corpus
library(quanteda)
library(quanteda.textstats)
corpus_sg <- corpus(SG, text_field = "policy")
summary(corpus_sg, 2)

#13.2) Prepare the tokens
sg_tokens <- quanteda::tokens(corpus_sg,
                              what = c("word"),
                              remove_punct = TRUE, # default FALSE
                              remove_symbols = TRUE, # default FALSE
                              remove_numbers = FALSE,
                              remove_url = TRUE, # default FALSE
                              remove_separators = TRUE,
                              split_hyphens = FALSE,
                              include_docvars = TRUE,
                              padding = FALSE,
                              verbose = quanteda_options("verbose")
)

sg_dtm <- quanteda::dfm(sg_tokens, tolower=TRUE) 

#13.3) Remove stopwords
sg_tokens_nostop <- sg_tokens %>%
  tokens_tolower() %>%
  tokens_remove(stopwords('en'))
sg_dtm_nostop <- dfm(sg_tokens_nostop)
sg_dtm_nostop %>%
  textstat_frequency() %>% 
  head(10)

#13.4) Create the function for cosine
cos_sg_dtm <- textstat_simil(sg_dtm, method="cosine")
dim(cos_sg_dtm)
View(cos_sg_dtm)

#13.5)  The result below provides the level of similarity between Snap and Google privacy policies.
sort(cos_sg_dtm[1:2],dec=T)
