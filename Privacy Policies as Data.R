#Privacy Policies as Data
#Assessing word frequency, context and similarities.

#1) Importing Privacy.xlsx (Import Dataset)
#1.1) Environment > Import Dataset > From Excel...

View(Privacy)

#2) Install necessary packages:
library(quanteda)
library(quanteda.textstats)

#3) We need to construct a corpus for the excel file:

corpus_privacy <- corpus(Privacy, text_field = "policy")

#4) The function describes our file. We can see three policies under analysis:

summary(corpus_privacy, 3)

# Output:
Corpus consisting of 3 documents, showing 3 documents:

  Text Types Tokens Sentences  company Year
 text1  1264   5861       230     meta 2022
 text2  1268   5603       190   google 2022
 text3  1022   4453       171 whatsapp 2021

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
  # I am listing 30 words for a cloud visualization. You may use the cloud word from Microsoft Power BI for better visualization.
                     
privacy_tokens_nostop <- privacy_tokens %>%
tokens_tolower() %>%
tokens_remove(stopwords('en'))
privacy_dtm_nostop <- dfm(privacy_tokens_nostop)
privacy_dtm_nostop %>%
textstat_frequency() %>% 
head(30)

# Output:
feature frequency rank docfreq group
1  information       356    1       3   all
2     services       188    2       3   all
3          use       179    3       3   all
4       google       114    4       2   all
5     products        96    5       3   all
6      account        90    6       3   all
7          can        88    7       3   all
8      privacy        83    8       3   all
9      collect        83    8       3   all
10     provide        76   10       3   all
11          us        75   11       3   all
12        like        75   11       3   all
13      policy        65   13       3   all
14     example        63   14       3   all
15   including        62   15       3   all
16        meta        57   16       2   all
17      device        56   17       3   all
18         may        56   17       3   all
19        also        54   19       3   all
20    activity        52   20       3   all
21     content        52   20       3   all
22        data        48   22       3   all
23        help        46   23       3   all
24         ads        46   23       3   all
25       learn        44   25       3   all
26       share        43   26       3   all
27    settings        42   27       3   all
28      delete        39   28       3   all
29        apps        38   29       3   all
30       using        38   29       3   all

                          
#7) I will use the cosine method to verify the degree of similarities between the policies:
cos_privacy_dtm <- textstat_simil(privacy_dtm, method="cosine")

                     
#8) Based on the matrix below, we can identify the most similar documents:
                     
sort(cos_privacy_dtm[3:1],dec=T)

# Output:
[1] 1.0000000 0.8954104 0.8747002


#9) I will use the "KWIC" to identify the context of how a given "word" shows in these policies. In this example, I will use the word "consent".

kwic(privacy_tokens,"consent", window=4) %>% 
  head(15)

# Output:
Keyword-in-context with 6 matches.                                                                                               
 [text2, 2002]                  We'll ask for your | consent | before using your information   
 [text2, 2937]           following cases With your | consent | We'll share personal information
 [text2, 2949]                   when we have your | consent | For example if youuse           
 [text2, 3005]               ask for your explicit | consent | to share any sensitive          
 [text2, 4171] PrivacyPolicy without your explicit | consent | We always indicate the          
 [text3, 3292]                 want to revoke your | consent | to our use of 

#10)  We can use the "KWIC" also to compare in which context a given "word" receives in two different policies.
# I will compare the use of the term "data" in the Privacy Act of 1974, as amended, 5 U.S.C. § 552a, with the privacy policies under analysis.
# First, I will list the results of the privacy policies. For the policies under analysis, we have the following:

kwic(privacy_tokens,"data", window=4) %>% 
  head(15)

# Output:
Keyword-in-context with 15 matches.                                                                                        
 [text1, 1124]               you visit and cookie | data | like through Social Plugins  
 [text1, 2629] and similar technologies including | data | that we store on             
 [text1, 2959]        and advertising vendors and | data | providers who have the       
  [text2, 948]               GPS and other sensor | data | from your device IP          
  [text2, 990]              The types of location | data | that we collect and          
 [text2, 1205]         web storage or application | data | caches databases and server  
 [text2, 1214]           logs WHY GOOGLE COLLECTS | DATA | We use data to               
 [text2, 1217]               COLLECTS DATA We use | data | to build better services     
 [text2, 1577]         Measure performance We use | data | for analytics and measurement
 [text2, 1593]             For example we analyse | data | about your visits to         
 [text2, 1610]              design And we alsouse | data | about the ads that           
 [text2, 1863] algorithmsto recognise patterns in | data | For example Google Translate 
 [text2, 2442]              to review and control | data | that's saved to your         
 [text2, 2622]              of Google Export your | data | To delete your information   
 [text2, 2996]               have given access to | data | in your Google Account.We'll

#11) Now, we can obtain the results under the Privacy Act of 1974, as amended, 5 U.S.C. § 552a, we will need to perform the following:
#11.1) Importing the File "P1.xlsx".
# Environment > Import Dataset > From Excel...
View(P1)

# 11.2) Construct the corpus:
corpus_p1 <- corpus(P1, text_field = "policy")
summary(corpus_p1, 1)

# Output:
Corpus consisting of 1 document, showing 1 document:

  Text Types Tokens Sentences     company Year
 text1  1247   6216        49 privacy_act 1974

# 11.3) Obtain the tokens:
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

#11.4) Process the KWIC for the Privacy Act of 1974:
kwic(p1_tokens,"data", window=4) %>% 
  head(15)

# Output:
Keyword-in-context with 13 matches.                                                                                           
    [text1, 9]        When implementing the Open | Data | Policy agencies shall incorporate
   [text1, 81]        Implementation of the Open | Data | Policy To fa­cilitate effective  
   [text1, 91]        implementation of the Open | Data | Policy I direct the              
  [text1, 107]              issuance of the Open | Data | Policy the CIO and               
  [text1, 131]           in integrating the Open | Data | Policy into their oper­ations    
  [text1, 165]              the adoption of open | data | practices b Within 90            
  [text1, 177]              issuance of the Open | Data | Policy the Administrator for     
  [text1, 232]          in­tegration of the Open | Data | Policy requirements into Federal 
  [text1, 301]        implementation of the Open | Data | Policy The CPO shall             
 [text1, 1371] to produce aggre­gate statistical | data | without any personal identifiers 
 [text1, 1388]  statistical project the specific | data | of which may not                 
 [text1, 3782]         persons to submit written | data | views or arguments to            
 [text1, 5142]   consisting only of identi­fying | data | and notations of arrests 

#12) To Assess the term frequency-inverse document frequency (TF-IDF), we will follow these steps:

privacy_dtm_freq <- dfm_tfidf(privacy_dtm)
privacy_dtm_freq
privacy_dtm_freq %>%
  textstat_frequency(force=TRUE) %>%
  head(10)

# Output:
feature frequency rank docfreq group
1    google 20.074404    1       2   all
2  whatsapp 13.836516    2       1   all
3    search 10.496668    3       1   all
4      meta 10.037202    4       2   all
5  personal  9.542425    5       1   all
6     sites  8.111061    6       1   all
7   youtube  6.202576    7       1   all
8     saved  4.771213    8       1   all
9  facebook  4.754464    9       2   all
10     them  4.578373   10       2   all


#13) To compare the similarities between a pair of privacy policies using cosine, I will compare the privacy policy from Google and Snap Inc (Snapchat). 
#The file "SG.xlsx" lists the two policies.
#13.1) Importing the File "SG.xlsx".
# Environment > Import Dataset > From Excel...
View(SG)

#13.2) Prepare the corpus
library(quanteda)
library(quanteda.textstats)
corpus_sg <- corpus(SG, text_field = "policy")
summary(corpus_sg, 2)

# Output:
Corpus consisting of 2 documents, showing 2 documents:

  Text Types Tokens Sentences company year
 text1  1303   3039       129    snap 2022
 text2  1268   5603       190  google 2022

#13.3) Prepare the tokens
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

#13.4) Remove stopwords and identify those with the highest frequency.
sg_tokens_nostop <- sg_tokens %>%
  tokens_tolower() %>%
  tokens_remove(stopwords('en'))
sg_dtm_nostop <- dfm(sg_tokens_nostop)
sg_dtm_nostop %>%
  textstat_frequency() %>% 
  head(10)

# Output:
feature frequency rank docfreq group
1  information       152    1       2   all
2       google       113    2       1   all
3     services        97    3       2   all
4          use        69    4       2   all
5      account        53    5       2   all
6          may        53    5       2   all
7          can        50    7       2   all
8      privacy        42    8       2   all
9         data        42    8       2   all
10     collect        39   10       2   all

#13.5) Create the function for cosine
cos_sg_dtm <- textstat_simil(sg_dtm, method="cosine")
dim(cos_sg_dtm)
View(cos_sg_dtm)

#13.6)  The result below provides the level of similarity between Snap and Google privacy policies.
sort(cos_sg_dtm[1:2],dec=T)

# Output:
[1] 1.0000000 0.8261098
