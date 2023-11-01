
##For this workshop, we will need the following packages that you first need to install by running these commands:
install.packages("dplyr") #for data management if needed
install.packages("radiant") #for descriptive statistics
install.packages("quanteda") #for textual data pre-treatment and basic analysis
install.packages("quanteda.textstats")
install.packages("quanteda.textstats")
install.packages("rainette") #for exploratory textual analysis

##Then we can just load the packages
library(dplyr)
library(radiant)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(rainette)




# Inspect the database and conduct descriptive statistics -----------------


radiant::radiant_window()


# Textual analysis of family description ----------------------------------
setwd("E:/Enseignements/Pondicherry Winter school/Textual analysis/Workshop")
load("sample.rdata")


##Transform to corpus
corpus <- corpus(sample$Desc_family)

#Remove punctuation & symbols
tok <- tokens(corpus, remove_punct = TRUE,
              remove_symbols=TRUE,
              remove_numbers =TRUE)

#Remove stop words & words relating to specific family members
tok <- tokens_remove(tok,c(stopwords("en"),"mother","father","mom","dad","sister","sisters","bro","sis","brother","brothers","married","unmarried","parent","parents"))
#Keep words with at least three letters
tok<-tokens_select(tok,min_nchar=3)
#Create document-feature matrix (keep in lower case)
dtm <- dfm(tok, tolower = TRUE)

#Keep relatively frequent terms
dtm <- dfm_trim(dtm, min_docfreq = 10)

#First a word cloud
textplot_wordcloud(dtm, random_order = F, rotation = 0.25,min_size =1,max_words = 100,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))



res <- rainette(dtm, k = 6)

rainette_explor(res, dtm=dtm, corpus_src=corpus)


###Decide to keep the partition in 4 classes
sample$cluster <- cutree(res, k = 4)
save(sample,file="sample2.rdata")
#Explore how these clusters correspond to specific profiles (male/female, ...)
radiant::radiant_window()


# Family description: Slightly different pre-treatment using compounds ---------------------------------------------------------------

tok <- tokens(corpus)

col <- tok |> 
  tokens_remove(c(stopwords("en"),"mother","father","sister","sisters","mom","dad","bro","sis","brother","brothers","married","unmarried","parent","parents")) |> 
  tokens_select(pattern = "^[A-Z]", valuetype = "regex", 
                case_insensitive = T, padding = TRUE) |> 
  quanteda.textstats::textstat_collocations(min_count = 5, tolower = T)
head(col)

comp_toks2 <- tokens_compound(tok, pattern = col)
comp_toks2<-tokens(comp_toks2,remove_punct = T,remove_symbols=TRUE,
                   remove_numbers =TRUE)
comp_toks2 <- tokens_remove(comp_toks2,c(stopwords("en"),"mother","father","sister","sisters","mom","dad","bro","sis","brother","brothers","married","unmarried","parent","parents"))
comp_toks2<-tokens_select(comp_toks2,min_nchar=3)

dtm <- dfm(comp_toks2, tolower = TRUE)
dtm <- dfm_trim(dtm, min_docfreq = 10)

textplot_wordcloud(dtm, random_order = FALSE, rotation = 0.25,min_size =1,max_words = 100,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))


res <- rainette(dtm, k = 6)

rainette_explor(res, dtm=dtm, corpus_src=corpus)


# Textual analysis of the desired partner ---------------------------------------------------------

corpus <- corpus(sample$Desc_desiredpartner)

#Remove punctuation & symbols
tok <- tokens(corpus, remove_punct = TRUE,
              remove_symbols=TRUE,
              remove_numbers =TRUE)

#Remove stop words & words relating to specific family members
tok <- tokens_remove(tok,c(stopwords("en")))
#Keep words with at least three letters
tok<-tokens_select(tok,min_nchar=3)
#Create document-feature matrix (keep in lower case)
dtm <- dfm(tok, tolower = TRUE)

#Keep relatively frequent terms
dtm <- dfm_trim(dtm, min_docfreq = 10)

#First a word cloud
textplot_wordcloud(dtm, random_order = FALSE, rotation = 0.25,min_size =1,max_words = 100,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))



res <- rainette(dtm, k = 8)

rainette_explor(res, dtm=dtm, corpus_src=corpus)

# Textual analysis of the desired partner differentiating by gender---------------------------------------------------------
#First, add the sex information to the corpus
docvars(corpus, "Sex") <- sample$Sex
tok2 <- corpus %>%
  corpus_subset(Sex %in% c("Male", "Female")) %>%
  tokens(remove_punct = TRUE,
         remove_symbols=TRUE,
         remove_numbers =TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_select(min_nchar=3)
dfmat2 <- dfm(tok2,tolower=T) %>%
  dfm_group(Sex) %>%
dfm_trim(min_termfreq = 3)


textplot_wordcloud(dfmat2, comparison = TRUE, min_size =1,max_words = 40,
                   color = c("darkgreen", "darkorange"))
#The most typical tokens of female profiles regarding desired partner
head(tstat1 <- textstat_keyness(dfmat2), 20)
#The most typical tokens of male profiles regarding desired partner
tail(tstat1 <- textstat_keyness(dfmat2), 20)

##Exploratory analysis of men
dfmat2male<-tokens_subset(tok2, Sex=="Male") %>% dfm(tolower=T) %>%  dfm_trim(min_termfreq = 3)
corpusmale<-corpus_subset(corpus,Sex=="Male")
resmale <- rainette(dfmat2male, k = 6)

rainette_explor(resmale, dtm=dfmat2male, corpus_src=corpusmale)

##Exploratory analysis of women
dfmat2female<-tokens_subset(tok2, Sex=="Female") %>% dfm(tolower=T) %>%  dfm_trim(min_termfreq = 3)
corpusfemale<-corpus_subset(corpus,Sex=="Female")
resfemale <- rainette(dfmat2female, k = 6)

rainette_explor(resfemale, dtm=dfmat2female, corpus_src=corpusfemale)
