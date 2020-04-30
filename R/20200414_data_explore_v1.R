
rm(list = ls(all = T))
gc()


#-----------------------------------

library(rjson)
library(data.table)
library(stringr)
library(convPkg5)
library(ade4)
library(cluster)

#-----------------------------------
# Load data

m1 <- 'C:\\Users\\sargo\\Desktop\\kaggle\\covid19\\data\\CORD-19-research-challenge\\metadata.csv'

df1 <- data.frame(fread(m1))

# head(df1)

# Find distinct authors

# l1 <- str_split(df1$authors, pattern = ';')
# df1 <- as.data.frame(do.call('cbind', l1))
# 
# df1 <- data.frame(data.table::rbindlist(l1))
# df1 <- rbindlist(lapply(l1[[1:100]], as.data.frame.list))

# str(l1)


# l2 <- l1[1:5]
# df1 <- do.call(rbind, l2)


# vec<-unlist(l1)
# rankTab(rankTab(vec)$Freq)
# head(df1)



# df1$c_flag <- grepl('corona',df1$abstract, ignore.case = T)
# df1$a_num_flag <- grepl("[A-Za-z]+-[0-9]",df1$abstract, ignore.case = T)
# 
# d1 <- sapply(df1$abstract[1:10], function(x) str_extract("[A-Za-z]+-[0-9]",'',x))
# d2 <- unlist(d1)


# Find words with 1 or 2 hyphens
# s1 <- str_extract_all(df1$abstract, "[A-Za-z]+-[0-9]|[A-Za-z][0-9]+-[A-Za-z]|[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]")

# test_str <- c('state-of-the-art','human-to-human', 's-3')

s1 <- str_extract_all(df1$abstract, '([:alnum:]+-[:alnum:]+-[:alnum:]+-[:alnum:]+|[:alnum:]+-[:alnum:]+-[:alnum:]+|[:alnum:]+-[:alnum:]+)')
s2 <- unlist(s1)
s3 <- as.character(unique(tolower(s2)));length(s3)
s4 <- s3[str_detect(s3, '\\d')];length(s4)

# c1 <- c('123-abg','fds-324','23-3g-6s')

# s3[str_count(s3, '-') == 2]

# grepl('[A-Za-z]+-[0-9]|[0-9]+-[A-Za-z]', c1)

# dt1 <- data.table(df1)
# dt1$flag_col <- str_detect(dt1$abstract, s3[1])

# Create data.frame to collect results
dat1 <- data.frame(term = as.character(s4), paper_cnt = NA, total_cnt = NA, stringsAsFactors = F)
# Put data into data.table structure
df2 <- data.table(df1)

# Loop thru terms
# if term occurs in more than one paper count # of occurences otherwise leave value as NA.
# for(i in 1:20){ # i = 1
for(i in 1:length(dat1$term)){ # i = 1
  term1 <- dat1$term[i]
  df2$flag_col <- stringr::str_detect( df2$abstract, regex(term1, ignore_case = T ))
  paper_sum <- sum(df2$flag_col)
  dat1[i,'paper_cnt'] <- paper_sum
  if(paper_sum > 1){
    df2$cnt_col <- stringr::str_count(df2$abstract, regex(term1, ignore_case = T ))
    dat1[i,'total_cnt'] <- sum(df2$cnt_col)
  }
  if(i %% 1000 == 0){print(i);flush.console()}
}


# df1[grepl('cell-to-c ',df1$abstract, ignore.case = T),'abstract'][1]

dat2 <- dat1[dat1$paper_cnt > 1,]
dat2 <- dat2[order(-dat2$paper_cnt),]
dat2$virus_flag <- str_detect(dat2$term, 'virus|mers|sars')
dat2$zene_flag <- str_detect(dat2$term, 'zene')
dat2$ase_flag <- str_detect(dat2$term, 'ase')
dat2$flu_flag <- str_detect(dat2$term, 'flu')
dat2$fluo_flag <- str_detect(dat2$term, 'fluo')

#-------------------------------------------------------

# create list of terms to compare with letter combinations
dat3 <- data.table(dat2[,c('term','paper_cnt','total_cnt')])

#-------------------------------------------------------


charVec <- c(letters, 1:9, '-')

# create df with 2 letter combos to compare
g1 <- expand.grid(col1 = charVec, col2 = charVec)
g1$pat <- paste(g1$col1, g1$col2, sep = '')
g1$occur_cnt <- NA

# loop thru terms and find how many times each letter pattern occurs in the terms
for(i in 1:length(g1$pat)){ # i = 1
  term1 <- g1$pat[i]
  # term1 <- 'leu'
  # dat3[(stringr::str_detect( dat3$term, regex(term1, ignore_case = T ))),]
  g1[i,'occur_cnt'] <-sum(stringr::str_detect( dat3$term, regex(term1, ignore_case = T )))
  # dat3$flag_col <- stringr::str_detect( dat3$term, regex(term1, ignore_case = T ))
  if(i %% 10000 == 0){print(i);flush.console()}
}

# look at top occuring patterns
g2 <- g1[order(-g1$occur_cnt),]

#-------------------------------------------------------

# create df with 3 letter combos to compare
g1 <- expand.grid(col1 = charVec, col2 = charVec, col3 = charVec)
g1$pat <- paste(g1$col1, g1$col2, g1$col3, sep = '')
g1$occur_cnt <- NA

# loop thru terms and find how many times each letter pattern occurs in the terms
for(i in 1:length(g1$pat)){ # i = 1
  term1 <- g1$pat[i]
  # term1 <- 'leu'
  # dat3[(stringr::str_detect( dat3$term, regex(term1, ignore_case = T ))),]
  g1[i,'occur_cnt'] <-sum(stringr::str_detect( dat3$term, regex(term1, ignore_case = T )))
  # dat3$flag_col <- stringr::str_detect( dat3$term, regex(term1, ignore_case = T ))
  if(i %% 10000 == 0){print(i);flush.console()}
}

# look at top occuring patterns
g3 <- g1[order(-g1$occur_cnt),]

#-------------------------------------------------------

# create df with 4 letter combos to compare
g1 <- expand.grid(col1 = charVec, col2 = charVec, col3 = charVec, col4 = charVec)
g1$pat <- paste(g1$col1, g1$col2, g1$col3, g1$col4, sep = '')
g1$occur_cnt <- NA

# loop thru terms and find how many times each letter pattern occurs in the terms
for(i in 1:length(g1$pat)){ # i = 1
    term1 <- g1$pat[i]
    # term1 <- 'leu'
    # dat3[(stringr::str_detect( dat3$term, regex(term1, ignore_case = T ))),]
    g1[i,'occur_cnt'] <-sum(stringr::str_detect( dat3$term, regex(term1, ignore_case = T )))
    # dat3$flag_col <- stringr::str_detect( dat3$term, regex(term1, ignore_case = T ))
    if(i %% 10000 == 0){print(i);flush.console()}
}

# look at top occuring patterns
g4 <- g1[order(-g1$occur_cnt),]


#------------------------------------------------------
# build feature matrix 

dat4 <- as.data.frame(dat3)
# pat2 <- g2[order(-g2$occur_cnt),'pat'][1:100]
# pat3 <- g3[order(-g3$occur_cnt),'pat'][1:500]
# pat4 <- g4[order(-g4$occur_cnt),'pat'][1:500]
pat2 <- g2[g2$occur_cnt >= 5,'pat'];length(pat2)
pat3 <- g3[g3$occur_cnt >= 5,'pat'];length(pat3)
pat4 <- g4[g4$occur_cnt >= 5,'pat'];length(pat4)
pats_all <- c(pat2, pat3, pat4)

for(i in 1:length(pats_all)){ # i = 1
  pat_a <- pats_all[i]
  col_name <- paste('pt_',pat_a, sep = '')
  dat4$newCol <- as.numeric(stringr::str_detect( dat4$term, regex(pat_a, ignore_case = T )))
  colnames(dat4)[ncol(dat4)] <- col_name
  if(i %% 100 == 0){print(i);flush.console()}
  # print(i);flush.console()
}

colsToUse <- names(dat4)[grepl('^pt_',names(dat4), ignore.case = T)]
# cdv1 <- convPkg5::clusDropVar(x = dat4[,colsToUse], y = 2:20, threshold = .5, xLoopsBeforePrint = 10)

ddm1 <- dudi.mix(dat4[,colsToUse], add.square = FALSE, scannf = F, nf = ncol(dat4[,colsToUse]))

for(j in 201:300){
  set.seed(456)
  pam1 <- clara(ddm1$li[, 1:length(ddm1$eig[ddm1$eig > 1])], k = j, samples = 50)
  sil1 <- pam1$silinfo$avg.width
  print(paste("i: ", i, " | k: ", j, " | sil: ", sil1, " | var: ",  sep = ""))
  flush.console()
}  


dat4$cluster <- pam1$clustering
table(dat4$cluster)

head(dat4[dat4$cluster == 1,1:10])
for(i in 1:length(unique(dat4$cluster))){
  print(i);flush.console()
  print(head(dat4[dat4$cluster == i,1:10]));flush.console()
}


head(dat4[dat4$cluster == 1,order(colSums(-dat4),colSums(-dat4,na.rm=TRUE))])
dat4[,order(colSums(-dat4,na.rm=TRUE))]
dat4[,colsToUse][order(colSums(dat4[,colsToUse]))]

dat[,names(sort(colSums(dat), decreasing = T))]
dat4[dat4$cluster ==1,colsToUse][,names(sort(colSums(dat4[,colsToUse]), decreasing = T))]


# order(colSums(-df),colSums(-df,na.rm=TRUE))

agg1 <- t(aggregate(.~ dat4$cluster , FUN = mean, data = dat4[,c(colsToUse, 'cluster')]))
agg1[1:100,1:10]

# write.csv(agg1, file = "C:\\Users\\sargo\\Desktop\\kaggle\\covid19\\output\\20200422_agg_clusters_v1.csv" , row.names = T)

# for(i in 1:length(dat1$term)){ # i = 1
#   term1 <- dat1$term[i]
#   df2$flag_col <- stringr::str_detect( df2$abstract, regex(term1, ignore_case = T ))
#   paper_sum <- sum(df2$flag_col)
#   dat1[i,'paper_cnt'] <- paper_sum
#   if(paper_sum > 1){
#     df2$cnt_col <- stringr::str_count(df2$abstract, regex(term1, ignore_case = T ))
#     dat1[i,'total_cnt'] <- sum(df2$cnt_col)
#   }
#   if(i %% 1000 == 0){print(i);flush.console()}
# }




dim(g1)

head(g1)





#-------------------------------------------------------


# write.csv(dat4, file = 'C:\\Users\\sargo\\Desktop\\kaggle\\covid19\\output\\20200429_hyphenated_word_clusters_v1.csv')

summary(dat2)

outfile1 <- "C:\\Users\\sargo\\Desktop\\kaggle\\covid19\\output\\20200416_hyphenated_words_v1.csv"
# write.csv(dat2, outfile1, row.names = F)

# dat2 <- as.data.frame(fread(outfile1))

#------------------------------



# dat2 <- dat2[order(-dat2$paper_cnt),]

# dim(dat2[dat2$paper_cnt == 1,])

#  grepl("^[A-Za-z]+-[0-9]+$",'A2B-123')

# table(df1$c_flag, df1$a_num_flag)



# df4 <- data.frame(rows = 1:5, tst = NA)
# df4$tst[1] <- 'asdf'
# df4$tst[2] <- 'asdf-123 bbb-144 asdf-123'
# df4$tst[3] <- 'asdf'
# df4$tst[4] <- 'asdf-34 dfg-345 ljk-453 asdf-123 asdf-123 asdf-123 asdf-123'
# df4$tst[5] <- 'asdf'

# terma <- 'asdf-123'
# df4$flag <- as.numeric(grepl(terma, df4$tst))
# df4$cnt <- str_count(df4$tst, terma)


