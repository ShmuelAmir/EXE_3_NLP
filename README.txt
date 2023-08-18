# Shmuel Amir 316392323
# team: WatsonTeam 
# date: 11-08-2023
# github link: https://github.com/ShmuelAmir/Exe_3_NLP 

data.file <- 'data/okcupid_profiles.csv.gz'
source('Week5_datingNLP.r')

# ———— confusion matrix ————
##           Reference
## Prediction  f  m
##          f 16 12
##          m 24 48                         
## Accuracy (average) : 0.643

# ———— training times ————
## 10-fold training 1
## Time difference of 49.75443 mins
## 10-fold training 2
## Time difference of 41.11271 mins
## 10-fold training 3
## Time difference of 51.45463 mins

# ———— list of female words ————
## [1] "friend" "work"   "music"  "good"   "time"   "thing"  "peopl"  "go"     "life"   "food"   "just"   "movi"   "make"   "get"   
## [15] "new"    "enjoy"  "book"   "want"   "know"   "live"   "look"   "realli" "tri"    "think"  "also"  

# ———— list of male words ————
## [1] "friend" "good"   "music"  "work"   "peopl"  "time"   "life"   "thing"  "food"   "go"     "movi"   "make"   "new"    "just"  
## [15] "get"    "book"   "famili" "enjoy"  "live"   "want"   "realli" "know"   "tri"    "also"   "look"  
