library(rvest)
AR<- read_html("http://www.baseball-reference.com/teams/ARI/") %>% 
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
AR$Current<-rep("Arizona Diamondbacks", 19)
## Atlanta
ATL<- read_html("http://www.baseball-reference.com/teams/ATL/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
ATL$Current<-rep("Atlanta Braves", 141)
## Baltimore
BAL<-read_html("http://www.baseball-reference.com/teams/BAL/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
BAL$Current<-rep("Baltimore Orioles", 116)
## Boston AKA best team ever
BOS<-read_html("http://www.baseball-reference.com/teams/BOS/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
BOS[,23]<-rep("Boston Red Sox", 116)
colnames(BOS)<-c(colnames(BOS)[1:22], "Current")
## Chicago Cubs
CHC<-read_html("http://www.baseball-reference.com/teams/CHC/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
CHC$Current<-rep("Chicago Cubs", 141)
## Chicago White Sox
CWS<-read_html("http://www.baseball-reference.com/teams/CHW/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
CWS$Current<- rep("Chicago White Sox", length(CWS$Tm))
## Cincinatti
CIN<-read_html("http://www.baseball-reference.com/teams/CIN/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
CIN$Current<- rep("Cincinnati Reds", length(CIN$Tm))
## Cleveland aka 2016 WS Champs <3 
CLE<-read_html("http://www.baseball-reference.com/teams/CLE/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
CLE$Current<-rep("Cleveland Indians", length(CLE$Tm))
## Colorado
COL<-read_html("http://www.baseball-reference.com/teams/COL/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
COL$Current<-rep("Colorado Rockies", length(COL$Tm))
## Detroit
DET<-read_html("http://www.baseball-reference.com/teams/DET/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
DET$Current<-rep("Detroit Tigers", length(DET$Tm))
## Houston
HOU<-read_html("http://www.baseball-reference.com/teams/HOU/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
HOU$Current<-rep("Houston Astros", length(HOU$Tm))
## Kansas City 
KSR<-read_html("http://www.baseball-reference.com/teams/KCR/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
KSR$Current<-rep("Kansas City Royals", length(KSR$Tm))
## LA - Angels
ANA<-read_html("http://www.baseball-reference.com/teams/ANA/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
ANA$Current<-rep("Los Angeles Angels", length(ANA$Tm))
## LA - Dodgers
LAD<-read_html("http://www.baseball-reference.com/teams/LAD/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
LAD$Current<-rep("Los Angeles Dodgers", length(LAD$Tm))
## Miami
FLA<-read_html("http://www.baseball-reference.com/teams/FLA/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
FLA$Current<-rep("Miami Marlins", length(FLA$Tm))
## Milwaukee
MIL<-read_html("http://www.baseball-reference.com/teams/MIL/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
MIL$Current<-rep("Milwaukee Brewers", length(MIL$Tm))
## Minnesota
MIN<- read_html("http://www.baseball-reference.com/teams/MIN/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
MIN$Current<-rep("Minnesota Twins", length(MIN$Tm))
## NY Mets - just y
NYM<-read_html("http://www.baseball-reference.com/teams/NYM/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
NYM$Current<-rep("New York Mets", length(NYM$Tm))
## NY Yanks - yuck 
NYY<-read_html("http://www.baseball-reference.com/teams/NYY/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
NYY$Current<-rep("New York Yankees", length(NYY$Tm))
## Oakland
OAK<-read_html("http://www.baseball-reference.com/teams/OAK/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
OAK$Current<-rep("Oakland Athletics", length(OAK$Tm))
## Philly
PHI<-read_html("http://www.baseball-reference.com/teams/PHI/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
PHI$Current<-rep("Philadelphia Phillies", length(PHI$Tm))
## Pittsburgh
PIT<-read_html("http://www.baseball-reference.com/teams/PIT/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
PIT$Current<-rep("Pittsburgh Pirates", length(PIT$Tm))
## San Diego
SDP<-read_html("http://www.baseball-reference.com/teams/SDP/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
SDP$Current<-rep("San Diego Padres", length(SDP$Tm))
## SF - bandwagoners
SFG<-read_html("http://www.baseball-reference.com/teams/SFG/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
SFG$Current<-rep("San Francisco Giants", length(SFG$Tm))
## Seatull
SEA<-read_html("http://www.baseball-reference.com/teams/SEA/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
SEA$Current<-rep("Seattle Mariners", length(SEA$Tm))
## Tampa Bay
TBD<-read_html("http://www.baseball-reference.com/teams/TBD/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
TBD$Current<-rep("Tampa Bay Rays", length(TBD$Tm))
## Toronto
TOR<-read_html("http://www.baseball-reference.com/teams/TOR/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
TOR$Current<-rep("Toronto Blue Jays", length(TOR$Tm))
## St. Louis
STL<-read_html("http://www.baseball-reference.com/teams/STL/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
STL$Current<-rep("St. Louis Cardinals", length(STL$Tm))
## Texas
TEX<-read_html("http://www.baseball-reference.com/teams/TEX/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
TEX$Current<-rep("Texas Rangers", length(TEX$Tm))
## Washington 
WSN<-read_html("http://www.baseball-reference.com/teams/WSN/") %>%
  html_nodes("#franchise_years") %>%
  html_table() %>%as.data.frame()
WSN$Current<-rep("Washington Nationals", length(WSN$Tm))

# create a table called baseball that contains all of the teams' franchise histories
baseball<-rbind(ATL, AR, BAL, BOS, CHC, CWS, CIN, CLE, COL, DET, HOU, KSR, ANA, 
                  LAD, FLA, MIL, MIN, NYM, NYY, OAK, PHI, PIT, SDP, SFG,
                  SEA, STL, TBD, TEX, TOR, WSN)
# at the end, be sure to print out the dimensions of your baseball table.
dim(baseball)

library(stringr)
# This code checks to see if text in table has regular space character
# Because the text from the web uses a non-breaking space, we expect there to be a mismatch
# I'm converting to raw because when displayed on screen, we cannot see the difference between
# a regular breaking space and a non-breaking space.
all.equal(charToRaw(baseball$Tm[1]), charToRaw("Arizona Diamondbacks"))
# identify which columns are character columns
char_cols <- which(lapply(baseball, typeof) == "character")

# for each character column, convert to UTF-8
# then replace the non-breaking space with a regular space
for(i in char_cols){
    baseball[[i]] <- str_conv(baseball[[i]], "UTF-8")
    baseball[[i]] <- str_replace_all(baseball[[i]],"\\s"," ")
    baseball[[i]] <- str_replace_all(baseball[[i]], "[:space:]"," " )  # you might have to use this depending on your operating system and which meta characters it recognizes
}

# check to see if the conversion worked
## should now be TRUE
all.equal(charToRaw(baseball$Tm[1]), charToRaw("Arizona Diamondbacks"))

baseball2<-baseball %>% group_by(., Current) %>% filter(., Year>=2001 && Year <= 2016)
#Total Wins
wins<-baseball2 %>% summarise(., TotalWins = sum(W))
wins
#Total Losses
loss<-baseball2 %>% summarise(., TotalLosses = sum(L))
loss
#Total Runs Scored
runs<-baseball2 %>% summarise(., RunScored = sum(R))
runs
# Total Runs Allowed
runs_allowed<-baseball2 %>% summarise(., RunAllowed = sum(RA))
runs_allowed
# Total Win Percentage
winper<-baseball2 %>% summarise(., WinPercentage = sum(W)/sum(G))
winper
# Total Pythagorean Win Percantage
pyth<-baseball2 %>% summarise(. , PythWinPercentage = sum(R^2)/(sum(R^2) + sum(RA^2)))
pyth

summary_table<-data.frame(wins, loss[, 2], runs[, 2], runs_allowed[, 2], winper[, 2], pyth[, 2])
sorted<-summary_table %>% arrange(. , desc(PythWinPercentage))
sorted
require(stringr)
which(str_detect(baseball$Managers,"([A-Z]\\.[A-Z][a-z]+|[A-Z]\\.[A-Z]'[A-Z][a-z]+|[A-Z][a-z][A-Z][a-z]+|[A-Z]\\.[A-Z][a-z][:space:][A-Z][a-z]+)[:space:]\\(([0-9]{2,3})-([0-9]{2,3})\\)" ) == FALSE)

mgrs<-str_match_all(baseball$Managers,"([A-Z]\\.[A-Z][a-z]+|[A-Z]\\.[A-Z]'[A-Z][a-z]+|[A-Z]\\.[A-Z][a-z][A-Z][a-z]+)[:space:]\\(([0-9]{1,3})-([0-9]{1,3})\\)")
bos<-str_match_all(BOS$Managers,"([A-Z]\\.[A-Z][a-z]+|[A-Z]\\.[A-Z]'[A-Z][a-z]+|[A-Z][a-z][A-Z][a-z]+)[:space:]\\(([0-9]{1,3})-([0-9]{1,3})\\)")


# wins - accounted for single digit, games ending in 0
#names - apostrophe, two capital letters

mgrs.df<-data.frame()
mgr.names<-character(0)
mgr.wins<-numeric(0)
mgr.loss<-numeric(0)
for (i in 1:length(mgrs)) {
  mgr.names<-c(mgr.names, mgrs[[i]][,2])
  mgr.wins<-c(mgr.wins, mgrs[[i]][,3])
  mgr.loss<-c(mgr.loss, mgrs[[i]][,4])
}
mgr.wins<-as.numeric(mgr.wins)
mgr.loss<-as.numeric(mgr.loss)
mgr.df<-data.frame(Managers = mgr.names, Wins = mgr.wins, Losses = mgr.loss)  
mgr.df2<-mgr.df %>% group_by(.,Managers) %>% summarise(., Games = sum(Wins, Losses), TotalWin = sum(Wins), TotalLoss = sum(Losses), TotalWinPer = sum(Wins)/Games) %>% arrange(.,desc(Games))
head(mgr.df2, 10) %>% as.data.frame()
