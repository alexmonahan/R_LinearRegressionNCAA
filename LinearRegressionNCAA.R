#First we need to load in the data
teams <- read.csv("http://statweb.stanford.edu/~jgorham/teams.csv",as.is=TRUE)
games <- read.csv("http://statweb.stanford.edu/~jgorham/games.csv",as.is=TRUE)

#All of the necessary team names we need for this analysis
all.teams <- sort(unique(c(teams$team,games$home,games$away)))

#Number of teams and games
nrow(teams)
nrow(games)

#Let's look at how our Stanford team performed now...
teams[teams$team == "stanford-cardinal", ]

#New dataframe with the stanford games
games.stanford <- games[(games$home == "stanford-cardinal") |
                          (games$away == "stanford-cardinal"), ]
nrow(games.stanford)

#Number of games stanford won!
won <- with(games.stanford, ((home == "stanford-cardinal") & (homeScore > awayScore)) | ((away == "stanford-cardinal") & (homeScore < awayScore)))
nrow(games.stanford[won, ])
#Stanford won 19 games this passed season

#We will now implement a simple linear regression model
#yi = quality of home(i) − quality of away(i) + noise
X0 <- as.data.frame(matrix(0,nrow(games),length(all.teams))) 
names(X0) <- all.teams

#Our response vector
y <- games$homeScore - games$awayScore

#We will now fill in X0, the matrix we initialized earlier
for(team in all.teams) {
  X0[,team] <- 1*(games$home==team) - 1*(games$away==team)
}

#The following basically forces βj = 0 for the j corresponding to Stanford
X <- X0[,names(X0) != "stanford-cardinal"]

#Fit the regression
reg.season.games <- which(games$gameType=="REG")
fit <- lm(y ~ 0 + ., data=X, subset=reg.season.games) 
head(coef(summary(fit)))
summary(fit)$r.squared

#With βstanford = 0, then the estimate βberkley is the estimated score difference (berkley again stanford)
coef(fit)["`california-golden-bears`"]

#Expected Duke-UNC score
coef(fit)["`duke-blue-devils`"] - coef(fit)["`north-carolina-tar-heels`"]

#Analyzing home court advantage! Here we include games that were held at a neutral location
homeAdv <- 1 - games$neutralLocation
Xh <- cbind(homeAdv=homeAdv, X)
homeAdv.mod <- lm(y ~ 0 + ., data=Xh, subset=reg.season.games) head(coef(summary(homeAdv.mod)), 1)
