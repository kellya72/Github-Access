#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)
library(xml2)
detach(package:plotly, unload=TRUE)
library(plotly)
packageVersion('plotly')

oauth_endpoints("github")

# Change based on what you 
myapp <- oauth_app(appname = "kellya72assignment5",
                   key = "560ed7ecdc82972fa209",
                   secret = "f00dbab3f27ef247be30b6393e0f92436b239d3e")
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

gtoken <- config(token = github_token)

getFollowers <- function(username)
{
  followersList <- GET(paste0("https://api.github.com/users/", username, "/followers?per_page=100&page="), gtoken)
  json1 = content(followersList)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  followers <- githubDF$login
  return (followers);
}
getFollowers("kellya72")

numberOfFollowers <- function(username)
{
  followers = getFollowers(username)
  numberOfFollowers = length(followers)
  return(numberOfFollowers)
}
numberOfFollowers("bensie")

getFollowing <- function(username)
{
  followingList <- GET(paste0("https://api.github.com/users/", username, "/following"), gtoken)
  json1 = content(followingList)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  following <- githubDF$login
  return (following);
}
getFollowing("kellya72")

numberFollowing <- function(username)
{
  following = getFollowing(username)
  numberFollowing = length(following)
  return(numberFollowing)
}
numberFollowing("kellya72")

getCurrentUserFollowers <- function()
{
  followersList <- GET(paste0("https://api.github.com/user/followers"), gtoken)
  json1 = content(followersList)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  followers <- githubDF$login
  return (followers);
}
#getCurrentUserFollowers()

getCurrentUserFollowing <- function()
{
  followingList <- GET(paste0("https://api.github.com/user/following"), gtoken)
  json1 = content(followingList)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  following <- githubDF$login
  return (following);
}
getCurrentUserFollowing()

ListOfRepositories <- function(username){
  repositoriesList = GET(paste0("https://api.github.com/users/", username, "/repos?per_page=100&page="), gtoken)
  json1 = content(repositoriesList)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  repositories <- githubDF$name
  return (repositories);
}
ListOfRepositories("kellya72")

getCurrentUserListOfRepositories <- function(){
  repositoriesList = GET(paste0("https://api.github.com/users/repos?per_page=100&page="), gtoken)
  json1 = content(repositoriesList)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  repositories <- githubDF$name
  return (repositories);
}
getCurrentUserListOfRepositories()

numberOfRepositories <- function(username)
{
  repositories = ListOfRepositories(username)
  number = length(repositories)
  return(number)
}
#numberOfRepositories("kellya72")

getNumberOfCommits <- function(username){
  repositories=ListOfRepositories(username)
  count= numberOfRepositories(username)
  totalNumberOfCommits=0
  for(i in 1:count){
    repo = repositories[i]
    commits = repoCommits(username, repo)
    numberOfCommits = length(commits)
    totalNumberOfCommits = totalNumberOfCommits+numberOfCommits
  }
  return(totalNumberOfCommits)
}
getNumberOfCommits("kellya72")

repoCommits <- function(username, repo){
  commitsList <- GET(paste0("https://api.github.com/repos/", username,"/", repo, "/commits"),gtoken)
  json1 = content(commitsList)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  commits = githubDF$commit$message
  return(commits)
}
#repoCommits("kellya72","Github-Access")

numberOfCommitsInRepo <- function(username, repo){
  commits= repoCommits(username, repo)
  number= length(commits)
  return(number)
}
numberOfCommitsInRepo("Kellya72","Github-Access")

#############################################
#Assignment 6 #

get30Organisations <- function(){
  orgs <- GET("https://api.github.com/organizations?per_page=30",gtoken)
  json1 = content(orgs)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  list = githubDF$login
  return(list)
}
get30Organisations()

getListOfReposFromOrg <- function(org){
  repos <- GET(paste0("https://api.github.com/orgs/",org, "/repos?per_page=100"),gtoken)
  json1 = content(repos)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  reposList = githubDF$name
  reposList
  return(reposList)
}
getListOfReposFromOrg("softa")

listOfAllRepos <- function(){
  orgs= get3Organisations()
  #repos= c()
  reposList= c()
  count=0
  for (i in 1:30){
    organisation= orgs[i]
    repos= getListOfReposFromOrg(organisation)
    numberOfRepos= length(repos)
    length=count+numberOfRepos
    while (count<length) {
      for(i in 1:numberOfRepos){
        reposList[count]= repos[i]
        count= count+1
      }
    }
  }
  return(reposList)
}
listOfAllRepos()

getMembersOfOrg <- function(organisation){
  members <- GET(paste0("https://api.github.com/orgs/",organisation, "/members"),gtoken)
 # members <- GET(("https://api.github.com/orgs/rails/members"),gtoken)
  json1 = content(members)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  memberList <- githubDF$login
  return (memberList);
}
getMembersOfOrg("rails")

getAllMembers <- function(){
  orgs = get30Organisations()
  allMembers = c()
  count=0
  for(i in 1:30){
    members= getMembersOfOrg(orgs[i])
    noOfNewMembers= length(members)
    noOfMembers= noOfNewMembers + count
    while(count<noOfMembers){
      for(i in 1:noOfNewMembers){
        allMembers[count]= members[i]
        count= count+1
      }
    }
  }
  return(allMembers)
}
getAllMembers()

get100Users <- function(){
  users = GET("https://api.github.com/users?per_page=100",gtoken)
  json1 = content(users)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  usersList <- githubDF$login
  return (usersList);
}
get100Users() 

allMemberRepoNumbers <-function(){
  users= getAllMembers()
  noFollowers= c()
  noCommits= c()
  for(i in 1:length(users)){
    noFollowers[i]= numberOfFollowers(users[i])
    noCommits[i]= getNumberOfCommits(users[i])
   }
   df = cbind(users,noFollowers,noCommits)
   return(df)
 }
userData= allMemberRepoNumbers()
userData
write.csv(userData,file="memberData.csv")

allUserRepoNumbers <-function(){
  users= get100Users()
  noFollowers= c()
  noCommits= c()
  for(i in 1:length(users)){
    noFollowers[i]= numberOfFollowers(users[i])
    noCommits[i]= getNumberOfCommits(users[i])
  }
  df = cbind(users,noFollowers,noCommits)
  return(df)
}
userDataNotInOrg= allUserRepoNumbers()
userDataNotInOrg
write.csv(userData,file="notOrgMembers1.csv")

datadf = as.data.frame(userData)
library(plotly)
memberGraph <- plot_ly(data = datadf, x = ~noFollowers, y = ~noCommits, text = ~paste("User: ", users, '<br>Followers:', noFollowers,'<br>Commits:', commits),marker = list(size = 2,
                                                                                                                                                                            color = 'rgba(255, 182, 193, .9)',
                                                                                                                                                                            line = list(color = 'rgba(152, 0, 0, .8)',
                                                                                                                                                                                        width = 2)))%>%
  layout(title = "Members of Organisation: No of Commits vs No of followers",
         yaxis = list(zeroline = FALSE),
         xaxis = list(zeroline = FALSE))
memberGraph

datadfUsers = as.data.frame(userDataNotInOrg)


userGraph<- plot_ly(data = datadfUsers, x = ~noFollowers, y = ~noCommits, text = ~paste("User: ", users, '<br>Followers:', noFollowers,'<br>Commits:', commits), marker = list(size = 2,
                                                                                                                                                                               color = 'rgba(255, 182, 193, .9)',
                                                                                                                                                                               line = list(color = 'rgba(152, 0, 0, .8)',
                                                                                                                                                                                           width = 2)))%>%
layout(title = "First 100 users: No of Commits vs No of followers",
       yaxis = list(zeroline = FALSE),
       xaxis = list(zeroline = FALSE))
userGraph

allUserRepo <-function(){
  users= get100Users()
  noRepos= c()
  noCommits= c()
  for(i in 1:length(users)){
    noRepos[i]= numberOfRepositories(users[i])
  }
  df = cbind(users,noRepos)
  return(df)
}
numberRepoPerUser= allUserRepo()
write.csv(numberRepoPerUser,file="repoNumbers.csv")

repodf = as.data.frame(numberRepoPerUser)
repoBox <- plot_ly(data= repodf,y = ~noRepos, type = "box")%>%
layout(title = "Number Of Repositories")
repoBox

