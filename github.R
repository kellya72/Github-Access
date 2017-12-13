#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)
library(xml2)
oauth_endpoints("github")

# Change based on what you 
myapp <- oauth_app(appname = "cs3012assignment5",
                   key = "5e8c13e2d32096020061",
                   secret = "dc7a74e4723846ac8cea04d3faeacfd0ebc6d39f")
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

gtoken <- config(token = github_token)
 
getFollowers <- function(username)
{
  followersList <- GET(paste0("https://api.github.com/users/",username, "/followers"), gtoken)
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
numberOfFollowers("mamut")

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
#numberFollowing("kellya72")

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
  repositoriesList = GET(paste0("https://api.github.com/users/", username, "/repos"), gtoken)
  json1 = content(repositoriesList)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  repositories <- githubDF$name
  return (repositories);
}
#ListOfRepositories("kellya72")
getCurrentUserListOfRepositories <- function(){
  repositoriesList = GET(paste0("https://api.github.com/users/repos"), gtoken)
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

get50Organisations <- function(){
  orgs <- GET("https://api.github.com/organizations?per_page=50",gtoken)
  json1 = content(orgs)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  list = githubDF$login
  return(list)
}
get50Organisations()

getListOfReposFromOrg <- function(org){
  repos <- GET(paste0("https://api.github.com/orgs/",org, "/repos"),gtoken)
  json1 = content(repos)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  reposList = githubDF$name
  reposList
  return(reposList)
}
getListOfReposFromOrg("softa")

listOfAllRepos <- function(){
  orgs= get5Organisations()
  #repos= c()
  reposList= c()
  count=0
  for (i in 1:50){
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
  orgs = get50Organisations()
  allMembers = c()
  count=0
  for(i in 1:50){
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

checkSame <- function(user1, user2){
  if(user1==user2){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}
#checkSame("kellya72","kellya72")
#checkSame("kellya72","k")
?rbind
checkForDuplicateMembers <- function(){
  membersList= getAllMembers()
  i=1
  different= TRUE
  while(count<length(membersList)){
    if(checkSame(membersList[i],membersList[i+1])){
      membersList[i+1]=NULL
      different=FALSE
    }
    i=i+1
  }
  return(different)
}
checkForDuplicateMembers()

membersOfOrgsTotalCommits <- function(){
  members= getAllMembers()
  orgRepos= listOfAllRepos()
  commits= c()
  for(i in 1:length(members)){
    for(j in 1:length(orgRepos)){
      temp=commits[i]
      numberOfCommits= numberOfCommitsInRepo(members[i],orgRepos[j])
      commits[i]= temp + numberOfCommits
    }
  }
  df = data.frame(members,commits)
  return(df)
}
membersOfOrgsTotalCommits()

membersOfOrgsTotalCommits <- function(){
  members= getAllMembers()
  commits= c()
  for(i in 1:length(members)){
    commits[i]= getNumberOfCommits(members[i])
  }
  df= data.frame(members,commits)
  return(df)
}
membersOfOrgsTotalCommits()


get500Users <- function(){
  users = GET("https://api.github.com/users?per_page=500",gtoken)
  json1 = content(users)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  usersList <- githubDF$login
  return (usersList);
}
get500Users()

allUserRepoNumbers <-function(){
  users= getAllMembers()
  noFollowers= c()
  noCommits= c()
  for(i in 1:length(users)){
    noFollowers[i]= numberOfFollowers(users[i])
    noCommits[i]= getNumberOfCommits(users[i])
   }
   df = rbind(users,noFollowers,noCommits)
   return(df)
 }
> userData= allUserRepoNumbers()

userData
write.csv(userData,file="userData.csv")
?write.csv
?lapply
?rbind
