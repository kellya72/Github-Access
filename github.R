#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)
library(xml2)
oauth_endpoints("github")

# Change based on what you 
myapp <- oauth_app(appname = "kellya72cs3012",
                   key = "fef796b7cc0c4b14c92b",
                   secret = "c3a5fd5b70e7e893593997a73705e9e988d993c0")
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

gtoken <- config(token = github_token)
 
getFollowers <- function(username)
{
  followersList <- GET(paste0("https://api.github.com/users/", username, "/followers"), gtoken)
  json1 = content(followersList)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  followers <- githubDF$login
  return (followers);
}
#getFollowers("kellya72")

numberOfFollowers <- function(username)
{
  followers = getFollowers(username)
  numberOfFollowers = length(followers)
  return(numberOfFollowers)
}
#numberOfFollowers("kellya72")

getFollowing <- function(username)
{
  followingList <- GET(paste0("https://api.github.com/users/", username, "/following"), gtoken)
  json1 = content(followingList)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  following <- githubDF$login
  return (following);
}
#getFollowing("kellya72")

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
#getCurrentUserFollowing()

ListOfRepositories <- function(username){
  repositoriesList = GET(paste0("https://api.github.com/users/", username, "/repos"), gtoken)
  json1 = content(repositoriesList)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  repositories <- githubDF$name
  return (repositories);
}
#ListOfRepositories("kellya72")

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
#getNumberOfCommits("kellya72")

repoCommits <- function(username, repo){
  commitsList <- GET(paste0("https://api.github.com/repos/", username,"/", repo, "/commits"),gtoken)
  json1 = content(commitsList)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  commits = githubDF$commit$message
  return(commits)
}
#repoCommits("kellya72","Github-Access")



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
  orgs= get50Organisations()
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