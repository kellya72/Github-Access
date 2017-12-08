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
getFollowers("kellya72")
numberOfFollowers <- function(username)
{
  followers = getFollowers(username)
  numberOfFollowers = length(followers)
  return(numberOfFollowers)
}
numberOfFollowers("kellya72")
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
  
}
getNumberOfCommits <- function(username){
  
}
getNumberOfRepositories <- function(username){
  
}
