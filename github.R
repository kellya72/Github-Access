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
  
}
getFollowing <- function(username)
{
  
}
getCurrentUserFollowers <- function()
{
  
}
getNumberOfCommits <- function(username){
  
}
getNumberOfRepositories <- function(username){
  
}
