{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-} -- this is required for use of :> from ServantAPI
{-# LANGUAGE DeriveGeneric #-} -- required for use of deriving(Generic)
{-# LANGUAGE DeriveAnyClass #-} -- required for use of deriving(FromJSON)
{-# LANGUAGE DuplicateRecordFields #-} -- if two endpoints have a value of the same name, e.g. GHUserData and GHUserBasicInfoers both have a 'login' value, this allows for that

module MyGitHubAPI where

import Data.Aeson ( FromJSON, ToJSON )
import Data.Proxy ( Proxy(..) )
import Data.Text ( Text )
import GHC.Generics ( Generic )
import Servant.Client as SClient
    ( client,
      mkClientEnv,
      ClientEnv,
      ClientM,
      BaseUrl(BaseUrl),
      Scheme(Http) )
import Servant.API
    ( type (:<|>)(..),
      BasicAuth,
      BasicAuthData,
      Capture,
      JSON,
      Header,
      type (:>),
      Get )
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

type GHUserName = Text
type GHRepoName = Text
type GHOrgName = Text
type ProgramLanguage = Text
type GHHeaderUserAgent = Text
type GHHeaderAccept = Text

-- TODO: Resolve the issue with Haskell reserved keywords conflicting with key names in GitHub API, e.g. 'type'. I might be able to work around this entirely.
-- Had some issues with the values in the JSON returned that we want to collect may have the same value as a Haskell keyword. 
-- For example, there are values in the JSON labeled 'type', but we can't use these like normal, as Haskell will throw an error.
-- This function should resolve this issue:

-- https://stackoverflow.com/questions/48474587/how-to-deal-with-haskells-reserved-keywords-in-record-fields

-- fieldRename :: String -> String
-- fieldRename "_type" = "type"
-- ownerFieldRename name = name

-- I'm just going to assume going forward that every possible value will be a maybe, even if its guaranteed to be there. This
-- will save me having to check if I have to unpack that specific value from the Maybe type every time

-- I'm going to do a very catch-all approach in terms of what endpoints I chose in the GitHub API here. This should give me a lot of
-- leeway for the visualisation task, as I won't have to keep throwing in more and more functions at a later point if I have 
-- all the individual calls done already. Since I'm separating the call of each individual GH-data-type into its own function in 
-- main, and if, at a later point, I want to create an overarching method for gathering all the information at once for my web
-- app visualisation, then this means that this overarching method can call all the calls for the GH-data-types in the required order,
-- extract only the relevant information needed for the subsequent call, and then move on. 


-- first /users/ https://docs.github.com/en/free-pro-team@latest/rest/reference/users
data GHUserData =
    GHUserData { 
        login :: Maybe Text,            -- some of the information here is private. With my token, I can see the private info for my account
        name :: Maybe Text,             -- but get nothing back for owned_private_repos for, say, torvalds
        avatar_url :: Maybe Text,
        url :: Maybe Text,
        public_repos :: Maybe Int,
        owned_private_repos :: Maybe Int,
        hireable :: Maybe Bool,
        company :: Maybe Text
    } deriving (Generic, FromJSON, ToJSON, Show)

-- The users endpoints details the following, which are all URLs meaning we have to access them as their own endpoints. Since thw structure of the 
-- get requests here are trivial, we don't need to collect these URLs from the user-call, we can just create our own unqiue endpoints for each 
-- one that we want to use:
--   "followers_url": "https://api.github.com/users/octocat/followers",                     -- using this one
--   "following_url": "https://api.github.com/users/octocat/following{/other_user}",        -- using this one
--   "gists_url": "https://api.github.com/users/octocat/gists{/gist_id}",
--   "starred_url": "https://api.github.com/users/octocat/starred{/owner}{/repo}",          -- using this one
--   "subscriptions_url": "https://api.github.com/users/octocat/subscriptions",
--   "organizations_url": "https://api.github.com/users/octocat/orgs",                      -- using this one
--   "repos_url": "https://api.github.com/users/octocat/repos",                             -- using this one
--   "events_url": "https://api.github.com/users/octocat/events{/privacy}",                 
--   "received_events_url": "https://api.github.com/users/octocat/received_events"

-- https://docs.github.com/en/free-pro-team@latest/rest/reference/users#list-followers-of-a-user

-- much of the following data types can be reused for completely different calls. This is because the data extracted is consistent, 
-- and we'll usually extract the individual data from these datapoints to make a subsequent, more detailed call, if needed.

data GHUserBasicInfo =                            -- used for when a endpoint provides a lists of users, doesn't gointo great detail
    GHUserBasicInfo {                             -- about the user, but we can extract the 'login' value and make subsequent calls
        login :: Maybe Text,                      -- based on this particular user
        avatar_url :: Maybe Text
    } deriving (Generic, FromJSON, Show)

data GHRepoBasicInfo =                         -- used for the user's repos, repos the user has starred
    GHRepoBasicInfo {
        name :: Maybe Text,
        owner :: Maybe GHUserBasicInfo,
        description :: Maybe Text
    } deriving (Generic, FromJSON, Show)

data GHPullRequests = 
    GHPullRequests {
        title :: Maybe Text                      
    } deriving (Generic, FromJSON, Show)

data GHOrgBasicInfo = 
    GHOrgBasicInfo {
        login :: Maybe Text,
        avatar_url :: Maybe Text
    } deriving (Generic, FromJSON, Show)

data GHCommitBasicInfo =
    GHCommitBasicInfo {
        sha :: Maybe Text,
        url :: Maybe Text
    } deriving (Generic, FromJSON, Show)

data GHBranchBasicInfo = 
    GHBranchBasicInfo {
        name :: Maybe Text,
        commit :: Maybe GHCommitBasicInfo,
        protected :: Maybe Bool
    } deriving (Generic, FromJSON, Show)

data GHCommitActivity = 
    GHCommitActivity {
        days :: Maybe [Int],
        total :: Maybe Int,
        week :: Maybe Int
    } deriving (Generic, FromJSON, Show)

data GHWeeksContributor = 
    GHWeeksContributor {
        w :: Maybe Int,
        a :: Maybe Int,
        d :: Maybe Int,
        c :: Maybe Int
    } deriving (Generic, FromJSON, Show)

data GHContributorActivity = 
    GHContributorActivity {
        author :: GHUserBasicInfo,
        total :: Maybe Int,
        weeks :: Maybe [GHWeeksContributor]
    } deriving (Generic, FromJSON, Show)


-- For future reference, I might need to add a Languages call when I get to the visualisation project, so I'll leave what I've done of it here
-- and come back to it

-- data Language = Language String Int         -- have to specify this type this way, we can't use record format 
--                                             -- as each element of the JSON will be something like "C": 1234 "Java" :3453,
--                                             -- which makes it impossible to parse with record format

-- instance FromJSON Language where
--   parseJSON (Object o) =                -- note that we are using an alternative method for defining FromJSON here.
--     Language <$> o .: "language"     -- we could have used template supportK instead.
--                 <*> o .: "lines"

--   parseJSON _ = mzero

-- I want to test exactly how the syntax of MyGitHubAPI (defined directly below) is applied to the "= client myGitHubAPI" instruction
-- further below. I will use Pull Requests for this, i.e. https://developer.github.com/v3/pulls/, which is defined with 
-- GET /repos/:owner/:repo/pulls - DONE

type MyGitHubAPI = 
    "users"                                             -- getGHUserName
            :> Header "User-Agent" GHHeaderUserAgent
            :> Header "Accept" GHHeaderAccept
            :> BasicAuth "github" Int
            :> Capture "username" GHUserName
            :> Get '[JSON] GHUserData
    :<|> "users"                                        -- GET /users/{username}/followers
            :> Header "User-Agent" GHHeaderUserAgent 
            :> Header "Accept" GHHeaderAccept
            :> BasicAuth "github" Int  
            :> Capture "user" GHUserName 
            :> "followers"
            :> Get '[JSON] [GHUserBasicInfo] 
    :<|> "repos"                                        -- getGHPullRequests
            :> Header "User-Agent" GHHeaderUserAgent
            :> Header "Accept" GHHeaderAccept
            :> BasicAuth "github" Int
            :> Capture "owner" GHUserName
            :> Capture "repo" GHRepoName
            :> "pulls"
            :> Get '[JSON] [GHPullRequests]
    :<|> "users"                                        -- GET /users/{username}/following
            :> Header "User-Agent" GHHeaderUserAgent 
            :> Header "Accept" GHHeaderAccept
            :> BasicAuth "github" Int  
            :> Capture "user" GHUserName 
            :> "following"
            :> Get '[JSON] [GHUserBasicInfo] 
    :<|> "users"                                        -- /users/{username}/starred
            :> Header "User-Agent" GHHeaderUserAgent 
            :> Header "Accept" GHHeaderAccept
            :> BasicAuth "github" Int  
            :> Capture "user" GHUserName 
            :> "starred"
            :> Get '[JSON] [GHRepoBasicInfo]            
    :<|> "users"                                         -- /users/{username}/repos
            :> Header "User-Agent" GHHeaderUserAgent 
            :> Header "Accept" GHHeaderAccept
            :> BasicAuth "github" Int  
            :> Capture "user" GHUserName 
            :> "repos"
            :> Get '[JSON] [GHRepoBasicInfo]           
     :<|> "users"                                     -- users/steviejeebies/orgs
            :> Header "User-Agent" GHHeaderUserAgent 
            :> Header "Accept" GHHeaderAccept
            :> BasicAuth "github" Int  
            :> Capture "user" GHUserName 
            :> "orgs"
            :> Get '[JSON] [GHOrgBasicInfo]   
    :<|> "orgs"                                     -- /orgs/{org}/members
            :> Header "User-Agent" GHHeaderUserAgent 
            :> Header "Accept" GHHeaderAccept
            :> BasicAuth "github" Int  
            :> Capture "org" GHOrgName 
            :> "members"
            :> Get '[JSON] [GHUserBasicInfo] 
    :<|> "repos"                                        -- /repos/{owner}/{repo}/contributors
            :> Header "User-Agent" GHHeaderUserAgent
            :> Header "Accept" GHHeaderAccept
            :> BasicAuth "github" Int
            :> Capture "owner" GHUserName
            :> Capture "repo" GHRepoName
            :> "contributors"
            :> Get '[JSON] [GHUserBasicInfo]
    :<|> "repos"                                         -- /repos/{owner}/{repo}/branches
            :> Header "User-Agent" GHHeaderUserAgent
            :> Header "Accept" GHHeaderAccept
            :> BasicAuth "github" Int
            :> Capture "owner" GHUserName
            :> Capture "repo" GHRepoName
            :> "branches"
            :> Get '[JSON] [GHBranchBasicInfo]  
    :<|> "repos"                                          -- /repos/{owner}/{repo}/stats/code_frequency
            :> Header "User-Agent" GHHeaderUserAgent
            :> Header "Accept" GHHeaderAccept
            :> BasicAuth "github" Int
            :> Capture "owner" GHUserName
            :> Capture "repo" GHRepoName
            :> "stats"
            :> "code_frequency"
            :> Get '[JSON] [[Int]]  
    :<|> "repos"                                          -- /repos/{owner}/{repo}/stats/commit_activity
            :> Header "User-Agent" GHHeaderUserAgent
            :> Header "Accept" GHHeaderAccept
            :> BasicAuth "github" Int
            :> Capture "owner" GHUserName
            :> Capture "repo" GHRepoName
            :> "stats"
            :> "commit_activity"
            :> Get '[JSON] [GHCommitActivity]   
    :<|> "repos"                                          -- /repos/{owner}/{repo}/stats/contributors (this is all contributor commit activity for a given repo) https://docs.github.com/en/free-pro-team@latest/rest/reference/repos#get-all-contributor-commit-activity         
            :> Header "User-Agent" GHHeaderUserAgent
            :> Header "Accept" GHHeaderAccept
            :> BasicAuth "github" Int
            :> Capture "owner" GHUserName
            :> Capture "repo" GHRepoName
            :> "stats"
            :> "contributors"
            :> Get '[JSON] [GHContributorActivity]                        
                                                                           


-- FOR REPO LANGUAGES
    -- :<|> "repos"                                        -- /repos/{owner}/{repo}/languages 
    --         :> Header "User-Agent" GHHeaderUserAgent
    --         :> Header "Accept" GHHeaderAccept
    --         :> BasicAuth "github" Int
    --         :> Capture "owner" GHUserName
    --         :> Capture "repo" GHRepoName
    --         :> "languages"
    --         :> Get '[JSON] [Language] 

--getRepoLanguages :: Maybe GHHeaderUserAgent -> Maybe GHHeaderAccept -> BasicAuthData -> GHUserName -> GHRepoName -> ClientM [Language]


                                                    

myGitHubAPI :: Proxy MyGitHubAPI
myGitHubAPI = Proxy

getGHUserName :: Maybe GHHeaderUserAgent -> Maybe GHHeaderAccept -> BasicAuthData -> GHUserName -> ClientM GHUserData
getGHUserFollowers :: Maybe GHHeaderUserAgent -> Maybe GHHeaderAccept -> BasicAuthData -> GHUserName -> ClientM [GHUserBasicInfo]
getGHPullRequests :: Maybe GHHeaderUserAgent -> Maybe GHHeaderAccept -> BasicAuthData -> GHUserName -> GHRepoName -> ClientM [GHPullRequests]
getGHUserFollowing :: Maybe GHHeaderUserAgent -> Maybe GHHeaderAccept -> BasicAuthData -> GHUserName -> ClientM [GHUserBasicInfo]
getGHUserStarred :: Maybe GHHeaderUserAgent -> Maybe GHHeaderAccept -> BasicAuthData -> GHUserName -> ClientM [GHRepoBasicInfo]
getGHUserRepos :: Maybe GHHeaderUserAgent -> Maybe GHHeaderAccept -> BasicAuthData -> GHUserName -> ClientM [GHRepoBasicInfo]
getGHUserOrgs :: Maybe GHHeaderUserAgent -> Maybe GHHeaderAccept -> BasicAuthData -> GHUserName -> ClientM [GHOrgBasicInfo]
getGHOrgMembers :: Maybe GHHeaderUserAgent -> Maybe GHHeaderAccept -> BasicAuthData -> GHOrgName -> ClientM [GHUserBasicInfo]
getRepoContributors :: Maybe GHHeaderUserAgent -> Maybe GHHeaderAccept -> BasicAuthData -> GHUserName -> GHRepoName -> ClientM [GHUserBasicInfo]
getRepoBranches :: Maybe GHHeaderUserAgent -> Maybe GHHeaderAccept -> BasicAuthData -> GHUserName -> GHRepoName -> ClientM [GHBranchBasicInfo]
getCodeFrequencyRepoWeekly :: Maybe GHHeaderUserAgent -> Maybe GHHeaderAccept -> BasicAuthData -> GHUserName -> GHRepoName -> ClientM [[Int]]
getCommitActivityRepoYear :: Maybe GHHeaderUserAgent -> Maybe GHHeaderAccept -> BasicAuthData -> GHUserName -> GHRepoName -> ClientM [GHCommitActivity]
getContributorCommitActivity :: Maybe GHHeaderUserAgent -> Maybe GHHeaderAccept -> BasicAuthData -> GHUserName -> GHRepoName -> ClientM [GHContributorActivity]


-- adding getGHPullRequests to the myGitHubAPI definition
-- From testing, this means that the ordering here is the same as the ordering from 'type MyGitHubAPI' above.
( getGHUserName 
    :<|> getGHUserFollowers 
    :<|> getGHPullRequests 
    :<|> getGHUserFollowing 
    :<|> getGHUserStarred 
    :<|> getGHUserRepos 
    :<|> getGHUserOrgs 
    :<|> getGHOrgMembers 
    :<|> getRepoContributors 
    :<|> getRepoBranches 
    :<|> getCodeFrequencyRepoWeekly
    :<|> getCommitActivityRepoYear
    :<|> getContributorCommitActivity
    ) = client myGitHubAPI

-- environment, used for establishing connection, defined in this file so it does not have to be defined in the Lib file
env :: IO SClient.ClientEnv
env = do
  manager <- newManager tlsManagerSettings
  return $ SClient.mkClientEnv manager (SClient.BaseUrl SClient.Http "api.github.com" 80 "")