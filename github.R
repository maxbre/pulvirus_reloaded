### 3. Configure git with Rstudio ############################################

## set your user name and email:
usethis::use_git_config(user.name = "raffaele-morelli", user.email = "raffaele.morelli@isprambiente.com")

## create a personal access token for authentication:
usethis::create_github_token()

## set personal access token:
credentials::set_github_pat("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")

## or store it manually in '.Renviron':
# usethis::edit_r_environ()
## store your personal access token with: GITHUB_PAT=xxxyyyzzz
## and make sure '.Renviron' ends with a newline

# ----------------------------------------------------------------------------

#### 4. Verify settings ######################################################

usethis::git_sitrep()

## Your username and email should be stated correctly in the output. 
## Also, the report shoud cotain something like:
## 'Personal access token: '<found in env var>''

# ----------------------------------------------------------------------------

## THAT'S IT!

