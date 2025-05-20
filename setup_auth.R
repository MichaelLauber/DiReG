# Load required libraries
if (!require(DBI)) install.packages("DBI", repos = "https://cloud.r-project.org")
if (!require(RSQLite)) install.packages("RSQLite", repos = "https://cloud.r-project.org")
if (!require(sodium)) install.packages("sodium", repos = "https://cloud.r-project.org")
if (!require(openssl)) install.packages("openssl", repos = "https://cloud.r-project.org")
if (!require(shinymanager)) install.packages("shinymanager", repos = "https://cloud.r-project.org")
if (!require(bcrypt)) install.packages("bcrypt", repos = "https://cloud.r-project.org")
if (!require(optparse)) install.packages("optparse", repos = "https://cloud.r-project.org")

library(DBI)
library(RSQLite)
library(sodium)
library(openssl)
library(shinymanager)
library(bcrypt)
library(optparse)

# Parse command line arguments
option_list <- list(
  optparse::make_option(c("-p", "--passphrase"), type="character", default=NULL, 
                        help="Database passphrase", metavar="character"),
  optparse::make_option(c("-r", "--regular_user"), type="character", default=NULL, 
                        help="Regular username", metavar="character"),
  optparse::make_option(c("--regular_pwd"), type="character", default=NULL, 
                        help="Regular user password", metavar="character"),
  optparse::make_option(c("--regular_email"), type="character", default=NULL, 
                        help="Regular user email", metavar="character"),
  optparse::make_option(c("-a", "--admin_user"), type="character", default=NULL, 
                        help="Admin username", metavar="character"),
  optparse::make_option(c("--admin_pwd"), type="character", default=NULL, 
                        help="Admin password", metavar="character"),
  optparse::make_option(c("--admin_email"), type="character", default=NULL, 
                        help="Admin email", metavar="character"),
  optparse::make_option(c("-k", "--api_key"), type="character", default="", 
                        help="OpenAI API fallback key", metavar="character")
)

opt_parser <- optparse::OptionParser(option_list=option_list)
opt <- optparse::parse_args(opt_parser)

# Check for required arguments
if (is.null(opt$passphrase) || is.null(opt$regular_user) || is.null(opt$regular_pwd) || 
    is.null(opt$regular_email) || is.null(opt$admin_user) || is.null(opt$admin_pwd) ||
    is.null(opt$admin_email)) {
  stop("All required arguments must be provided. Use --help for more information.")
}

# Create credentials dataframe
credentials <- data.frame(
  user = c(opt$regular_user, opt$admin_user),
  password = sapply(list(opt$regular_pwd, opt$admin_pwd), function(x) bcrypt::hashpw(x)),
  start = NA,
  expire = NA,
  admin = c(FALSE, TRUE),
  email = c(opt$regular_email, opt$admin_email),
  api_key = c("", ""),
  is_active = c(TRUE, TRUE),
  is_hashed_password = c(TRUE, TRUE),
  stringsAsFactors = FALSE
)

# Create the database
shinymanager::create_db(
  credentials_data = credentials,
  sqlite_path = "database.sqlite",
  passphrase = opt$passphrase
)

# Connect to the database and verify
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "database.sqlite")
credentials_verify <- shinymanager::read_db_decrypt(
  conn = con,
  name = "credentials",
  passphrase = opt$passphrase
)
DBI::dbDisconnect(con)

# Generate and save encryption keys
key <- sodium::random(32)
encrypted_passphrase <- sodium::data_encrypt(charToRaw(opt$passphrase), key)
saveRDS(encrypted_passphrase, "encrypted_passphrase.rds")
saveRDS(key, "key_passphrase.rds")

# Generate AES key for OpenAI API encryption
aes_key_openai_api <- openssl::rand_bytes(32)  # 32 bytes = 256-bit key
saveRDS(aes_key_openai_api, "aes_key_openai_api.rds")

# Save fallback API key
if (opt$api_key != "") {
  saveRDS(opt$api_key, "fllback_api_key.rds")
  cat("Fallback API key saved.\n")
}