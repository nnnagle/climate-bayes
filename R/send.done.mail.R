# Send email
require(sendmailR)
library(mailR)
#sendmail_options(smtpServer="ASPMX.L.GOOGLE.COM")
settings <- jsonlite::fromJSON('~/Dropbox/git_root/climate-bayes/conf/settings.json')
from <- sprintf("<%s@gmail.com>", settings$gmail$username)
to <- sprintf("<%s@gmail.com>", settings$gmail$username)
subject <- "Hello from R"
body <- "I'm done!"
smtp = list(host.name = "smtp.gmail.com", port = 465,
            user.name = settings$gmail$username, passwd = settings$gmail$password, ssl = T)
send.mail(from=from, to=to, subject=subject, body=body, smtp=smtp, authenticate=T)
