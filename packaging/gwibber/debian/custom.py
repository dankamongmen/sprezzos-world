from os.path import join, exists

prv_file = join("/usr/share/gwibber/data", "twitter")
if exists(prv_file):
  f = open(prv_file, "r")
  try:
    data = eval(f.read())
  except:
    pass

TWITTER_OAUTH_KEY = data["TWITTER_OAUTH_KEY"]
TWITTER_OAUTH_SECRET = data["TWITTER_OAUTH_SECRET"]
