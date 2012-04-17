tell application "Mail"
     set cm to ¬
         (item 1 of (messages of item 1 of TARGETBOX whose id is MAILID))
     set junk mail status of cm to not junk mail status of cm
end tell
