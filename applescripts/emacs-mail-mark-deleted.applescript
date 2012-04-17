tell application "Mail"
	get item 1 of TARGETBOX
	set messageList to (messages of result whose id is MAILID)
	set theMessage to item 1 of messageList
        set deleted status of theMessage to true
end tell
