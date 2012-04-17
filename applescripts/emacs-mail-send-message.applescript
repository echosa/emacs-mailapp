tell application "Mail"
    set newMail to make new outgoing message with properties {sender:"FROM",subject:"MSGSUBJECT",content:"BODY",visible:SHOWWINDOW}
	tell newMail
		SENDTO
	end tell
    SENDCMD
end tell
