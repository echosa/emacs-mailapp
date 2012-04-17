tell application "Mail"
	set theAddresses to ""
	get accounts
	repeat with theAccount in result
		get email addresses of theAccount
		repeat with theAddress in result
			set theAddresses to theAddresses & (name of the theAccount) & "|-|" & theAddress & "|||"
		end repeat
	end repeat
	theAddresses
end tell
