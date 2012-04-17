tell application "Mail"
	get email addresses of item 1 of (accounts whose name is "THEACCOUNT")
	set theAddress to item 1 of result
end tell
