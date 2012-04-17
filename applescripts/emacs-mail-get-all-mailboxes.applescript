tell application "Mail"
	set allBoxes to ""
	get accounts
	repeat with thisAccount in result
       	set theseBoxes to ""
		set firstItem to 0
		repeat with thisBox in (mailboxes of thisAccount)
			if firstItem is 0 then
				set firstItem to 1
			else
				set theseBoxes to theseBoxes & "|-|"
			end if
			set theseBoxes to theseBoxes & (name of thisBox)
		end repeat
		set allBoxes to allBoxes & (name of thisAccount) & "|||" & theseBoxes & "|||"
	end repeat
	allBoxes
end tell
