-- require "emacs-mail-util.applescript"

tell application "Mail"
	set attachmentString to "("
	-- set acc to item 1 of (accounts whose name is "Gmail")
	-- set box to item 1 of (mailboxes of acc whose name is "Interesting")
	-- set messageList to messages of box whose id is 22698
	-- set theMessage to item 1 of messageList
    get item 1 of TARGETBOX
    set messageList to (messages of result whose id is MAILID)
    set theMessage to item 1 of messageList
	set attachList to mail attachments of theMessage
	repeat with theAttachment in result
		set attachmentstr to "("
		set attachmentstr to attachmentstr & my createLispAssoc("id", id of theAttachment)
		set attachmentstr to attachmentstr & my createLispAssoc("name", name of the theAttachment)
		set attachmentstr to attachmentstr & my createLispAssoc("mime", MIME type of the theAttachment)
		set attachmentstr to attachmentstr & my createLispAssoc("size", file size of the theAttachment)
		set attachmentstr to attachmentstr & my createLispAssoc("downloaded", downloaded of the theAttachment)
		set attachmentstr to attachmentstr  & ")"
		set attachmentString to attachmentString & attachmentstr
	end repeat
	set attachmentString to attachmentString & ")"
end tell
