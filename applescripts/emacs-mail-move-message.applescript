tell application "Mail"
    set start to mailbox "STARTBOX" of account "TARGETACCOUNT"
    set target to mailbox "TARGETBOX" of account "TARGETACCOUNT"
    set theMessage to item 1 of (messages of start whose id is MAILID)
    set mailbox of theMessage to target
end tell
