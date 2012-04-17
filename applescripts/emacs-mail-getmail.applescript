-- require "emacs-mail-util.applescript"

tell application "Mail"
    set messagesString to "("
    get item 1 of TARGETBOX
    set messageList to (messages of result LISTFILTER)
    repeat with thisMessage in messageList
           set messageStr to "("
           set messageStr to messageStr & ¬
               my createLispAssoc("id", id of thisMessage)
           set messageStr to messageStr & ¬
               my createLispAssoc("subject", subject of thisMessage)
           set messageStr to messageStr & ¬
               my createLispAssoc("date", date received of thisMessage)
           set messageStr to messageStr & ¬
               my createLispAssoc("from", sender of thisMessage)
           set messageStr to messageStr & ¬
               my createLispAssoc("status", read status of thisMessage)
           set messageStr to messageStr & ¬
               my createLispAssoc("junk", junk mail status of thisMessage)
           set messageStr to messageStr & ¬
               my createLispAssoc("mailbox", ¬
                                  name of mailbox of thisMessage)
           set messageStr to messageStr & ¬
               my createLispAssoc("account", ¬
                                  name of account of mailbox of thisMessage)
           set messageStr to messageStr & ")"
           set messagesString to messagesString & messageStr
    end repeat
    set messagesString to messagesString & ")"
end tell
