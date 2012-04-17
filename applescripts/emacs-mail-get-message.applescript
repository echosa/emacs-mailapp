-- require "emacs-mail-util.applescript"

tell application "Mail"
     get item 1 of TARGETBOX
     set messageList to (messages of result whose id is MAILID)
     set theMessage to item 1 of messageList
     set read status of theMessage to true
     set messageStr to "(("
     set messageStr to messageStr & ¬
         my createLispAssoc("subject", subject of theMessage)
     set messageStr to messageStr & ¬
         my createLispAssoc("from", sender of theMessage)
     set messageStr to messageStr & ¬
         my createLispAssoc("date", date received of theMessage)
     set messageStr to messageStr & ¬
         my createLispAssoc("mailbox", ¬
                            name of mailbox of theMessage)
     set messageStr to messageStr & ¬
         my createLispAssoc("account", ¬
                            name of account of mailbox of theMessage)
     set messageStr to messageStr & ¬
         my createLispAssoc("content", source of theMessage)
     set messageStr to messageStr & "))"
end tell

