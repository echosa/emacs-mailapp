on strReplace(thisText, searchString, replacementString)
   set AppleScript's text item delimiters to the searchString
   set the itemList to every text item of thisText
   set AppleScript's text item delimiters to the replacementString
   set thisText to the itemList as string
   set AppleScript's text item delimiters to ""
   return thisText
end strReplace

on createLispAssoc(car, cdr)
   set dclass to class of cdr
	
   if dclass is equal to text then
      set cdr to my strReplace(cdr, "\\", "\\\\")
      set cdr to "\"" & my strReplace(cdr, "\"", "\\\"") & "\""
   else if dclass is equal to boolean then
        if cdr then
           set cdr to "t"
        else
           set cdr to "nil"
        end if
   else if dclass is equal to date then
        set cdr to "\"" & cdr & "\""
   end if
	
   return "(" & car & " . " & cdr & ")"
end createLispAssoc
