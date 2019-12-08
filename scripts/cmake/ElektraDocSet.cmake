set (INFO_PATH "${DOCSET_PATH}/Contents/Info.plist")

file (COPY ${ICON_PATH} DESTINATION ${DOCSET_PATH})
file (READ ${INFO_PATH} INFO)
string (REGEX REPLACE "(<key>DocSetPlatformFamily</key>\n[ \t]*<string>)[^<]+" "\\1elektra" INFO_KEYWORD_FIXED ${INFO})
file (WRITE ${INFO_PATH} ${INFO_KEYWORD_FIXED})
