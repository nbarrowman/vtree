#' @importFrom utf8 as_utf8

convertToHTML <- function(x,just="c") {
  # Convert various text elements to their HTML entities.
  # Note that order matters here!
  
  x <- utf8::as_utf8(x)

  x <- gsub("&","&amp;",x)
  
  x <- gsub("'","&rsquo;",x)
  
  # x <- gsub("<=","&le;",x)
  x <- gsub("<=","&lt;=",x)
  x <- gsub(">=","&gt;=",x)
  # x <- gsub(">=","&ge;",x)
  x <- gsub("<","&lt;",x)
  x <- gsub(">","&gt;",x)

  # Also convert character sequences for line breaks.

  x <- gsub("\n\\*l","<BR ALIGN='LEFT'/>",x)

  if (just %in% c("c","C","center","Center","centre","Centre")) {
    x <- gsub("\\\\n","<BR/>",x)
    x <- gsub("\n","<BR/>",x)
  } else
  if (just %in% c("l","L","left","Left")) {
    x <- gsub("\\\\n","<BR ALIGN='LEFT'/>",x)
    x <- gsub("\n","<BR ALIGN='LEFT'/>",x)    
  } else
  if (just %in% c("r","R","right","Right")) {
    x <- gsub("\\\\n","<BR ALIGN='RIGHT'/>",x)
    x <- gsub("\n","<BR ALIGN='RIGHT'/>",x)    
  } else
  stop("Unknown argument of just")  
    

  # Markdown-style formatting

  x <- gsub("\\*\\*(.+?)\\*\\*","<B>\\1</B>",x)
  x <- gsub("\\*(.+?)\\*","<I>\\1</I>",x)

  # In markdown, _underscores_ can be used to format in italics.
  # But I have disabled this because it caused problems with
  # variable_names_likeThis
  # x <- gsub("_(.+?)_","<I>\\1</I>",x)

  # Special character sequence for color!

  x <- gsub("%%([^ ]+?) (.+?)%%","<FONT COLOR=\"\\1\">\\2</FONT>",x)

  # Markdown-style formatting for superscript and subscript

  x <- gsub("\\^(.+?)\\^","<FONT POINT-SIZE='10'><SUP>\\1</SUP></FONT>",x)
  x <- gsub("~(.+?)~","<FONT POINT-SIZE='10'><SUB>\\1</SUB></FONT>",x)

  x
}
