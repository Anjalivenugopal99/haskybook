## Design Details

Markdown files follow a specific syntax that is easy to read and just as easy to write.They are plain text files so they can be created using any text editor on any computer.To host webpages, we need the notes in html, which is more cumbersome to write.

** STEP 1 **

First step in building this application is to parse markdown to HTML format. The conversion would be done using a parsec library in Haskell which uses monadic parser combinators. Parsec is defined as a monad transformer that can be stacked on arbitrary monads, and it is also parametric in the input stream type. 

** STEP 2 **

Our next step will be implementing the restructuring of HTML files to enable the comprehensive documentation including content sidebar. This would allow a static website with connected pages. Along with the contents of each file we will give a  * Table of contents *  which will automatically generate the contents and each page corresponding to the title in there. 






