---
layout: post
title: My example Literate Haskell post
issueId: 4
---

\begin{comment}
 https://www.uv.es/wikibase/doc/cas/pandoc_manual_instalado.wiki?60
 https://wiki.haskell.org/index.php?title=Literate_programming
 https://pandoc.org/MANUAL.html
 https://hackage.haskell.org/package/literatex
 https://www.extrema.is/blog/2023/03/21/literate-haskell-markdown-headings
 https://stackoverflow.com/questions/3473389/lhs-and-markdown-codeblocks

\end{comment}


Header 1
===

Header 2
---

<h3 id="header-3">Header 3</h3>


  > block quote

<a href="http://www.google.com">Link to google</a>

-- this comment doesn't work

% this comment doesn't work

{- this comment doesn't work -}

<!-- this comment doesn't work -->

 <!-- this comment works, because it has a space at the beginning of the line -->

<!--
  This comment works, because the tag is closed in a different line, AND is printed to markdown.
  We can use --strip-comments to prevent that
-->

\begin{comment}This comment works\end{comment}


\begin{comment}Hiding code from markdown\end{comment}
\long\def\ignore#1{}

\begin{comment}This is hidden from markdown\end{comment}
\begin{code}%
module LiterateHaskellExample  where
\end{code}

\begin{code}
x :: Integer
x = 234
\end{code}

\begin{comment}This is hidden from haskell\end{comment}
```hs
data FailingHaskell = Code
```

\begin{comment}This is hidden from haskell and markdown\end{comment}
\ignore{
\begin{code}
data Some = Code
\end{code}
}
