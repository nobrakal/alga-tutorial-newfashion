#!/bin/sh

pandoc -s -t html5 tutorial.tex > tutorial.html
sed -i '/<style.*>/,/<\/style>/d' tutorial.html
sed -i 's|</head>|</head><style>\n</style>|' tutorial.html
sed -i -e '/<style>/r main.css' tutorial.html
sed -i 's|</body>|<script src="https://cdn.rawgit.com/google/code-prettify/master/loader/run_prettify.js?lang=hs&amp;skin=desert"></script>\n</body>|' tutorial.html
sed -i 's|<pre class="sourceCode haskell">|<pre class="sourceCode haskell prettyprint">|' tutorial.html

LST="figspng/connect.png figspng/overlay.png"
for f in $LST
do
  sed -i "s|<img src=\"$f\"|<img src=\"$f\" class=\"big\"|" tutorial.html
done
