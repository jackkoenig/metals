"use strict";(self.webpackChunk=self.webpackChunk||[]).push([[989],{3905:(e,t,a)=>{a.d(t,{Zo:()=>c,kt:()=>h});var n=a(7294);function i(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function o(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function l(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?o(Object(a),!0).forEach((function(t){i(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):o(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function s(e,t){if(null==e)return{};var a,n,i=function(e,t){if(null==e)return{};var a,n,i={},o=Object.keys(e);for(n=0;n<o.length;n++)a=o[n],t.indexOf(a)>=0||(i[a]=e[a]);return i}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(n=0;n<o.length;n++)a=o[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(i[a]=e[a])}return i}var r=n.createContext({}),p=function(e){var t=n.useContext(r),a=t;return e&&(a="function"==typeof e?e(t):l(l({},t),e)),a},c=function(e){var t=p(e.components);return n.createElement(r.Provider,{value:t},e.children)},m="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},d=n.forwardRef((function(e,t){var a=e.components,i=e.mdxType,o=e.originalType,r=e.parentName,c=s(e,["components","mdxType","originalType","parentName"]),m=p(a),d=i,h=m["".concat(r,".").concat(d)]||m[d]||u[d]||o;return a?n.createElement(h,l(l({ref:t},c),{},{components:a})):n.createElement(h,l({ref:t},c))}));function h(e,t){var a=arguments,i=t&&t.mdxType;if("string"==typeof e||i){var o=a.length,l=new Array(o);l[0]=d;var s={};for(var r in t)hasOwnProperty.call(t,r)&&(s[r]=t[r]);s.originalType=e,s[m]="string"==typeof e?e:i,l[1]=s;for(var p=2;p<o;p++)l[p]=a[p];return n.createElement.apply(null,l)}return n.createElement.apply(null,a)}d.displayName="MDXCreateElement"},2459:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>c,contentTitle:()=>r,default:()=>h,frontMatter:()=>s,metadata:()=>p,toc:()=>m});var n=a(7462),i=a(3366),o=(a(7294),a(3905)),l=["components"],s={author:"\xd3lafur P\xe1ll Geirsson",title:"Fast goto definition with low memory footprint",authorURL:"https://twitter.com/olafurpg",authorImageURL:"https://avatars2.githubusercontent.com/u/1408093?s=460&v=4"},r=void 0,p={permalink:"/metals/blog/2018/12/12/fast-goto-definition",source:"@site/blog/2018-12-12-fast-goto-definition.md",title:"Fast goto definition with low memory footprint",description:"Metals throws away its navigation index when it shuts down. Next time it starts,",date:"2018-12-12T00:00:00.000Z",formattedDate:"December 12, 2018",tags:[],readingTime:8.42,hasTruncateMarker:!0,authors:[{name:"\xd3lafur P\xe1ll Geirsson",url:"https://twitter.com/olafurpg",imageURL:"https://avatars2.githubusercontent.com/u/1408093?s=460&v=4"}],frontMatter:{author:"\xd3lafur P\xe1ll Geirsson",title:"Fast goto definition with low memory footprint",authorURL:"https://twitter.com/olafurpg",authorImageURL:"https://avatars2.githubusercontent.com/u/1408093?s=460&v=4"},prevItem:{title:"Metals v0.3.2 - Iron",permalink:"/metals/blog/2018/12/14/iron"},nextItem:{title:"Metals v0.3 - Iron",permalink:"/metals/blog/2018/12/06/iron"}},c={authorsImageUrls:[void 0]},m=[{value:"Problem statement",id:"problem-statement",level:2},{value:"Java",id:"java",level:3},{value:"Scala",id:"scala",level:3},{value:"Initial solution",id:"initial-solution",level:2},{value:"Optimized solution",id:"optimized-solution",level:2},{value:"Evaluation",id:"evaluation",level:2},{value:"Conclusion",id:"conclusion",level:2}],u={toc:m},d="wrapper";function h(e){var t=e.components,a=(0,i.Z)(e,l);return(0,o.kt)(d,(0,n.Z)({},u,a,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("p",null,"Metals throws away its navigation index when it shuts down. Next time it starts,\nthe index is computed again from scratch. Although this approach is simple, it\nrequires indexing to be fast enough so you don't mind running it again and\nagain. Also, because we don't persist the index to disk, we need to be careful\nwith memory usage."),(0,o.kt)("p",null,"This post covers how Metals achieves fast source indexing for Scala with a small\nmemory footprint. We describe the problem statement, explain the initial\nsolution and how an optimization delivered a 10x speedup. Finally, we evaluate\nthe result on a real-world project."),(0,o.kt)("p",null,"The work presented in this post was done as part of my job at the\n",(0,o.kt)("a",{parentName:"p",href:"https://scala.epfl.ch/"},"Scala Center"),"."),(0,o.kt)("h2",{id:"problem-statement"},"Problem statement"),(0,o.kt)("p",null,"What happens when you run Goto Definition? In reality, a lot goes on but in this\npost we're gonna focus on a specific problem: given a method like\n",(0,o.kt)("inlineCode",{parentName:"p"},"scala.Some.isEmpty")," and many thousand source files with millions of lines of\ncode, how do we quickly find the source file that defines that method?"),(0,o.kt)("p",null,(0,o.kt)("img",{parentName:"p",src:"https://user-images.githubusercontent.com/1408093/49591684-67070700-f96f-11e8-873d-90c40480528b.gif",alt:"goto-definition"})),(0,o.kt)("p",null,"There are some hard constraints:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"we must answer quickly, normal requests should respond within 100-200ms."),(0,o.kt)("li",{parentName:"ul"},"memory usage should not exceed 10-100Mb since we also need memory to implement\nother features and we're sharing the computer with other applications."),(0,o.kt)("li",{parentName:"ul"},"computing an index should not take more than ~10 seconds after importing the\nbuild, even for large projects with millions of lines of source code\n(including dependencies).")),(0,o.kt)("p",null,"To keep things simple, imagine we have all source files available in a directory\nthat we can walk and read."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'walk()\n// Stream("scala/Option.scala", "scala/Predef.scala", ...)\n\nread("scala/Option.scala")\n// "sealed abstract class Option { ... }; class Some extends Option { ... }"\n')),(0,o.kt)("h3",{id:"java"},"Java"),(0,o.kt)("p",null,"For Java, the challenge is not so difficult since the compiler enforces that\neach source file contains a single public class with matching filename."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-java"},"// java/nio/file/Files.java\npackage java.nio.file;\npublic class Files {\n    public static byte[] readAllBytes(path: Path) {\n      // ...\n    }\n}\n")),(0,o.kt)("p",null,"To respond to a Goto Definition request for the ",(0,o.kt)("inlineCode",{parentName:"p"},"Files.readAllBytes")," method, we"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"take the enclosing toplevel class ",(0,o.kt)("inlineCode",{parentName:"li"},"java.nio.file.Files")),(0,o.kt)("li",{parentName:"ul"},"read the corresponding file ",(0,o.kt)("inlineCode",{parentName:"li"},"java/nio/File/Files.java")),(0,o.kt)("li",{parentName:"ul"},"parse ",(0,o.kt)("inlineCode",{parentName:"li"},"Files.java")," to find the exact position of ",(0,o.kt)("inlineCode",{parentName:"li"},"readAllBytes"))),(0,o.kt)("p",null,"This approach is fast (parsing one file is cheap) and it also requires no index\n(0Mb memory!)."),(0,o.kt)("h3",{id:"scala"},"Scala"),(0,o.kt)("p",null,"For Scala, the problem is trickier because the compiler allows multiple toplevel\nclasses in the same source file."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},"// scala/Option.scala\npackage scala\nsealed abstract class Option[+T] { /* ... */ }\nfinal class Some[+T](value: T) extends Option[T] {\n  def isEmpty = false\n  // ...\n}\n")),(0,o.kt)("p",null,"To navigate to the ",(0,o.kt)("inlineCode",{parentName:"p"},"scala.Some.isEmpty")," method we can't use the same approach as\nin Java because the class ",(0,o.kt)("inlineCode",{parentName:"p"},"scala.Some")," is in ",(0,o.kt)("inlineCode",{parentName:"p"},"scala/Option.scala"),", not\n",(0,o.kt)("inlineCode",{parentName:"p"},"scala/Some.scala"),"."),(0,o.kt)("p",null,"One possible solution is to read the ",(0,o.kt)("inlineCode",{parentName:"p"},"scala/Some.class")," classfile that contains\nthe filename ",(0,o.kt)("inlineCode",{parentName:"p"},"Option.scala")," where ",(0,o.kt)("inlineCode",{parentName:"p"},"scala.Some")," is defined."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sh"},'$ javap -classpath $(coursier fetch org.scala-lang:scala-library:2.12.8) scala.Some\nCompiled from "Option.scala"\n...\n')),(0,o.kt)("p",null,"However, source information in classfiles is not always reliable and it may be\nremoved by tools that process jar files. Let's restrict the problem to only use\nsource files."),(0,o.kt)("p",null,"Instead of using JVM classfiles, we can walk all Scala source files and build an\nindex that maps toplevel classes to the source file that defines that class. The\nindex looks something like this:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'val index = Map[Symbol, Path](\n  Symbol("scala.Some") -> Path("scala/Option.scala"),\n  // ...\n)\n')),(0,o.kt)("p",null,"With this index, we find the definition of ",(0,o.kt)("inlineCode",{parentName:"p"},"Some.isEmpty")," using the same steps\nas for ",(0,o.kt)("inlineCode",{parentName:"p"},"Files.readAllBytes")," in Java:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"take the enclosing toplevel class ",(0,o.kt)("inlineCode",{parentName:"li"},"scala.Some")),(0,o.kt)("li",{parentName:"ul"},"query index to know that ",(0,o.kt)("inlineCode",{parentName:"li"},"scala.Some")," is defined in ",(0,o.kt)("inlineCode",{parentName:"li"},"scala/Option.scala")),(0,o.kt)("li",{parentName:"ul"},"parse ",(0,o.kt)("inlineCode",{parentName:"li"},"scala/Option.scala")," to find exact position of ",(0,o.kt)("inlineCode",{parentName:"li"},"isEmpty")," method.")),(0,o.kt)("p",null,"The challenge is to efficiently build the index."),(0,o.kt)("h2",{id:"initial-solution"},"Initial solution"),(0,o.kt)("p",null,"One approach to build the index is to use the\n",(0,o.kt)("a",{parentName:"p",href:"https://scalameta.org/"},"Scalameta")," parser to extract the toplevel classes of\neach source file. This parser does not desugar the original code making it\nuseful for refactoring and code-formatting tools like\n",(0,o.kt)("a",{parentName:"p",href:"http://scalacenter.github.io/scalafix/"},"Scalafix"),"/",(0,o.kt)("a",{parentName:"p",href:"http://scalameta.org/scalafmt"},"Scalafmt"),".\nI'm also familiar with Scalameta parser API so it was fast to get a working\nimplementation. However, is the parser fast enough to parse millions of lines of\ncode on every server startup?"),(0,o.kt)("p",null,'According to JMH benchmarks, the Scalameta parser handles ~92k lines/second\nmeasured against a sizable corpus of Scala code. The benchmarks use the\n"single-shot" mode of JMH, for which the documentation says:'),(0,o.kt)("blockquote",null,(0,o.kt)("p",{parentName:"blockquote"},'"This mode is useful to estimate the "cold" performance when you don\'t want to\nhide the warmup invocations."')),(0,o.kt)("p",null,"Cold performance is an OK estimate for our use-case since indexing happens\nduring server startup."),(0,o.kt)("p",null,"The Scala standard library is ~36k lines so at 92k lines/second this solution\nscales up to a codebase with up to 20-30 similarly-sized library dependencies.\nIf we add more library dependencies, we exceed the 10 second constraint for\nindexing time. For a codebase with 5 million lines of code, users might have to\nwait one minute for indexing to complete. We should aim for better."),(0,o.kt)("h2",{id:"optimized-solution"},"Optimized solution"),(0,o.kt)("p",null,"We can speed up indexing by writing a custom parser that extracts only the\ninformation we need from a source file. For example, the Scalameta parser\nextracts method implementations that are irrelevant for our indexer. Our indexer\nneeds to know the toplevel classes and nothing more."),(0,o.kt)("p",null,"The simplified algorithm for this custom parser goes something like this:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"tokenize source file"),(0,o.kt)("li",{parentName:"ul"},"on consecutive ",(0,o.kt)("inlineCode",{parentName:"li"},"package object")," keywords, record package object"),(0,o.kt)("li",{parentName:"ul"},"on ",(0,o.kt)("inlineCode",{parentName:"li"},"package")," keyword, record package name"),(0,o.kt)("li",{parentName:"ul"},"on ",(0,o.kt)("inlineCode",{parentName:"li"},"class")," and ",(0,o.kt)("inlineCode",{parentName:"li"},"trait")," and ",(0,o.kt)("inlineCode",{parentName:"li"},"object")," keywords, record toplevel class"),(0,o.kt)("li",{parentName:"ul"},"on ",(0,o.kt)("inlineCode",{parentName:"li"},"(")," and ",(0,o.kt)("inlineCode",{parentName:"li"},"[")," and ",(0,o.kt)("inlineCode",{parentName:"li"},"{")," delimiters, skip tokens until we find matching closing\ndelimiter")),(0,o.kt)("p",null,"There are a few more cases to handle, but the implementation ended up being ~200\nlines of code that took an afternoon to write and test (less time than it took\nto write this blog post!)."),(0,o.kt)("p",null,"Benchmarks show that the custom parser is almost 10x faster compared to the\ninitial solution."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-text"},"[info] Benchmark                  Mode  Cnt  Score   Error  Units\n[info] MetalsBench.toplevelParse    ss   30  0.349 \xb1 0.003   s/op\n[info] MetalsBench.scalametaParse   ss   30  3.370 \xb1 0.175   s/op\n")),(0,o.kt)("p",null,"At ~920k lines/second it takes ~6 seconds to index a codebase with 5 million\nlines of code, a noticeable improvement to the user experience compared to the\none minute it took with the previous solution."),(0,o.kt)("h2",{id:"evaluation"},"Evaluation"),(0,o.kt)("p",null,"Micro-benchmarks are helpful but they don't always reflect user experience. To\nevaluate how our indexer performs in the real world, we test Metals on the\n",(0,o.kt)("a",{parentName:"p",href:"https://www.prisma.io/"},"Prisma")," codebase. Prisma is a server\xa0implemented in\nScala that replaces traditional ORMs and data access layers with a universal\ndatabase abstraction."),(0,o.kt)("p",null,(0,o.kt)("img",{parentName:"p",src:"https://user-images.githubusercontent.com/1408093/49875321-08cf9d80-fe21-11e8-9f02-54ff4960a7af.png",alt:"prisma"})),(0,o.kt)("p",null,"The project has around 80k lines of Scala code."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sh"},"$ git clone https://github.com/prisma/prisma.git\n$ cd prisma/server\n$ loc\nLanguage             Files        Lines        Blank      Comment         Code\nScala                  673        88730        12811         1812        74107\n")),(0,o.kt)("p",null,"We open VS Code with the Metals extension, enable the server property\n",(0,o.kt)("inlineCode",{parentName:"p"},"-Dmetals.statistics=all")," and import the build."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sh"},"$ tail -f .metals/metals.log\ntime: ran 'sbt bloopInstall' in 1m4s\ntime: imported workspace in 13s\nmemory: index using 13.1M (923,085 lines Scala)\n")),(0,o.kt)("p",null,'We run "Import build" again to get a second measurement.'),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sh"},"time: ran 'sbt bloopInstall' in 41s\ntime: imported workspace in 7s\nmemory: index using 13.1M (990,144 Scala)\n")),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("strong",{parentName:"li"},"\"ran 'sbt bloopInstall'\""),": the time it takes to dump the sbt build\nstructure with ",(0,o.kt)("a",{parentName:"li",href:"https://scalacenter.github.io/bloop/"},"Bloop"),", the build server\nused by Metals.",(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"The Prisma build has 44 sbt projects."),(0,o.kt)("li",{parentName:"ul"},"Running ",(0,o.kt)("inlineCode",{parentName:"li"},"sbt")," takes 23 seconds to reach the sbt shell."))),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("strong",{parentName:"li"},'"imported workspace"'),": the time it takes to import the build structure into\nMetals and index sources of all projects, including the following steps:",(0,o.kt)("ol",{parentName:"li"},(0,o.kt)("li",{parentName:"ol"},"Query Bloop for the dumped build structure, listing all project\ndependencies, sources and compiler options."),(0,o.kt)("li",{parentName:"ol"},"Walk, read and index all Scala sources in the workspace, happens in both\nruns."),(0,o.kt)("li",{parentName:"ol"},"Walk, read and index all library dependency sources, happens only for the\nfirst run since the indexer caches\xa0",(0,o.kt)("inlineCode",{parentName:"li"},"*-sources.jar")," file results.")),(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},'It\'s expected that "lines of code" increased by ~70k lines in the second run\nsince we indexed the workspace sources again.'),(0,o.kt)("li",{parentName:"ul"},"It's expected that indexing was faster in the second run since indexes are\ncached for ",(0,o.kt)("inlineCode",{parentName:"li"},"*-sources.jar")," files and the JVM has also warmed up."))),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("strong",{parentName:"li"},'"index using 13.1M"'),": the memory footprint of the index mapping toplevel\nScala classes to which file they are defined in.",(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"Memory is measured using\n",(0,o.kt)("a",{parentName:"li",href:"https://openjdk.java.net/projects/code-tools/jol/"},"OpenJDK JOL"),(0,o.kt)("inlineCode",{parentName:"li"},"GraphLayout.totalSize()"),"."),(0,o.kt)("li",{parentName:"ul"},"Note that the server uses more memory in total, this is only for the\nnavigation index."),(0,o.kt)("li",{parentName:"ul"},"The index does not contain entries where the toplevel classname matches the\nfilename, for the same reason we don't index Java sources.")))),(0,o.kt)("p",null,"On average, Goto Definition responds well within our 100ms goal."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-text"},"time: definition in 0.03s\ntime: definition in 0.02s\n")),(0,o.kt)("p",null,"However, occasional definition requests take much longer to respond."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-text"},"info  time: definition in 8s\ninfo  time: definition in 0.62s\n")),(0,o.kt)("p",null,"The outliers happen when navigating inside library dependency sources, which is\nexpected to be slower since we need to type-check those sources before querying\nthe index."),(0,o.kt)("h2",{id:"conclusion"},"Conclusion"),(0,o.kt)("p",null,"Metals uses a custom Scala parser to index sources at roughly 1 million\nlines/second in micro-benchmarks. The index is used to quickly respond to Goto\nDefinition requests. On a case-study project containing 44 modules and ~900k\nlines of Scala sources (including library dependencies), it takes ~10 seconds to\nimport the build structure (including source indexing) and the resulting index\nuses 13M memory."),(0,o.kt)("p",null,"The Java+Scala source indexers used by Metals are available in a standalone\nlibrary called mtags."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'libraryDependencies += "org.scalameta" %% "mtags" % "0.3.1"\n')),(0,o.kt)("p",null,"The mtags library is already being used by the\n",(0,o.kt)("a",{parentName:"p",href:"http://almond-sh.github.io/almond/stable/docs/intro"},"Almond Scala kernel for Jupyter"),"\nto support Goto Definition inside Jupyter notebooks."),(0,o.kt)("p",null,"Can the indexer become faster? Sure, I suspect there's still room for 2-3x\nspeedups with further optimizations. However, I'm not convinced it will make a\nsignificant improvement to the user experience since we remain bottle-necked by\ndumping sbt build structure and compiling the sources."),(0,o.kt)("p",null,"Try out Goto Definition with Metals today using VS Code, Atom, Vim, Sublime Text\nor Emacs using the installation instructions here:\n",(0,o.kt)("a",{parentName:"p",href:"https://scalameta.org/metals/docs/editors/overview.html"},"https://scalameta.org/metals/docs/editors/overview.html"),'. The indexer is working\nwhen you see "Importing build" in the status bar\n',(0,o.kt)("img",{parentName:"p",src:"https://user-images.githubusercontent.com/1408093/49924982-f5234600-feb7-11e8-9edd-715388bb546f.gif",alt:"imageedit_3_3576623823"})))}h.isMDXComponent=!0}}]);