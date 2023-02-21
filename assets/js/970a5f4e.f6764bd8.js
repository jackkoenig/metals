"use strict";(self.webpackChunk=self.webpackChunk||[]).push([[6561],{3905:(e,t,n)=>{n.d(t,{Zo:()=>u,kt:()=>h});var a=n(7294);function i(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function l(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){i(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function r(e,t){if(null==e)return{};var n,a,i=function(e,t){if(null==e)return{};var n,a,i={},o=Object.keys(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||(i[n]=e[n]);return i}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(i[n]=e[n])}return i}var s=a.createContext({}),p=function(e){var t=a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):l(l({},t),e)),n},u=function(e){var t=p(e.components);return a.createElement(s.Provider,{value:t},e.children)},d="mdxType",m={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},c=a.forwardRef((function(e,t){var n=e.components,i=e.mdxType,o=e.originalType,s=e.parentName,u=r(e,["components","mdxType","originalType","parentName"]),d=p(n),c=i,h=d["".concat(s,".").concat(c)]||d[c]||m[c]||o;return n?a.createElement(h,l(l({ref:t},u),{},{components:n})):a.createElement(h,l({ref:t},u))}));function h(e,t){var n=arguments,i=t&&t.mdxType;if("string"==typeof e||i){var o=n.length,l=new Array(o);l[0]=c;var r={};for(var s in t)hasOwnProperty.call(t,s)&&(r[s]=t[s]);r.originalType=e,r[d]="string"==typeof e?e:i,l[1]=r;for(var p=2;p<o;p++)l[p]=n[p];return a.createElement.apply(null,l)}return a.createElement.apply(null,n)}c.displayName="MDXCreateElement"},4755:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>u,contentTitle:()=>s,default:()=>h,frontMatter:()=>r,metadata:()=>p,toc:()=>d});var a=n(7462),i=n(3366),o=(n(7294),n(3905)),l=["components"],r={id:"vscode",sidebar_label:"VS Code",title:"Visual Studio Code"},s=void 0,p={unversionedId:"editors/vscode",id:"editors/vscode",title:"Visual Studio Code",description:"Goto Definition",source:"@site/target/docs/editors/vscode.md",sourceDirName:"editors",slug:"/editors/vscode",permalink:"/metals/docs/editors/vscode",draft:!1,editUrl:"https://github.com/scalameta/metals/edit/main/docs/editors/vscode.md",tags:[],version:"current",frontMatter:{id:"vscode",sidebar_label:"VS Code",title:"Visual Studio Code"},sidebar:"docs",previous:{title:"Overview",permalink:"/metals/docs/"},next:{title:"Vim",permalink:"/metals/docs/editors/vim"}},u={},d=[{value:"Requirements",id:"requirements",level:2},{value:"Installation",id:"installation",level:2},{value:"Importing a build",id:"importing-a-build",level:2},{value:"Custom sbt launcher",id:"custom-sbt-launcher",level:3},{value:"Speeding up import",id:"speeding-up-import",level:3},{value:"Importing changes",id:"importing-changes",level:3},{value:"Manually trigger build import",id:"manually-trigger-build-import",level:3},{value:"Run doctor",id:"run-doctor",level:2},{value:"Configure Java version",id:"configure-java-version",level:2},{value:"macOS",id:"macos",level:3},{value:"Custom artifact repositories (Maven or Ivy resolvers)",id:"custom-artifact-repositories-maven-or-ivy-resolvers",level:2},{value:"HTTP proxy",id:"http-proxy",level:2},{value:'Using latest Metals <a name="SNAPSHOT">SNAPSHOT</a>',id:"using-latest-metals-snapshot",level:2},{value:"Files and Directories to include in your Gitignore",id:"files-and-directories-to-include-in-your-gitignore",level:2},{value:"Show document symbols",id:"show-document-symbols",level:2},{value:"Go to parent code lenses",id:"go-to-parent-code-lenses",level:2},{value:"Create new project from template",id:"create-new-project-from-template",level:2},{value:"Running and debugging your code",id:"running-and-debugging-your-code",level:2},{value:"via code lenses",id:"via-code-lenses",level:3},{value:"via a <code>launch.json</code> configuration",id:"via-a-launchjson-configuration",level:3},{value:"via Metals&#39; commands",id:"via-metals-commands",level:3},{value:"On type formatting for multiline string formatting",id:"on-type-formatting-for-multiline-string-formatting",level:2},{value:"Formatting on paste for multiline strings",id:"formatting-on-paste-for-multiline-strings",level:2},{value:"Worksheets",id:"worksheets",level:2},{value:"Getting started with Worksheets",id:"getting-started-with-worksheets",level:3},{value:"Evaluations",id:"evaluations",level:3},{value:"Using dependencies in worksheets",id:"using-dependencies-in-worksheets",level:3},{value:"Running scalafix rules",id:"running-scalafix-rules",level:2},{value:"Searching a symbol in the workspace",id:"searching-a-symbol-in-the-workspace",level:2},{value:"Test Explorer",id:"test-explorer",level:2},{value:"Coming from IntelliJ",id:"coming-from-intellij",level:2},{value:"GitHub Codespaces and GitHub.dev support",id:"github-codespaces-and-githubdev-support",level:2}],m={toc:d},c="wrapper";function h(e){var t=e.components,n=(0,i.Z)(e,l);return(0,o.kt)(c,(0,a.Z)({},m,n,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("p",null,(0,o.kt)("img",{parentName:"p",src:"https://user-images.githubusercontent.com/1408093/48776422-1f764f00-ecd0-11e8-96d1-170f2354d50e.gif",alt:"Goto Definition"})),(0,o.kt)("h2",{id:"requirements"},"Requirements"),(0,o.kt)("p",null,(0,o.kt)("strong",{parentName:"p"},"Java 8, 11, 17 provided by OpenJDK or Oracle"),". Eclipse OpenJ9 is not\nsupported, please make sure the ",(0,o.kt)("inlineCode",{parentName:"p"},"JAVA_HOME")," environment variable\npoints to a valid Java 8, 11 or 17 installation."),(0,o.kt)("p",null,(0,o.kt)("strong",{parentName:"p"},"macOS, Linux or Windows"),". Metals is developed on many operating systems and\nevery PR is tested on Ubuntu, Windows and MacOS."),(0,o.kt)("p",null,(0,o.kt)("strong",{parentName:"p"},"Scala 2.13, 2.12, 2.11 and Scala 3"),". Metals supports these Scala versions:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("strong",{parentName:"p"},"Scala 2.13"),":\n2.13.10, 2.13.9, 2.13.8, 2.13.7, 2.13.6, 2.13.5, 2.13.4, 2.13.3")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("strong",{parentName:"p"},"Scala 2.12"),":\n2.12.17, 2.12.16, 2.12.15, 2.12.14, 2.12.13, 2.12.12, 2.12.11, 2.12.10")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("strong",{parentName:"p"},"Scala 2.11"),":\n2.11.12")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("strong",{parentName:"p"},"Scala 3"),":\n3.3.0-RC3, 3.3.0-RC2, 3.2.2, 3.2.1, 3.2.0, 3.1.3, 3.1.2, 3.1.1, 3.1.0, 3.0.2"))),(0,o.kt)("p",null,"Note that 2.11.x support is deprecated and it will be removed in future releases.\nIt's recommended to upgrade to Scala 2.12 or Scala 2.13"),(0,o.kt)("h2",{id:"installation"},"Installation"),(0,o.kt)("p",null,"Install the Metals extension from the\n",(0,o.kt)("a",{parentName:"p",href:"https://marketplace.visualstudio.com/items?itemName=scalameta.metals"},"Marketplace")," by clicking on this badge ",(0,o.kt)("a",{parentName:"p",href:"vscode:extension/scalameta.metals"},(0,o.kt)("img",{parentName:"a",src:"https://img.shields.io/badge/metals-vscode-blue.png",alt:"Install Metals extension"}))," or via the VS Code editor:"),(0,o.kt)("p",null,(0,o.kt)("img",{parentName:"p",src:"https://imgur.com/Qew0fNH.png",alt:"install stable version"})),(0,o.kt)("blockquote",null,(0,o.kt)("p",{parentName:"blockquote"},"Make sure to disable the extensions\n",(0,o.kt)("a",{parentName:"p",href:"https://marketplace.visualstudio.com/items?itemName=dragos.scala-lsp"},"Scala Language Server"),"\nand\n",(0,o.kt)("a",{parentName:"p",href:"https://marketplace.visualstudio.com/items?itemName=lightbend.vscode-sbt-scala"},"Scala (sbt)"),"\nif they are installed. The\n",(0,o.kt)("a",{parentName:"p",href:"https://marketplace.visualstudio.com/items?itemName=lampepfl.dotty"},"Dotty Language Server"),"\ndoes ",(0,o.kt)("strong",{parentName:"p"},"not")," need to be disabled because the Metals and Dotty extensions don't\nconflict with each other. However, if you want to work on Scala 3 code in a\nworkspace that was previously opened with ",(0,o.kt)("inlineCode",{parentName:"p"},"Dotty Language Server")," you need to\nfirst remove ",(0,o.kt)("inlineCode",{parentName:"p"},".dotty-ide-artifact")," before opening the workspace with Metals.")),(0,o.kt)("p",null,"Next, open a directory containing your Scala code. The extension activates when\nthe main directory contains ",(0,o.kt)("inlineCode",{parentName:"p"},"build.sbt")," or ",(0,o.kt)("inlineCode",{parentName:"p"},"build.sc")," file, a Scala file is\nopened, which includes ",(0,o.kt)("inlineCode",{parentName:"p"},"*.sbt"),", ",(0,o.kt)("inlineCode",{parentName:"p"},"*.scala")," and ",(0,o.kt)("inlineCode",{parentName:"p"},"*.sc")," file, or a standard Scala\ndirectory structure ",(0,o.kt)("inlineCode",{parentName:"p"},"src/main/scala")," is detected."),(0,o.kt)("p",null,"It is also possible to opt in to install the pre-release version and try out the latest cutting edge features from Metals server.\nApart from new features, pre-release versions also include many bugfixes. It's encouraged to use them with ",(0,o.kt)("a",{parentName:"p",href:"#SNAPSHOT"},"SNAPSHOT")," releases of Metals server. Using pre-release versions may result in less stable experience and it is not indented for beginners.\nPre-release versions follow ",(0,o.kt)("inlineCode",{parentName:"p"},"major.minor.PATCH")," versioning."),(0,o.kt)("p",null,(0,o.kt)("img",{parentName:"p",src:"https://imgur.com/CzOTleE.png",alt:"Install the pre-release extension"})),(0,o.kt)("h2",{id:"importing-a-build"},"Importing a build"),(0,o.kt)("p",null,'The first time you open Metals in a new workspace it prompts you to import the build.\nClick "Import build" to start the installation step.'),(0,o.kt)("p",null,(0,o.kt)("img",{parentName:"p",src:"https://i.imgur.com/0VqZWay.png",alt:"Import build"})),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},'"Not now" disables this prompt for 2 minutes.'),(0,o.kt)("li",{parentName:"ul"},'"Don\'t show again" disables this prompt forever, use ',(0,o.kt)("inlineCode",{parentName:"li"},"rm -rf .metals/")," to re-enable\nthe prompt."),(0,o.kt)("li",{parentName:"ul"},"Use ",(0,o.kt)("inlineCode",{parentName:"li"},"tail -f .metals/metals.log")," to watch the build import progress."),(0,o.kt)("li",{parentName:"ul"},"Behind the scenes, Metals uses ",(0,o.kt)("a",{parentName:"li",href:"https://scalacenter.github.io/bloop/"},"Bloop")," to\nimport sbt builds, but you don't need Bloop installed on your machine to run this step.")),(0,o.kt)("p",null,"Once the import step completes, compilation starts for your open ",(0,o.kt)("inlineCode",{parentName:"p"},"*.scala"),"\nfiles."),(0,o.kt)("p",null,"Once the sources have compiled successfully, you can navigate the codebase with\ngoto definition."),(0,o.kt)("h3",{id:"custom-sbt-launcher"},"Custom sbt launcher"),(0,o.kt)("p",null,"By default, Metals runs an embedded ",(0,o.kt)("inlineCode",{parentName:"p"},"sbt-launch.jar")," launcher that respects ",(0,o.kt)("inlineCode",{parentName:"p"},".sbtopts")," and ",(0,o.kt)("inlineCode",{parentName:"p"},".jvmopts"),".\nHowever, the environment variables ",(0,o.kt)("inlineCode",{parentName:"p"},"SBT_OPTS")," and ",(0,o.kt)("inlineCode",{parentName:"p"},"JAVA_OPTS")," are not respected."),(0,o.kt)("p",null,'Update the "Sbt Script" setting to use a custom ',(0,o.kt)("inlineCode",{parentName:"p"},"sbt")," script instead of the\ndefault Metals launcher if you need further customizations like reading environment\nvariables."),(0,o.kt)("p",null,(0,o.kt)("img",{parentName:"p",src:"https://i.imgur.com/NuwEBe4.png",alt:"Sbt Launcher"})),(0,o.kt)("h3",{id:"speeding-up-import"},"Speeding up import"),(0,o.kt)("p",null,'The "Import build" step can take a long time, especially the first time you\nrun it in a new build. The exact time depends on the complexity of the build and\nif library dependencies need to be downloaded. For example, this step can take\neverything from 10 seconds in small cached builds up to 10-15 minutes in large\nuncached builds.'),(0,o.kt)("p",null,"Consult the ",(0,o.kt)("a",{parentName:"p",href:"https://scalacenter.github.io/bloop/docs/build-tools/sbt#speeding-up-build-export"},"Bloop documentation"),"\nto learn how to speed up build import."),(0,o.kt)("h3",{id:"importing-changes"},"Importing changes"),(0,o.kt)("p",null,"When you change ",(0,o.kt)("inlineCode",{parentName:"p"},"build.sbt")," or sources under ",(0,o.kt)("inlineCode",{parentName:"p"},"project/"),", you will be prompted to\nre-import the build."),(0,o.kt)("p",null,(0,o.kt)("img",{parentName:"p",src:"https://i.imgur.com/72kdZkL.png",alt:"Import sbt changes"})),(0,o.kt)("h3",{id:"manually-trigger-build-import"},"Manually trigger build import"),(0,o.kt)("p",null,'To manually trigger a build import, execute the "Import build" command through\nthe command palette (',(0,o.kt)("inlineCode",{parentName:"p"},"Cmd + Shift + P"),")."),(0,o.kt)("p",null,(0,o.kt)("img",{parentName:"p",src:"https://i.imgur.com/QHLKt8u.png",alt:"Import build command"})),(0,o.kt)("h2",{id:"run-doctor"},"Run doctor"),(0,o.kt)("p",null,'Execute the "Run Doctor" through the command palette to troubleshoot potential\nconfiguration problems in your workspace.'),(0,o.kt)("p",null,(0,o.kt)("img",{parentName:"p",src:"https://i.imgur.com/K02g0UM.png",alt:"Run doctor command"})),(0,o.kt)("h2",{id:"configure-java-version"},"Configure Java version"),(0,o.kt)("p",null,"The VS Code plugin uses by default the ",(0,o.kt)("inlineCode",{parentName:"p"},"JAVA_HOME")," environment variable (via\n",(0,o.kt)("a",{parentName:"p",href:"https://www.npmjs.com/package/locate-java-home"},(0,o.kt)("inlineCode",{parentName:"a"},"locate-java-home")),") to locate\nthe ",(0,o.kt)("inlineCode",{parentName:"p"},"java"),' executable. To override the default Java home location, update the\n"Java Home" variable in the settings menu.'),(0,o.kt)("p",null,(0,o.kt)("img",{parentName:"p",src:"https://i.imgur.com/sKrPKk2.png",alt:"Java Home setting"})),(0,o.kt)("p",null,"If this setting is defined, the VS Code plugin uses the custom path instead of\nthe ",(0,o.kt)("inlineCode",{parentName:"p"},"JAVA_HOME")," environment variable."),(0,o.kt)("h3",{id:"macos"},"macOS"),(0,o.kt)("p",null,"To globally configure ",(0,o.kt)("inlineCode",{parentName:"p"},"$JAVA_HOME")," for all GUI applications, see\n",(0,o.kt)("a",{parentName:"p",href:"https://stackoverflow.com/questions/135688/setting-environment-variables-on-os-x"},"this Stackoverflow answer"),"."),(0,o.kt)("p",null,"If you prefer to manually configure Java home through VS Code, run the following\ncommand to copy the Java 8 home path."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sh"},"/usr/libexec/java_home -v 1.8 | pbcopy\n")),(0,o.kt)("h2",{id:"custom-artifact-repositories-maven-or-ivy-resolvers"},"Custom artifact repositories (Maven or Ivy resolvers)"),(0,o.kt)("p",null,"Use the 'Custom Repositories' setting for the Metals VS Code extension to tell\n",(0,o.kt)("a",{parentName:"p",href:"https://get-coursier.io/docs/other-proxy"},"Coursier")," to try to download Metals\nartifacts from your private artifact repository."),(0,o.kt)("p",null,"Use ",(0,o.kt)("inlineCode",{parentName:"p"},".jvmopts")," to set sbt options\n(",(0,o.kt)("a",{parentName:"p",href:"https://www.scala-sbt.org/1.0/docs/Proxy-Repositories.html"},"https://www.scala-sbt.org/1.0/docs/Proxy-Repositories.html"),") for\n",(0,o.kt)("inlineCode",{parentName:"p"},"sbt bloopInstall")," which resolves library dependencies. You can also provide a\ncustom sbt script (see 'Custom sbt launcher')."),(0,o.kt)("h2",{id:"http-proxy"},"HTTP proxy"),(0,o.kt)("p",null,"Metals uses ",(0,o.kt)("a",{parentName:"p",href:"https://get-coursier.io/docs/other-proxy"},"Coursier")," to download\nartifacts from Maven Central. To use Metals behind an HTTP proxy, configure the\nsystem properties ",(0,o.kt)("inlineCode",{parentName:"p"},"-Dhttps.proxyHost=\u2026 -Dhttps.proxyPort=\u2026")," in one of the\nfollowing locations:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("inlineCode",{parentName:"li"},".jvmopts")," file in the workspace directory."),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("inlineCode",{parentName:"li"},"JAVA_OPTS")," environment variable, make sure to start ",(0,o.kt)("inlineCode",{parentName:"li"},"code")," from your terminal\nwhen using this option since environment variables don't always propagate\ncorrectly when opening VS Code as a GUI application outside a terminal."),(0,o.kt)("li",{parentName:"ul"},'"Server Properties" setting for the Metals VS Code extension, which can be\nconfigured per-workspace or per-user.')),(0,o.kt)("h2",{id:"using-latest-metals-snapshot"},"Using latest Metals ",(0,o.kt)("a",{name:"SNAPSHOT"},"SNAPSHOT")),(0,o.kt)("p",null,'Update the "Server Version" setting to try out the latest pending Metals\nfeatures.'),(0,o.kt)("table",null,(0,o.kt)("thead",null,(0,o.kt)("tr",null,(0,o.kt)("th",null,"Version"),(0,o.kt)("th",null,"Published"))),(0,o.kt)("tbody",null,(0,o.kt)("tr",null,(0,o.kt)("td",null,"0.11.10"),(0,o.kt)("td",null,"02 Jan 2023 14:53")),(0,o.kt)("tr",null,(0,o.kt)("td",null,"0.11.10+165-d4feefc2-SNAPSHOT"),(0,o.kt)("td",null,"21 Feb 2023 06:34")))),'Run the "Reload Window" command after updating the setting for the new version to take effect.',(0,o.kt)("h2",{id:"files-and-directories-to-include-in-your-gitignore"},"Files and Directories to include in your Gitignore"),(0,o.kt)("p",null,"The Metals server places logs and other files in the ",(0,o.kt)("inlineCode",{parentName:"p"},".metals")," directory. The\nBloop compile server places logs and compilation artifacts in the ",(0,o.kt)("inlineCode",{parentName:"p"},".bloop"),"\ndirectory. The Bloop plugin that generates Bloop configuration is added in the\n",(0,o.kt)("inlineCode",{parentName:"p"},"metals.sbt")," file, which is added at ",(0,o.kt)("inlineCode",{parentName:"p"},"project/metals.sbt")," as well as further\n",(0,o.kt)("inlineCode",{parentName:"p"},"project")," directories depending on how deep ",(0,o.kt)("inlineCode",{parentName:"p"},"*.sbt")," files need to be supported.\nTo support each ",(0,o.kt)("inlineCode",{parentName:"p"},"*.sbt")," file Metals needs to create an additional file at\n",(0,o.kt)("inlineCode",{parentName:"p"},"./project/project/metals.sbt")," relative to the sbt file.\nWorking with Ammonite scripts will place compiled scripts into the ",(0,o.kt)("inlineCode",{parentName:"p"},".ammonite")," directory.\nIt's recommended to exclude these directories and files\nfrom version control systems like git."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sh"},"# ~/.gitignore\n.metals/\n.bloop/\n.ammonite/\nmetals.sbt\n")),(0,o.kt)("h2",{id:"show-document-symbols"},"Show document symbols"),(0,o.kt)("p",null,'Run the "Explorer: Focus on Outline View" command to open the symbol outline for\nthe current file in the sidebar.'),(0,o.kt)("p",null,(0,o.kt)("img",{parentName:"p",src:"https://i.imgur.com/T0kVJsr.gif",alt:"Document Symbols Outline"})),(0,o.kt)("p",null,'Run the "Open Symbol in File" command to search for a symbol in the current file\nwithout opening the sidebar.'),(0,o.kt)("p",null,(0,o.kt)("img",{parentName:"p",src:"https://i.imgur.com/0PJ4brd.png",alt:"Document Symbols Command"})),(0,o.kt)("p",null,"As you type, the symbol outline is also visible at the top of the file.\n",(0,o.kt)("img",{parentName:"p",src:"https://i.imgur.com/L217n4q.png",alt:"Document Symbols Outline"})),(0,o.kt)("h2",{id:"go-to-parent-code-lenses"},"Go to parent code lenses"),(0,o.kt)("p",null,"Metals has the ability to display code lenses that, when invoked,\nwill go to the parent class that contains the definition of the method or symbol.\nUnfortunately, it might cause some lag in larger code bases,\nwhich is why it is not enabled currently by default."),(0,o.kt)("p",null,"To enable the feature you need to modify the setting ",(0,o.kt)("inlineCode",{parentName:"p"},"metals.superMethodLensesEnabled")," to ",(0,o.kt)("inlineCode",{parentName:"p"},"true"),"."),(0,o.kt)("p",null,"Even without using the code lenses it's still possible to navigate the method hierarchy\nusing two commands:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("inlineCode",{parentName:"p"},"Metals: Go to super method")," - immediately goes to the parent of the method the cursor is pointing to")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("inlineCode",{parentName:"p"},"Metals: Reveal super method hierachy")," - displays the full method hierachy and enables to move to any parent,\nit is best used with the Metals Quick Pick extension."))),(0,o.kt)("p",null,"You can also bind those commands to a shortcut."),(0,o.kt)("h2",{id:"create-new-project-from-template"},"Create new project from template"),(0,o.kt)("p",null,"It is possible using Metals to easily setup a new project using the exiting ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/foundweekends/giter8/wiki/giter8-templates"},"giter8")," templates.\nThis is an equivalent to the ",(0,o.kt)("inlineCode",{parentName:"p"},"sbt new")," command, which uses the same mechanism.\nThere is a great number of templates already available and it should be easy to find something for yourself.\nTo start the setup you can use the Metals: New Scala project command, which works as following:"),(0,o.kt)("ol",null,(0,o.kt)("li",{parentName:"ol"},(0,o.kt)("p",{parentName:"li"},"Choose the template and then:"),(0,o.kt)("ol",{parentName:"li"},(0,o.kt)("li",{parentName:"ol"},"Use the proposed templates."),(0,o.kt)("li",{parentName:"ol"},'Choose "Discover more" and then choose from the list downloaded from the Giter8 wiki page.'),(0,o.kt)("li",{parentName:"ol"},"Input a custom Github repository following the ",(0,o.kt)("inlineCode",{parentName:"li"},"organization/repo")," schema."))),(0,o.kt)("li",{parentName:"ol"},(0,o.kt)("p",{parentName:"li"},"Navigate to the parent directory that you want to create your new project in.")),(0,o.kt)("li",{parentName:"ol"},(0,o.kt)("p",{parentName:"li"},"Choose the name or accept the default one.")),(0,o.kt)("li",{parentName:"ol"},(0,o.kt)("p",{parentName:"li"},"Choose whether to open a new window for the created project or use the existing one."))),(0,o.kt)("p",null,'The same command will be invoked when clicking the "New Scala Project" button in the Metals view.'),(0,o.kt)("p",null,"If you feel like a template should be included in the default displayed ones do not hesitate to create a\n",(0,o.kt)("a",{parentName:"p",href:"https://github.com/scalameta/metals/blob/cda5b8c2029e5f201fb8d0636e0365d796407bd9/metals/src/main/scala/scala/meta/internal/builds/NewProjectProvider.scala#L308"},"PR"),"\nor file an issue."),(0,o.kt)("h2",{id:"running-and-debugging-your-code"},"Running and debugging your code"),(0,o.kt)("p",null,"Metals supports running and debugging tests and main methods via the\n",(0,o.kt)("a",{parentName:"p",href:"https://microsoft.github.io/debug-adapter-protocol/"},"Debug Adapter Protocol"),".\nThe protocol is used to communicate between the editor and debugger, which means\nthat applications can be run the same as for any other language in the natively\nsupported ",(0,o.kt)("inlineCode",{parentName:"p"},"Run")," view. When using Metals the debugger itself is\n",(0,o.kt)("a",{parentName:"p",href:"https://scalacenter.github.io/bloop/"},"Bloop"),", which is also responsible for\nstarting the actual process."),(0,o.kt)("p",null,"Users can begin the debugging session in two ways:"),(0,o.kt)("h3",{id:"via-code-lenses"},"via code lenses"),(0,o.kt)("p",null,(0,o.kt)("img",{parentName:"p",src:"https://i.imgur.com/5nTnrcS.png",alt:"lenses"})),(0,o.kt)("p",null,"For each main or test class Metals shows two code lenses ",(0,o.kt)("inlineCode",{parentName:"p"},"run | debug")," or\n",(0,o.kt)("inlineCode",{parentName:"p"},"test | test debug"),", which show up above the definition as a kind of virtual\ntext. Clicking ",(0,o.kt)("inlineCode",{parentName:"p"},"run")," or ",(0,o.kt)("inlineCode",{parentName:"p"},"test")," will start running the main class or test without\nstopping at any breakpoints, while clicking ",(0,o.kt)("inlineCode",{parentName:"p"},"debug")," or ",(0,o.kt)("inlineCode",{parentName:"p"},"test debug")," will pause\nonce any of them are hit. It's not possible to add any arguments or java\nproperties when running using this method."),(0,o.kt)("h3",{id:"via-a-launchjson-configuration"},"via a ",(0,o.kt)("inlineCode",{parentName:"h3"},"launch.json")," configuration"),(0,o.kt)("p",null,"Visual Studio Code uses ",(0,o.kt)("inlineCode",{parentName:"p"},".vscode/launch.json")," to store user defined\nconfigurations, which can be run using:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"The ",(0,o.kt)("inlineCode",{parentName:"li"},"Run -> Start Debugging")," menu item or ",(0,o.kt)("inlineCode",{parentName:"li"},"workbench.action.debug.start"),"\nshortcut."),(0,o.kt)("li",{parentName:"ul"},"The ",(0,o.kt)("inlineCode",{parentName:"li"},"Run -> Run Without Debugging")," menu item or ",(0,o.kt)("inlineCode",{parentName:"li"},"workbench.action.debug.run"),"\nshortcut.")),(0,o.kt)("p",null,"If a user doesn't have anything yet saved, a configuration wizard will pop up to\nguide them. In the end users should end up with something like this:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-json"},'{\n  "version": "0.2.0",\n  "configurations": [\n    // Main class configuration\n    {\n      "type": "scala",\n      "request": "launch",\n      // configuration name visible for the user\n      "name": "Launch Main",\n      // full name of the class to run\n      "mainClass": "com.example.Main",\n      // optional arguments for the main class\n      "args": [],\n      // optional jvm properties to use\n      "jvmOptions": []\n    },\n    // Test class configuration\n    {\n      "type": "scala",\n      "request": "launch",\n      // configuration name visible for the user\n      "name": "Launch Test",\n      // full name of the class to run\n      "testClass": "com.example.Test"\n    },\n    // Attach debugger when running via:\n    // `-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=localhost:5005`\n    {\n      "type": "scala",\n      "request": "attach",\n      "name": "Attach debugger",\n      // name of the module that is being debugging\n      "buildTarget": "root",\n      // Host of the jvm to connect to\n      "hostName": "localhost",\n      // Port to connect to\n      "port": 5005\n    }\n  ]\n}\n')),(0,o.kt)("p",null,"You can also add an optional build target name, which is needed in case there\nare more than one class with the same name or when launching a class from\noutside the project. Inside ",(0,o.kt)("inlineCode",{parentName:"p"},'"configurations":')," add the key ",(0,o.kt)("inlineCode",{parentName:"p"},"buildTarget")," with\nyour target name, e.g. ",(0,o.kt)("inlineCode",{parentName:"p"},"root"),":"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-json"},'      "buildTarget": "root"\n')),(0,o.kt)("p",null,"The build target name corresponds to your project name. For example in sbt for\n",(0,o.kt)("inlineCode",{parentName:"p"},"lazy val interfaces = project")," the name of the build target will be\n",(0,o.kt)("inlineCode",{parentName:"p"},"interfaces")," for sources and ",(0,o.kt)("inlineCode",{parentName:"p"},"interfaces-test")," for tests. To make sure you have\nthe correct target names please run the command ",(0,o.kt)("inlineCode",{parentName:"p"},"Metals: Run Doctor"),"."),(0,o.kt)("p",null,"Multiple configurations can be stored in that file and can be chosen either\nmanually in the ",(0,o.kt)("inlineCode",{parentName:"p"},"Run")," view or can be picked by invoking a shortcut defined under\n",(0,o.kt)("inlineCode",{parentName:"p"},"workbench.action.debug.selectandstart"),"."),(0,o.kt)("h3",{id:"via-metals-commands"},"via Metals' commands"),(0,o.kt)("p",null,"You can also use commands that can be easily bound to shortcuts:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("inlineCode",{parentName:"li"},"metals.run-current-file")," - Run main class in the current file."),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("inlineCode",{parentName:"li"},"metals.test-current-file")," - Run test class in the current file"),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("inlineCode",{parentName:"li"},"metals.test-current-target")," - Run all tests in the current project.")),(0,o.kt)("p",null,"To assign shortcuts just go to the Keyboard Shortcuts page (",(0,o.kt)("inlineCode",{parentName:"p"},"File")," ->\n",(0,o.kt)("inlineCode",{parentName:"p"},"Preferences")," -> ",(0,o.kt)("inlineCode",{parentName:"p"},"Keyboard Shortcuts"),") and search for a command, click on it and\nuse your preferred shortcut."),(0,o.kt)("h2",{id:"on-type-formatting-for-multiline-string-formatting"},"On type formatting for multiline string formatting"),(0,o.kt)("p",null,(0,o.kt)("img",{parentName:"p",src:"https://imgur.com/a0O2vCs.gif",alt:"on-type"})),(0,o.kt)("p",null,"To properly support adding ",(0,o.kt)("inlineCode",{parentName:"p"},"|")," in multiline strings we are using the\n",(0,o.kt)("inlineCode",{parentName:"p"},"onTypeFormatting")," method. The functionality is enabled by default, but you can\ndisable/enable ",(0,o.kt)("inlineCode",{parentName:"p"},"onTypeFormatting")," inside Visual Studio Code settings by checking\n",(0,o.kt)("inlineCode",{parentName:"p"},"Editor: Format On Type"),":"),(0,o.kt)("p",null,(0,o.kt)("img",{parentName:"p",src:"https://i.imgur.com/s6nT9rC.png",alt:"on-type-setting"})),(0,o.kt)("h2",{id:"formatting-on-paste-for-multiline-strings"},"Formatting on paste for multiline strings"),(0,o.kt)("p",null,"Whenever text is paste into a multiline string with ",(0,o.kt)("inlineCode",{parentName:"p"},"|")," it will be properly\nformatted by Metals:"),(0,o.kt)("p",null,(0,o.kt)("img",{parentName:"p",src:"https://i.imgur.com/fF0XWYC.gif",alt:"format-on-paste"})),(0,o.kt)("p",null,"This feature is enabled by default. If you need to disable/enable formatting on\npaste in Visual Studio Code you can check the ",(0,o.kt)("inlineCode",{parentName:"p"},"Editor: Format On Paste")," setting:"),(0,o.kt)("p",null,(0,o.kt)("img",{parentName:"p",src:"https://i.imgur.com/rMrk27F.png",alt:"format-on-paste-setting"})),(0,o.kt)("h2",{id:"worksheets"},"Worksheets"),(0,o.kt)("p",null,"Worksheets are a great way to explore an api, try out an idea, or code\nup an example and quickly see the evaluated expression or result. Behind\nthe scenes worksheets are powered by the great work done in\n",(0,o.kt)("a",{parentName:"p",href:"https://scalameta.org/mdoc/"},"mdoc"),"."),(0,o.kt)("h3",{id:"getting-started-with-worksheets"},"Getting started with Worksheets"),(0,o.kt)("p",null,"To get started with a worksheet you can either use the ",(0,o.kt)("inlineCode",{parentName:"p"},"metals.new-scala-file"),"\ncommand and select ",(0,o.kt)("em",{parentName:"p"},"Worksheet")," or create a file called ",(0,o.kt)("inlineCode",{parentName:"p"},"*.worksheet.sc"),".\nThis format is important since this is what tells Metals that it's meant to be\ntreated as a worksheet and not just a Scala script. Where you create the\nscript also matters. If you'd like to use classes and values from your\nproject, you need to make sure the worksheet is created inside of your ",(0,o.kt)("inlineCode",{parentName:"p"},"src"),"\ndirectory. You can still create a worksheet in other places, but you will\nonly have access to the standard library and your dependencies."),(0,o.kt)("h3",{id:"evaluations"},"Evaluations"),(0,o.kt)("p",null,"After saving you'll see the result of the expression as a decoration at the end of the line.\nYou may not see the full result for example if it's too long, so you are also\nable to hover on the decoration to expand the decoration."),(0,o.kt)("p",null,"Keep in mind that you don't need to wrap your code in an ",(0,o.kt)("inlineCode",{parentName:"p"},"object"),". In worksheets\neverything can be evaluated at the top level."),(0,o.kt)("h3",{id:"using-dependencies-in-worksheets"},"Using dependencies in worksheets"),(0,o.kt)("p",null,"You are able to include an external dependency in your worksheet by including\nit in one of the following two ways."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},"// $dep.`organisation`::artifact:version` style\nimport $dep.`com.lihaoyi::scalatags:0.7.0`\n\n// $ivy.`organisation::artifact:version` style\nimport $ivy.`com.lihaoyi::scalatags:0.7.0`\n")),(0,o.kt)("p",null,(0,o.kt)("inlineCode",{parentName:"p"},"::")," is the same as ",(0,o.kt)("inlineCode",{parentName:"p"},"%%")," in sbt, which will append the current Scala binary version\nto the artifact name."),(0,o.kt)("p",null,"You can also import ",(0,o.kt)("inlineCode",{parentName:"p"},"scalac")," options in a special ",(0,o.kt)("inlineCode",{parentName:"p"},"$scalac")," import like below:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},"import $scalac.`-Ywarn-unused`\n")),(0,o.kt)("h2",{id:"running-scalafix-rules"},"Running scalafix rules"),(0,o.kt)("p",null,"Scalafix allows users to specify some refactoring and linting rules that can be applied to your\ncodebase. Please checkout the ",(0,o.kt)("a",{parentName:"p",href:"https://scalacenter.github.io/scalafix"},"scalafix website")," for more information."),(0,o.kt)("p",null,"Since Metals v0.11.7 it's now possible to run scalafix rules using a special\ncommand ",(0,o.kt)("inlineCode",{parentName:"p"},"metals.scalafix-run"),". In VS Code can be also run using the default shortcut of ",(0,o.kt)("inlineCode",{parentName:"p"},"shift + alt + ctrl + o"),".\nThis should run all the rules defined in your ",(0,o.kt)("inlineCode",{parentName:"p"},".scalafix.conf")," file. All built-in rules\nand the ",(0,o.kt)("a",{parentName:"p",href:"https://scalacenter.github.io/scalafix/docs/rules/community-rules.html#hygiene-rules"},"community hygiene ones")," can\nbe run without any additional settings. However, for all the other rules users need to\nadd an additional dependency in the ",(0,o.kt)("inlineCode",{parentName:"p"},"metals.scalafixRulesDependencies")," user setting.\nThose rules need to be in form of strings such as ",(0,o.kt)("inlineCode",{parentName:"p"},"com.github.liancheng::organize-imports:0.6.0"),", which\nfollows the same convention as ",(0,o.kt)("a",{parentName:"p",href:"https://get-coursier.io/"},"coursier dependencies"),"."),(0,o.kt)("p",null,"A sample scalafix configuration can be seen below:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-hocon"},'rules = [\n  OrganizeImports,\n  ExplicitResultTypes,\n  RemoveUnused\n]\n\nRemoveUnused.imports = false\n\nOrganizeImports.groupedImports = Explode\nOrganizeImports.expandRelative = true\nOrganizeImports.removeUnused = true\nOrganizeImports.groups = [\n  "re:javax?\\."\n  "scala."\n  "scala.meta."\n  "*"\n]\n\n')),(0,o.kt)("h2",{id:"searching-a-symbol-in-the-workspace"},"Searching a symbol in the workspace"),(0,o.kt)("p",null,'Metals provides an alternative command to the native "Go to symbol in workspace..." command, in order to work around some VS Code limitations (see ',(0,o.kt)("a",{parentName:"p",href:"https://github.com/microsoft/vscode/issues/98125"},"this issue")," for more context) and provide richer search capabilities."),(0,o.kt)("p",null,'You can invoke this command from the command palette (look for "Metals: Search symbol in workspace").\nOptionally you can also bind this command to a shortcut. For example, if you want to replace the native command with the Metals one you can configure this shortcut:'),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-js"},'  {\n    "key": "ctrl+t", // or "cmd+t" if you\'re on macOS\n    "command": "metals.symbol-search",\n    "when": "editorLangId == scala"\n  }\n')),(0,o.kt)("h2",{id:"test-explorer"},"Test Explorer"),(0,o.kt)("p",null,"Metals 0.11.0 implements Visual Studio Code's ",(0,o.kt)("a",{parentName:"p",href:"https://code.visualstudio.com/api/extension-guides/testing"},"Testing API"),".  "),(0,o.kt)("p",null,"Test Explorer UI is a new default way to run/debug test suites and replaces Code\nLenses. The new UI adds a testing view, which shows all test suites declared in\nproject's modules. From this panel it's possible to"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"view all discovered test suites grouped by build targets (modules) and filter them"),(0,o.kt)("li",{parentName:"ul"},"run/debug test"),(0,o.kt)("li",{parentName:"ul"},"navigate to test's definition.")),(0,o.kt)("p",null,(0,o.kt)("img",{parentName:"p",src:"https://i.imgur.com/Z3VtS0O.gif",alt:"test-explorer"})),(0,o.kt)("p",null,"NOTE: While Metals detects test suites for most of existing testing\nframeworks, support for recognizing individual tests is more limited.\nMetals supports the current set of test frameworks when it comes to\nindividual test discovery:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"Junit"),(0,o.kt)("li",{parentName:"ul"},"MUnit"),(0,o.kt)("li",{parentName:"ul"},"ScalatestIf you encounter an error, create an ",(0,o.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/issues"},"issue"),".")),(0,o.kt)("h2",{id:"coming-from-intellij"},"Coming from IntelliJ"),(0,o.kt)("p",null,"Install the\n",(0,o.kt)("a",{parentName:"p",href:"https://marketplace.visualstudio.com/items?itemName=k--kato.intellij-idea-keybindings"},"IntelliJ IDEA Keybindings"),"\nextension to use default IntelliJ shortcuts with VS Code."),(0,o.kt)("table",null,(0,o.kt)("thead",{parentName:"table"},(0,o.kt)("tr",{parentName:"thead"},(0,o.kt)("th",{parentName:"tr",align:null},"IntelliJ"),(0,o.kt)("th",{parentName:"tr",align:null},"VS Code"))),(0,o.kt)("tbody",{parentName:"table"},(0,o.kt)("tr",{parentName:"tbody"},(0,o.kt)("td",{parentName:"tr",align:null},"Go to class"),(0,o.kt)("td",{parentName:"tr",align:null},"Go to symbol in workspace")),(0,o.kt)("tr",{parentName:"tbody"},(0,o.kt)("td",{parentName:"tr",align:null},"Parameter info"),(0,o.kt)("td",{parentName:"tr",align:null},"Trigger parameter hints")),(0,o.kt)("tr",{parentName:"tbody"},(0,o.kt)("td",{parentName:"tr",align:null},"Basic completion"),(0,o.kt)("td",{parentName:"tr",align:null},"Trigger suggest")),(0,o.kt)("tr",{parentName:"tbody"},(0,o.kt)("td",{parentName:"tr",align:null},"Type info"),(0,o.kt)("td",{parentName:"tr",align:null},"Show hover")),(0,o.kt)("tr",{parentName:"tbody"},(0,o.kt)("td",{parentName:"tr",align:null},"Expand"),(0,o.kt)("td",{parentName:"tr",align:null},"Fold")),(0,o.kt)("tr",{parentName:"tbody"},(0,o.kt)("td",{parentName:"tr",align:null},"Extend Selection"),(0,o.kt)("td",{parentName:"tr",align:null},"Expand selection")))),(0,o.kt)("h2",{id:"github-codespaces-and-githubdev-support"},"GitHub Codespaces and GitHub.dev support"),(0,o.kt)("p",null,"See ",(0,o.kt)("a",{parentName:"p",href:"https://scalameta.org/metals/docs/editors/online-ides#github-codespaces-and-githubdev"},"https://scalameta.org/metals/docs/editors/online-ides#github-codespaces-and-githubdev")))}h.isMDXComponent=!0}}]);