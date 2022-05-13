"use strict";(self.webpackChunk=self.webpackChunk||[]).push([[1753],{3905:(e,t,a)=>{a.d(t,{Zo:()=>m,kt:()=>h});var l=a(7294);function n(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function r(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);t&&(l=l.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,l)}return a}function o(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?r(Object(a),!0).forEach((function(t){n(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):r(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function i(e,t){if(null==e)return{};var a,l,n=function(e,t){if(null==e)return{};var a,l,n={},r=Object.keys(e);for(l=0;l<r.length;l++)a=r[l],t.indexOf(a)>=0||(n[a]=e[a]);return n}(e,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(l=0;l<r.length;l++)a=r[l],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(n[a]=e[a])}return n}var s=l.createContext({}),p=function(e){var t=l.useContext(s),a=t;return e&&(a="function"==typeof e?e(t):o(o({},t),e)),a},m=function(e){var t=p(e.components);return l.createElement(s.Provider,{value:t},e.children)},u={inlineCode:"code",wrapper:function(e){var t=e.children;return l.createElement(l.Fragment,{},t)}},c=l.forwardRef((function(e,t){var a=e.components,n=e.mdxType,r=e.originalType,s=e.parentName,m=i(e,["components","mdxType","originalType","parentName"]),c=p(a),h=n,d=c["".concat(s,".").concat(h)]||c[h]||u[h]||r;return a?l.createElement(d,o(o({ref:t},m),{},{components:a})):l.createElement(d,o({ref:t},m))}));function h(e,t){var a=arguments,n=t&&t.mdxType;if("string"==typeof e||n){var r=a.length,o=new Array(r);o[0]=c;var i={};for(var s in t)hasOwnProperty.call(t,s)&&(i[s]=t[s]);i.originalType=e,i.mdxType="string"==typeof e?e:n,o[1]=i;for(var p=2;p<r;p++)o[p]=a[p];return l.createElement.apply(null,o)}return l.createElement.apply(null,a)}c.displayName="MDXCreateElement"},6263:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>m,contentTitle:()=>s,default:()=>h,frontMatter:()=>i,metadata:()=>p,toc:()=>u});var l=a(7462),n=a(3366),r=(a(7294),a(3905)),o=["components"],i={author:"Tomasz Godzik",title:"Metals v0.6.1 - Radium",authorURL:"https://twitter.com/tomekgodzik",authorImageURL:"https://avatars1.githubusercontent.com/u/3807253?s=460&v=4"},s=void 0,p={permalink:"/metals/blog/2019/06/11/radium",source:"@site/blog/2019-06-11-radium.md",title:"Metals v0.6.1 - Radium",description:'We are excited to announce the release of Metals v0.6.1, codename "Radium" \ud83c\udf89',date:"2019-06-11T00:00:00.000Z",formattedDate:"June 11, 2019",tags:[],readingTime:3.915,truncated:!1,authors:[{name:"Tomasz Godzik",url:"https://twitter.com/tomekgodzik",imageURL:"https://avatars1.githubusercontent.com/u/3807253?s=460&v=4"}],frontMatter:{author:"Tomasz Godzik",title:"Metals v0.6.1 - Radium",authorURL:"https://twitter.com/tomekgodzik",authorImageURL:"https://avatars1.githubusercontent.com/u/3807253?s=460&v=4"},prevItem:{title:"Metals v0.7.0 - Thorium",permalink:"/metals/blog/2019/06/28/thorium"},nextItem:{title:"Metals v0.5.1 - Mercury",permalink:"/metals/blog/2019/04/26/mercury"}},m={authorsImageUrls:[void 0]},u=[{value:"TL;DR",id:"tldr",children:[],level:2},{value:"Bloop upgrade",id:"bloop-upgrade",children:[],level:2},{value:"Automatic &quot;import build&quot; for Gradle, Maven and Mill",id:"automatic-import-build-for-gradle-maven-and-mill",children:[],level:2},{value:"More reliable shutdown",id:"more-reliable-shutdown",children:[],level:2},{value:"Completions freeze less",id:"completions-freeze-less",children:[],level:2},{value:"Keyword completions",id:"keyword-completions",children:[],level:2},{value:"VS Code doesn&#39;t compile projects until it is focused",id:"vs-code-doesnt-compile-projects-until-it-is-focused",children:[],level:2},{value:"Metals is now a default server in the Sublime LSP package",id:"metals-is-now-a-default-server-in-the-sublime-lsp-package",children:[],level:2},{value:"Contributors",id:"contributors",children:[],level:2},{value:"Merged PRs",id:"merged-prs",children:[],level:2}],c={toc:u};function h(e){var t=e.components,a=(0,n.Z)(e,o);return(0,r.kt)("wrapper",(0,l.Z)({},c,a,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("p",null,'We are excited to announce the release of Metals v0.6.1, codename "Radium" \ud83c\udf89\nThe release mostly focused on adding support for the build tools Gradle, Maven\nand Mill.'),(0,r.kt)("p",null,"Metals is a language server for Scala that works with VS Code, Atom, Vim,\nSublime Text and Emacs. Metals is developed at the\n",(0,r.kt)("a",{parentName:"p",href:"https://scala.epfl.ch/"},"Scala Center")," along with contributors from the\ncommunity."),(0,r.kt)("p",null,"In this release we merged 24 PRs and closed 6 issues, full details:\n",(0,r.kt)("a",{parentName:"p",href:"https://github.com/scalameta/metals/milestone/7?closed=1"},"https://github.com/scalameta/metals/milestone/7?closed=1"),"."),(0,r.kt)("h2",{id:"tldr"},"TL;DR"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},'automatic "import build" for Gradle, Maven and Mill'),(0,r.kt)("li",{parentName:"ul"},"upgraded to Bloop v1.3.2"),(0,r.kt)("li",{parentName:"ul"},"better handling of requests that access the presentation compiler"),(0,r.kt)("li",{parentName:"ul"},"code completions on keywords"),(0,r.kt)("li",{parentName:"ul"},"VS Code doesn't compile projects until it is focused"),(0,r.kt)("li",{parentName:"ul"},"Metals is now a default server in the Sublime LSP package")),(0,r.kt)("p",null,"Check out the website and give Metals a try: ",(0,r.kt)("a",{parentName:"p",href:"https://scalameta.org/metals/"},"https://scalameta.org/metals/")),(0,r.kt)("h2",{id:"bloop-upgrade"},"Bloop upgrade"),(0,r.kt)("p",null,"Metals now depends on Bloop v1.3.2 instead of v1.2.5. The v1.3 release of Bloop\nhas a ton of nice improvements that benefit Metals users. For full details, see\nthe\n",(0,r.kt)("a",{parentName:"p",href:"https://github.com/scalacenter/bloop/releases/tag/v1.3.0"},"Bloop v1.3.0 release notes"),"."),(0,r.kt)("h2",{id:"automatic-import-build-for-gradle-maven-and-mill"},'Automatic "import build" for Gradle, Maven and Mill'),(0,r.kt)("p",null,"Now, it's possible to use Metals with the build tools Gradle, Maven and Mill!\nImporting a build with Gradle, Maven and Mill now works the same way it works\nfor sbt."),(0,r.kt)("p",null,"For more details, see the corresponding documentation for each build tool:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"http://scalameta.org/metals/docs/build-tools/overview.html"},"Overview")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"http://scalameta.org/metals/docs/build-tools/gradle.html"},"Gradle")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"http://scalameta.org/metals/docs/build-tools/maven.html"},"Maven")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"http://scalameta.org/metals/docs/build-tools/mill.html"},"Mill"))),(0,r.kt)("h2",{id:"more-reliable-shutdown"},"More reliable shutdown"),(0,r.kt)("p",null,"An issue where the Metals process could continue running even after closing the\neditor has now been fixed."),(0,r.kt)("h2",{id:"completions-freeze-less"},"Completions freeze less"),(0,r.kt)("p",null,"An issue where Metals could get stuck in an infinite loop using 100% CPU while\nresponding to completions has now been fixed."),(0,r.kt)("h2",{id:"keyword-completions"},"Keyword completions"),(0,r.kt)("p",null,"Previously, Metals did not complete keywords like ",(0,r.kt)("inlineCode",{parentName:"p"},"import"),", ",(0,r.kt)("inlineCode",{parentName:"p"},"new"),", ",(0,r.kt)("inlineCode",{parentName:"p"},"lazy val")," or\n",(0,r.kt)("inlineCode",{parentName:"p"},"trait"),". Language keywords are now included in the auto-completions in most\ncases."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"object Main {\n    // Before\n    impo<COMPLETE>\n    // After\n    import\n}\n")),(0,r.kt)("p",null,"Keywords are suggested based on the context (e.g. you won't see throw suggested\nif the cursor is not inside a declaration)"),(0,r.kt)("p",null,"The only keywords that are not completed are: ",(0,r.kt)("inlineCode",{parentName:"p"},"extends"),", ",(0,r.kt)("inlineCode",{parentName:"p"},"finally"),", ",(0,r.kt)("inlineCode",{parentName:"p"},"with"),",\n",(0,r.kt)("inlineCode",{parentName:"p"},"forSome"),", ",(0,r.kt)("inlineCode",{parentName:"p"},"catch")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"finally"),"."),(0,r.kt)("h2",{id:"vs-code-doesnt-compile-projects-until-it-is-focused"},"VS Code doesn't compile projects until it is focused"),(0,r.kt)("p",null,"Previously, Metals would trigger compilation in the background even if the VS\nCode window was not focused. For example, switching git branches in a separate\nterminal window would still trigger compilation in Metals. Now, Metals waits\nuntil the VS Code window is focused to trigger compilation."),(0,r.kt)("p",null,"This feature is implemented as a LSP extension and is currently only supported\nby VS Code. For details on how to implement this extension for another editor,\nsee the\n",(0,r.kt)("a",{parentName:"p",href:"http://scalameta.org/metals/docs/integrations/new-editor#metals-windowstatedidchange"},"documentation on integrating a new text editor"),"."),(0,r.kt)("h2",{id:"metals-is-now-a-default-server-in-the-sublime-lsp-package"},"Metals is now a default server in the Sublime LSP package"),(0,r.kt)("p",null,"Metals is now a default server for Scala source files in the LSP package, see\n",(0,r.kt)("a",{parentName:"p",href:"https://github.com/tomv564/LSP/pull/571"},"tomv564/LSP#571"),". This greatly\nsimplifies the installation steps for Sublime Text users."),(0,r.kt)("h2",{id:"contributors"},"Contributors"),(0,r.kt)("p",null,"Big thanks to everybody who contributed to this release!"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"$ git shortlog -sn --no-merges v0.5.1..v0.6.1\n\xd3lafur P\xe1ll Geirsson\nAdam Gajek\nTomasz Godzik\nGabriele Petronella\nAyoub Benali\nCody Allen\nEvgeny Kurnevsky\nJeffrey Lau\nMarek \u017barnowski\nMarkus Hauck\nGerman Greiner\n")),(0,r.kt)("h2",{id:"merged-prs"},"Merged PRs"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Avoid inifinite loop when shortening types\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/751"},"#","751"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/olafurpg"},"olafurpg"),")"),(0,r.kt)("li",{parentName:"ul"},"Move releasing info to the website\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/748"},"#","748"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/gabro"},"gabro"),")"),(0,r.kt)("li",{parentName:"ul"},"Update bloop to 1.3.2 ",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/747"},"#","747"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/tgodzik"},"tgodzik"),")"),(0,r.kt)("li",{parentName:"ul"},"Split build tool and executable name to show properly in output\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/746"},"#","746"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/tgodzik"},"tgodzik"),")"),(0,r.kt)("li",{parentName:"ul"},"Update bloop to 1.3.0 and BSP to 2.0.0-M4\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/745"},"#","745"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/tgodzik"},"tgodzik"),")"),(0,r.kt)("li",{parentName:"ul"},"Add better hint in the doctor for Maven workspaces\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/744"},"#","744"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/tgodzik"},"tgodzik"),")"),(0,r.kt)("li",{parentName:"ul"},"Update emacs docs as scala is supported by lsp-mode directly now\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/741"},"#","741"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/kurnevsky"},"kurnevsky"),")"),(0,r.kt)("li",{parentName:"ul"},"Remove Scalafix from pre-push git hook.\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/740"},"#","740"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/olafurpg"},"olafurpg"),")"),(0,r.kt)("li",{parentName:"ul"},"Setup git hooks for scalafmt and scalafix.\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/738"},"#","738"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/olafurpg"},"olafurpg"),")"),(0,r.kt)("li",{parentName:"ul"},"Add automatic Mill import to metals\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/737"},"#","737"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/tgodzik"},"tgodzik"),")"),(0,r.kt)("li",{parentName:"ul"},"Improve handling of requests that access the presentation compiler.\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/736"},"#","736"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/olafurpg"},"olafurpg"),")"),(0,r.kt)("li",{parentName:"ul"},"Enforce scala version for embedded bloop server\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/735"},"#","735"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/tgodzik"},"tgodzik"),")"),(0,r.kt)("li",{parentName:"ul"},"Ensure build server shuts down before starting new build connection.\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/731"},"#","731"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/olafurpg"},"olafurpg"),")"),(0,r.kt)("li",{parentName:"ul"},"Fix blogpost typo ",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/730"},"#","730"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/zoonfafer"},"zoonfafer"),")"),(0,r.kt)("li",{parentName:"ul"},"Miscellaneous polish fixes\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/729"},"#","729"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/olafurpg"},"olafurpg"),")"),(0,r.kt)("li",{parentName:"ul"},"Implement textDocument/codeLens to run main function\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/728"},"#","728"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/marek1840"},"marek1840"),")"),(0,r.kt)("li",{parentName:"ul"},"Fix newlines in process output.\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/727"},"#","727"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/tgodzik"},"tgodzik"),")"),(0,r.kt)("li",{parentName:"ul"},"Update sublime doc ",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/726"},"#","726"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/ayoub-benali"},"ayoub-benali"),")"),(0,r.kt)("li",{parentName:"ul"},"Fix transitive scala library dependency in Gradle builds.\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/725"},"#","725"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/tgodzik"},"tgodzik"),")"),(0,r.kt)("li",{parentName:"ul"},"Adds maven integration to metals\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/722"},"#","722"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/tgodzik"},"tgodzik"),")"),(0,r.kt)("li",{parentName:"ul"},"Use CocRequestAsync in Vim docs\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/717"},"#","717"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/ceedubs"},"ceedubs"),")"),(0,r.kt)("li",{parentName:"ul"},"Set timeout for shutdown procedure, fixes ","#","715.\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/716"},"#","716"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/olafurpg"},"olafurpg"),")"),(0,r.kt)("li",{parentName:"ul"},"Add keyword completion ",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/712"},"#","712"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/markus1189"},"markus1189"),")"),(0,r.kt)("li",{parentName:"ul"},"Make bloop versions customizable via server properties.\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/710"},"#","710"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/olafurpg"},"olafurpg"),")"),(0,r.kt)("li",{parentName:"ul"},"Pause compile-on-save while the editor window is not focused\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/709"},"#","709"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/agajek"},"agajek"),")"),(0,r.kt)("li",{parentName:"ul"},"Add gradle support to imports in metals and refactor build tool support.\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/pull/694"},"#","694"),"\n(",(0,r.kt)("a",{parentName:"li",href:"https://github.com/tgodzik"},"tgodzik"),")")))}h.isMDXComponent=!0}}]);