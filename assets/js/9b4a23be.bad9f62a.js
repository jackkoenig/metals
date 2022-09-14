"use strict";(self.webpackChunk=self.webpackChunk||[]).push([[5688],{3905:(t,e,n)=>{n.d(e,{Zo:()=>c,kt:()=>p});var l=n(7294);function i(t,e,n){return e in t?Object.defineProperty(t,e,{value:n,enumerable:!0,configurable:!0,writable:!0}):t[e]=n,t}function r(t,e){var n=Object.keys(t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(t);e&&(l=l.filter((function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable}))),n.push.apply(n,l)}return n}function a(t){for(var e=1;e<arguments.length;e++){var n=null!=arguments[e]?arguments[e]:{};e%2?r(Object(n),!0).forEach((function(e){i(t,e,n[e])})):Object.getOwnPropertyDescriptors?Object.defineProperties(t,Object.getOwnPropertyDescriptors(n)):r(Object(n)).forEach((function(e){Object.defineProperty(t,e,Object.getOwnPropertyDescriptor(n,e))}))}return t}function o(t,e){if(null==t)return{};var n,l,i=function(t,e){if(null==t)return{};var n,l,i={},r=Object.keys(t);for(l=0;l<r.length;l++)n=r[l],e.indexOf(n)>=0||(i[n]=t[n]);return i}(t,e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(t);for(l=0;l<r.length;l++)n=r[l],e.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(t,n)&&(i[n]=t[n])}return i}var s=l.createContext({}),d=function(t){var e=l.useContext(s),n=e;return t&&(n="function"==typeof t?t(e):a(a({},e),t)),n},c=function(t){var e=d(t.components);return l.createElement(s.Provider,{value:e},t.children)},k={inlineCode:"code",wrapper:function(t){var e=t.children;return l.createElement(l.Fragment,{},e)}},u=l.forwardRef((function(t,e){var n=t.components,i=t.mdxType,r=t.originalType,s=t.parentName,c=o(t,["components","mdxType","originalType","parentName"]),u=d(n),p=i,m=u["".concat(s,".").concat(p)]||u[p]||k[p]||r;return n?l.createElement(m,a(a({ref:e},c),{},{components:n})):l.createElement(m,a({ref:e},c))}));function p(t,e){var n=arguments,i=e&&e.mdxType;if("string"==typeof t||i){var r=n.length,a=new Array(r);a[0]=u;var o={};for(var s in e)hasOwnProperty.call(e,s)&&(o[s]=e[s]);o.originalType=t,o.mdxType="string"==typeof t?t:i,a[1]=o;for(var d=2;d<r;d++)a[d]=n[d];return l.createElement.apply(null,a)}return l.createElement.apply(null,n)}u.displayName="MDXCreateElement"},8731:(t,e,n)=>{n.r(e),n.d(e,{contentTitle:()=>s,default:()=>u,frontMatter:()=>o,metadata:()=>d,toc:()=>c});var l=n(7462),i=n(3366),r=(n(7294),n(3905)),a=["components"],o={id:"overview",title:"Text Editors",sidebar_label:"Overview",slug:"/"},s=void 0,d={unversionedId:"editors/overview",id:"editors/overview",title:"Text Editors",description:"Latest Metals server versions",source:"@site/target/docs/editors/overview.md",sourceDirName:"editors",slug:"/",permalink:"/metals/docs/",editUrl:"https://github.com/scalameta/metals/edit/main/docs/editors/overview.md",tags:[],version:"current",frontMatter:{id:"overview",title:"Text Editors",sidebar_label:"Overview",slug:"/"},sidebar:"docs",next:{title:"VS Code",permalink:"/metals/docs/editors/vscode"}},c=[{value:"Latest Metals server versions",id:"latest-metals-server-versions",children:[],level:2},{value:"Editor support",id:"editor-support",children:[],level:2},{value:"Installation",id:"installation",children:[],level:2},{value:"Build import",id:"build-import",children:[],level:2},{value:"Diagnostics",id:"diagnostics",children:[{value:"Known limitations",id:"known-limitations",children:[],level:3}],level:2},{value:"Goto definition",id:"goto-definition",children:[{value:"Known limitations",id:"known-limitations-1",children:[],level:3}],level:2},{value:"Completions",id:"completions",children:[{value:"Known limitations",id:"known-limitations-2",children:[],level:3}],level:2},{value:"Hover (aka. type at point)",id:"hover-aka-type-at-point",children:[],level:2},{value:"Signature help (aka. parameter hints)",id:"signature-help-aka-parameter-hints",children:[],level:2},{value:"Find references",id:"find-references",children:[{value:"Known limitations",id:"known-limitations-3",children:[],level:3}],level:2},{value:"Worksheets",id:"worksheets",children:[],level:2},{value:"Document symbols",id:"document-symbols",children:[],level:2},{value:"Workspace symbols",id:"workspace-symbols",children:[],level:2},{value:"Formatting",id:"formatting",children:[],level:2},{value:"Code folding",id:"code-folding",children:[],level:2},{value:"Document highlight",id:"document-highlight",children:[],level:2},{value:"Package explorer",id:"package-explorer",children:[],level:2},{value:"Test Explorer",id:"test-explorer",children:[{value:"Running Tests",id:"running-tests",children:[],level:3}],level:2},{value:"Metals Extensions",id:"metals-extensions",children:[],level:2},{value:"Implicit decorations",id:"implicit-decorations",children:[],level:2},{value:"Additional file types",id:"additional-file-types",children:[],level:2}],k={toc:c};function u(t){var e=t.components,n=(0,i.Z)(t,a);return(0,r.kt)("wrapper",(0,l.Z)({},k,n,{components:e,mdxType:"MDXLayout"}),(0,r.kt)("h2",{id:"latest-metals-server-versions"},"Latest Metals server versions"),(0,r.kt)("p",null,"To find out how to set the version in your editor please check out the editor\nspecific sections."),(0,r.kt)("table",null,(0,r.kt)("thead",null,(0,r.kt)("tr",null,(0,r.kt)("th",null,"Version"),(0,r.kt)("th",null,"Published"))),(0,r.kt)("tbody",null,(0,r.kt)("tr",null,(0,r.kt)("td",null,"0.11.8"),(0,r.kt)("td",null,"10 Aug 2022 04:08")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"0.11.8+100-20a64360-SNAPSHOT"),(0,r.kt)("td",null,"14 Sep 2022 17:18")))),"Snapshot releases are not guaranteed to work.",(0,r.kt)("h2",{id:"editor-support"},"Editor support"),(0,r.kt)("p",null,"Metals works with the following text editors with varying degree of\nfunctionality."),(0,r.kt)("table",null,(0,r.kt)("thead",null,(0,r.kt)("tr",null,(0,r.kt)("td",null),(0,r.kt)("td",{align:"center"},"Visual Studio Code"),(0,r.kt)("td",{align:"center"},"Vim"),(0,r.kt)("td",{align:"center"},"Sublime Text"),(0,r.kt)("td",{align:"center"},"Emacs"))),(0,r.kt)("tbody",null,(0,r.kt)("tr",null,(0,r.kt)("td",null,"Installation"),(0,r.kt)("td",{align:"center"},"Single click"),(0,r.kt)("td",{align:"center"},"Single click"),(0,r.kt)("td",{align:"center"},"Single click"),(0,r.kt)("td",{align:"center"},"Few steps")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Build import"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Diagnostics"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Goto definition"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Completions"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705*"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Hover"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Hover for selection"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u274c")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Parameter hints"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Find references"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Run/Debug"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"}),(0,r.kt)("td",{align:"center"}),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Find implementations"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Rename symbol"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Code actions"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Worksheets"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"Comments")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Document symbols"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"Flat"),(0,r.kt)("td",{align:"center"},"Flat"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Workspace symbols"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Formatting"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Folding"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"}," "),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Highlight"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Metals Extensions"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"Status bar, Input box, Decoration protocol, Did focus"),(0,r.kt)("td",{align:"center"},"Status bar")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Organize imports"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Implicit decorations"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"Shown in hover"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Source file analyzer"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u274c")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Find text in dependency JAR files"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u274c")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Run scalafix rules"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")))),(0,r.kt)("h2",{id:"installation"},"Installation"),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Single click"),": Metals is easy to install and requires minimal configuration\nout-of-the-box."),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Few steps"),": installing Metals requires a few custom steps and minimal\nconfiguration to work."),(0,r.kt)("p",null,(0,r.kt)("em",{parentName:"p"},"You can find instructions on how to install Metals for your editor on its\nspecific page.")),(0,r.kt)("h2",{id:"build-import"},"Build import"),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"\u2705"),": it is possible to import a build such as an sbt project directly from\nthe editor."),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Requires browser"),": importing a build requires additional steps in a web\nbrowser using a localhost server. It is not possible to import a build within\nthe editor."),(0,r.kt)("h2",{id:"diagnostics"},"Diagnostics"),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"\u2705"),": Diagnostics are correctly published on compile."),(0,r.kt)("p",null,"Compile errors are reported as red squiggles in the editor. Compilation is\ntriggered on file save for the build target (project/module) containing the\nfocused text file."),(0,r.kt)("p",null,(0,r.kt)("img",{parentName:"p",src:"https://user-images.githubusercontent.com/1408093/48774587-f4d5c780-ecca-11e8-8087-acca5a05ca78.png",alt:"Diagnostics"})),(0,r.kt)("h3",{id:"known-limitations"},"Known limitations"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Slow feedback for type errors. Syntax errors are published as you type but\ntype errors are handled by the build tool, meaning diagnostics may take a\nwhile to publish for large projects.")),(0,r.kt)("h2",{id:"goto-definition"},"Goto definition"),(0,r.kt)("p",null,"Navigate to symbol definitions for project sources and Scala/Java library\ndependencies."),(0,r.kt)("p",null,"Symbols are resolved according to the last successful compilation in the build\ntool and navigation continues to work despite syntax errors in the open unsaved\nbuffer."),(0,r.kt)("p",null,(0,r.kt)("img",{parentName:"p",src:"https://user-images.githubusercontent.com/1408093/48776422-1f764f00-ecd0-11e8-96d1-170f2354d50e.gif",alt:"Goto Definition"})),(0,r.kt)("h3",{id:"known-limitations-1"},"Known limitations"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Navigation does not work for buffers that do not tokenize, for example due to\nunclosed string literals."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/scalameta/issues/1802"},"scalameta/scalameta#1802"),"\nreflective invocations (methods calls on structural types) do not resolve to a\ndefinition.")),(0,r.kt)("h2",{id:"completions"},"Completions"),(0,r.kt)("p",null,"Use code completions to explore APIs, implement interfaces, generate exhaustive\npattern matches and more."),(0,r.kt)("p",null,(0,r.kt)("img",{parentName:"p",src:"https://user-images.githubusercontent.com/1408093/56036958-725bac00-5d2e-11e9-9cf7-46249125494a.gif",alt:"2019-04-12 14 19 39"})),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("strong",{parentName:"li"},"Auto-import"),": imports are inserted at the bottom of the global import list.\nImports still need to be sorted and grouped manually, we are exploring ways to\nautomate this workflow in the future."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("strong",{parentName:"li"},"Override def"),": implement methods from the super class."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("strong",{parentName:"li"},"Exhaustive match"),": generate an exhaustive pattern match for sealed types."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("strong",{parentName:"li"},"String interpolator"),": automatically convert string literals into string\ninterpolators."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("strong",{parentName:"li"},"Filename"),": complete classnames based on the enclosing file."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("strong",{parentName:"li"},"Documentation"),": read the docstring for method symbols by pressing\nctrl+space in VS Code.")),(0,r.kt)("h3",{id:"known-limitations-2"},"Known limitations"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"completion results don't include symbols that have just been typed in separate\nfiles without a successful compilation in the build tool.")),(0,r.kt)("h2",{id:"hover-aka-type-at-point"},"Hover (aka. type at point)"),(0,r.kt)("p",null,"See the expression type and symbol signature under the cursor."),(0,r.kt)("p",null,(0,r.kt)("img",{parentName:"p",src:"https://i.imgur.com/2MfQvsM.gif",alt:null})),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("strong",{parentName:"li"},"Expression type"),": shows the non-generic type of the highlighted expression."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("strong",{parentName:"li"},"Symbol signature"),": shows the generic signature of symbol under the cursor\nalong with its docstring, if available.")),(0,r.kt)("h2",{id:"signature-help-aka-parameter-hints"},"Signature help (aka. parameter hints)"),(0,r.kt)("p",null,"View a method signature and method overloads as you fill in the arguments."),(0,r.kt)("p",null,(0,r.kt)("img",{parentName:"p",src:"https://i.imgur.com/DAWIrHu.gif",alt:null})),(0,r.kt)("h2",{id:"find-references"},"Find references"),(0,r.kt)("p",null,"Find symbol references in project sources. References include implicits,\ninferred ",(0,r.kt)("inlineCode",{parentName:"p"},".apply"),", desugared ",(0,r.kt)("inlineCode",{parentName:"p"},".flatMap")," from for comprehensions and other\nsymbols that may not be explicitly written in source, making it possible to\ndiscover usages of difficult-to-grep symbols. The Metals navigation index is\nlow-overhead and should only require a few megabytes of memory even for large\nprojects."),(0,r.kt)("p",null,(0,r.kt)("img",{parentName:"p",src:"https://user-images.githubusercontent.com/1408093/51089190-75fc8880-1769-11e9-819c-95262205e95c.png",alt:"Find references"})),(0,r.kt)("h3",{id:"known-limitations-3"},"Known limitations"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},'References to overridden methods are not included in the results. For example,\nif you run "find references" on the method ',(0,r.kt)("inlineCode",{parentName:"li"},"Dog.name()")," then it won't include\nreferences to the super method ",(0,r.kt)("inlineCode",{parentName:"li"},"Animal.name()"),".")),(0,r.kt)("h2",{id:"worksheets"},"Worksheets"),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"\u2705"),": Worksheets work via the Decoration protocol and are added as a\nnon-editable side decoration."),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Comments"),": Worksheets work via ",(0,r.kt)("inlineCode",{parentName:"p"},"workspace/applyEdit")," by adding comments to\nthe source code and support hover to show larger output. You can find more\ninformation about worksheets under the editor specific worksheet section. For\nexample, ",(0,r.kt)("a",{parentName:"p",href:"/metals/docs/editors/vscode#worksheets"},"here for VS Code"),"."),(0,r.kt)("h2",{id:"document-symbols"},"Document symbols"),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"\u2705"),": Document symbols are displayed in a hierarchical outline."),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Flat"),": Document symbols are displayed in a flat outline."),(0,r.kt)("p",null,(0,r.kt)("img",{parentName:"p",src:"https://user-images.githubusercontent.com/1408093/50635569-014c7180-0f53-11e9-8898-62803898781c.gif",alt:"Document Symbols"})),(0,r.kt)("h2",{id:"workspace-symbols"},"Workspace symbols"),(0,r.kt)("p",null,"Fuzzy search a symbol in the workspace of library dependencies by its name."),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"All-lowercase queries are treated as case-insensitive searches."),(0,r.kt)("li",{parentName:"ul"},"Queries ending with a dot ",(0,r.kt)("inlineCode",{parentName:"li"},".")," list nested symbols."),(0,r.kt)("li",{parentName:"ul"},"Queries containing a semicolon ",(0,r.kt)("inlineCode",{parentName:"li"},";")," search library dependencies.")),(0,r.kt)("p",null,(0,r.kt)("img",{parentName:"p",src:"https://i.imgur.com/w5yrK1w.gif",alt:"Fuzzy symbol search example"})),(0,r.kt)("h2",{id:"formatting"},"Formatting"),(0,r.kt)("p",null,"Metals uses Scalafmt to respond to formatting requests from the editor,\naccording to the configuration defined in ",(0,r.kt)("inlineCode",{parentName:"p"},".scalafmt.conf"),"."),(0,r.kt)("p",null,"Learn how to configure Scalafmt at\n",(0,r.kt)("a",{parentName:"p",href:"https://scalameta.org/scalafmt/docs/configuration.html"},"https://scalameta.org/scalafmt/docs/configuration.html"),"."),(0,r.kt)("p",null,(0,r.kt)("img",{parentName:"p",src:"https://user-images.githubusercontent.com/1408093/50635748-b0894880-0f53-11e9-913b-acfd5f505351.gif",alt:"Formatting"})),(0,r.kt)("h2",{id:"code-folding"},"Code folding"),(0,r.kt)("p",null,"Fold ranges such as large multi-line expressions, import groups and comments."),(0,r.kt)("p",null,(0,r.kt)("img",{parentName:"p",src:"https://camo.githubusercontent.com/3fdd7ae28907ac61c0a1ac5fdc07d085245957aa/68747470733a2f2f692e696d6775722e636f6d2f667149554a54472e676966",alt:null})),(0,r.kt)("h2",{id:"document-highlight"},"Document highlight"),(0,r.kt)("p",null,"Highlight references to the same symbol in the open file."),(0,r.kt)("p",null,(0,r.kt)("img",{parentName:"p",src:"https://i.imgur.com/0uhc9P5.gif",alt:null})),(0,r.kt)("h2",{id:"package-explorer"},"Package explorer"),(0,r.kt)("p",null,"Browse packages, classes and methods in the workspace and library dependencies\nusing the Metals sidebar. This feature is only implemented in VS Code."),(0,r.kt)("h2",{id:"test-explorer"},"Test Explorer"),(0,r.kt)("p",null,"Test Explorer is a feature that allows editors to display tests as a separate\ntree representation of tests. Although it was implemented in order to use Visual\nStudio Code's\n",(0,r.kt)("a",{parentName:"p",href:"https://code.visualstudio.com/api/extension-guides/testing"},"Testing API"),". The Test\nExplorer API is editor agnostic and can be used by other editors than just VS\nCode. ",(0,r.kt)("img",{parentName:"p",src:"https://i.imgur.com/Z3VtS0O.gif",alt:"test-explorer"})),(0,r.kt)("p",null,"Work on the Test Explorer is still in progress and the feature has some known\nlimitations:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Test Explorer is able to discover single test cases only for JUnit4 test\nclasses. Support for other test frameworks is being worked on."),(0,r.kt)("li",{parentName:"ul"},"detecting suites in cross scala-version projects is inconsistent, see\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals/issues/3503"},"this issue"),"."),(0,r.kt)("li",{parentName:"ul"},"there is no support for JS and Native platforms. For any changes subscribe to\nthe related\n",(0,r.kt)("a",{parentName:"li",href:"https://github.com/scalameta/metals-feature-requests/issues/256"},"feature request"),".")),(0,r.kt)("p",null,"You can find more information about Test Explorer under the\n",(0,r.kt)("a",{parentName:"p",href:"/metals/docs/editors/vscode#test-explorer"},"VS Code")," specific section."),(0,r.kt)("h3",{id:"running-tests"},"Running Tests"),(0,r.kt)("p",null,"Both run and debug under the hood use BSP's debug request. More information\nabout it can be found at\n",(0,r.kt)("a",{parentName:"p",href:"https://github.com/scalacenter/bloop/blob/master/docs/assets/dap-example-metals.png"},"Bloop DAP diagram"),"\nor\n",(0,r.kt)("a",{parentName:"p",href:"https://build-server-protocol.github.io/docs/specification.html#debug-request"},"BSP specification"),"\nwebsite."),(0,r.kt)("h2",{id:"metals-extensions"},"Metals Extensions"),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Status bar"),": Editor client implements the ",(0,r.kt)("inlineCode",{parentName:"p"},"metals/status")," notification."),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Decoration protocol"),": Editor client impliments the\n",(0,r.kt)("a",{parentName:"p",href:"/metals/docs/integrations/decoration-protocol"},"Decoration Protocol"),"."),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Tree view"),": Editor client implements the\n",(0,r.kt)("a",{parentName:"p",href:"/metals/docs/integrations/tree-view-protocol"},"Tree View Protocol"),"."),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Did focus"),": Editor client implements the ",(0,r.kt)("inlineCode",{parentName:"p"},"metals/didFocusTextDocument"),"\nnotification."),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Slow task"),": Editor client implements the ",(0,r.kt)("inlineCode",{parentName:"p"},"metals/slowTask")," request."),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Input box"),": Editor client implements the ",(0,r.kt)("inlineCode",{parentName:"p"},"metals/inputBox")," request."),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Quick pick"),": Editor client implements the ",(0,r.kt)("inlineCode",{parentName:"p"},"metals/quickPick")," request."),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Window state"),": Editor client implements the ",(0,r.kt)("inlineCode",{parentName:"p"},"metals/windowStateDidChange"),"\nnotification."),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"\u2705"),": Editor implements all Metals extension endpoints."),(0,r.kt)("p",null,"The Metals language server supports custom extensions that are not part of the\nLanguage Server Protocol (LSP). These extensions are not necessary for Metals to\nfunction but they improve the user experience. To learn more about Metals\nextensions, see ",(0,r.kt)("a",{parentName:"p",href:"/metals/docs/integrations/new-editor"},"integrating a new editor"),"."),(0,r.kt)("h2",{id:"implicit-decorations"},"Implicit decorations"),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"\u2705"),": Additional information inferred from the code can be show within the\ncode using virtual text."),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Shown in hover"),": Additional information inferred from the code can be show\nwhen hovering over a specific line. That hover only shows the additional symbols\non the current line."),(0,r.kt)("h2",{id:"additional-file-types"},"Additional file types"),(0,r.kt)("p",null,"Not all features are supported in all possible scenarios, especially when it\ncomes to non-standard Scala files like Ammonite scripts, worksheets or sbt\nscripts."),(0,r.kt)("table",null,(0,r.kt)("thead",null,(0,r.kt)("tr",null,(0,r.kt)("td",null),(0,r.kt)("td",{align:"center"},"sbt scripts"),(0,r.kt)("td",{align:"center"},"Worksheets"),(0,r.kt)("td",{align:"center"},"Ammonite scripts*"),(0,r.kt)("td",{align:"center"},"Standalone Scala files"))),(0,r.kt)("tbody",null,(0,r.kt)("tr",null,(0,r.kt)("td",null,"Diagnostics"),(0,r.kt)("td",{align:"center"},"\u2705*"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705*")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Goto definition"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Completions"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Hover"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Parameter hints"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Find references"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Run/Debug"),(0,r.kt)("td",{align:"center"}),(0,r.kt)("td",{align:"center"}),(0,r.kt)("td",{align:"center"}),(0,r.kt)("td",{align:"center"})),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Find implementations"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"}),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Rename symbol"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Code actions"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Document symbols"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Workspace symbols"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"All symbols are local"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Formatting"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Folding"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Highlight"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Organize imports"),(0,r.kt)("td",{align:"center"}),(0,r.kt)("td",{align:"center"}),(0,r.kt)("td",{align:"center"}),(0,r.kt)("td",{align:"center"})),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Implicit decorations"),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"}),(0,r.kt)("td",{align:"center"},"\u2705"),(0,r.kt)("td",{align:"center"},"\u2705")),(0,r.kt)("tr",null,(0,r.kt)("td",null,"Decode file (cfr, semanticdb, tasty, javap)"),(0,r.kt)("td",{align:"center"}),(0,r.kt)("td",{align:"center"}),(0,r.kt)("td",{align:"center"}),(0,r.kt)("td",{align:"center"})))),(0,r.kt)("p",null,"*"," Note that there are some specific Ammonite features that aren't supported\nlike ",(0,r.kt)("a",{parentName:"p",href:"https://ammonite.io/#Multi-stageScripts"},"multi-stage")," scripts. Currently\nAmmonite support is also limited to Scala 2."),(0,r.kt)("p",null,"*"," Diagnostics for sbt script and standalone Scala files will only show parsing\nerrors, but not diagnostics coming from the compiler."))}u.isMDXComponent=!0}}]);