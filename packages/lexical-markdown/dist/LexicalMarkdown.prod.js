/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
'use strict';var k=require("lexical"),t=require("@lexical/code"),z=require("@lexical/list"),B=require("@lexical/rich-text"),aa=require("@lexical/utils"),G=require("@lexical/link");function H(a,b){let c={};for(let d of a)a=b(d),c[a]?c[a].push(d):c[a]=[d];return c}function I(a){a=H(a,b=>b.type);return{element:a.element||[],textFormat:a["text-format"]||[],textMatch:a["text-match"]||[]}}let J=/[!-/:-@[-`{-~\s]/;
function ba(a){let b=I(a),c=b.textFormat.filter(d=>1===d.format.length);return d=>{let e=[];d=(d||k.$getRoot()).getChildren();for(let f of d)d=ca(f,b.element,c,b.textMatch),null!=d&&e.push(d);return e.join("\n\n")}}function ca(a,b,c,d){for(let e of b)if(b=e.export(a,f=>K(f,c,d)),null!=b)return b;return k.$isElementNode(a)?K(a,c,d):k.$isDecoratorNode(a)?a.getTextContent():null}
function K(a,b,c){let d=[];a=a.getChildren();a:for(let e of a){for(let f of c)if(a=f.export(e,g=>K(g,b,c),(g,l)=>L(g,l,b)),null!=a){d.push(a);continue a}k.$isLineBreakNode(e)?d.push("\n"):k.$isTextNode(e)?d.push(L(e,e.getTextContent(),b)):k.$isElementNode(e)?d.push(K(e,b,c)):k.$isDecoratorNode(e)&&d.push(e.getTextContent())}return d.join("")}
function L(a,b,c){let d=b.trim(),e=d,f=new Set;for(let l of c){c=l.format[0];let r=l.tag;if(M(a,c)&&!f.has(c)){f.add(c);var g=N(a,!0);M(g,c)||(e=r+e);g=N(a,!1);M(g,c)||(e+=r)}}return b.replace(d,()=>e)}
function N(a,b){let c=b?a.getPreviousSibling():a.getNextSibling();c||(a=a.getParentOrThrow(),a.isInline()&&(c=b?a.getPreviousSibling():a.getNextSibling()));for(;c;){if(k.$isElementNode(c)){if(!c.isInline())break;a=b?c.getLastDescendant():c.getFirstDescendant();if(k.$isTextNode(a))return a;c=b?c.getPreviousSibling():c.getNextSibling()}if(k.$isTextNode(c))return c;if(!k.$isElementNode(c))break}return null}function M(a,b){return k.$isTextNode(a)&&a.hasFormat(b)}
let O="undefined"!==typeof window&&"undefined"!==typeof window.document&&"undefined"!==typeof window.document.createElement,da=O&&"documentMode"in document?document.documentMode:null;O&&/Mac|iPod|iPhone|iPad/.test(navigator.platform);O&&/^(?!.*Seamonkey)(?=.*Firefox).*/i.test(navigator.userAgent);O&&"InputEvent"in window&&!da?"getTargetRanges"in new window.InputEvent("input"):!1;
let P=O&&/Version\/[\d.]+.*Safari/.test(navigator.userAgent),Q=O&&/iPad|iPhone|iPod/.test(navigator.userAgent)&&!window.MSStream,ea=O&&/Android/.test(navigator.userAgent),R=O&&/^(?=.*Chrome).*/i.test(navigator.userAgent);O&&ea&&R;let S=O&&/AppleWebKit\/[\d.]+/.test(navigator.userAgent)&&!R,fa=/^\s{0,3}$/,T=/^```(\w{1,10})?\s?$/;
function ha(a){let b=I(a),c=ia(b.textFormat);return(d,e)=>{d=d.split("\n");var f=d.length;e=e||k.$getRoot();e.clear();for(let h=0;h<f;h++){var g=d[h];a:{var l=d,r=h;var q=e;var y=l[r].match(T);if(y)for(var p=r,m=l.length;++p<m;)if(l[p].match(T)){y=t.$createCodeNode(y[1]);l=k.$createTextNode(l.slice(r+1,p).join("\n"));y.append(l);q.append(y);q=[y,p];break a}q=[null,r]}let [n,v]=q;if(null!=n)h=v;else{q=g;m=e;var w=b.element;p=c;l=b.textMatch;r=q.trim();y=k.$createTextNode(r);g=k.$createParagraphNode();
g.append(y);m.append(g);for(let {regExp:x,replace:u}of w)if(m=q.match(x)){y.setTextContent(q.slice(m[0].length));u(g,[y],m,!0);break}U(y,p,l);g.isAttached()&&0<r.length&&(q=g.getPreviousSibling(),k.$isParagraphNode(q)||B.$isQuoteNode(q)||z.$isListNode(q))&&(p=q,z.$isListNode(q)&&(q=q.getLastDescendant(),p=null==q?null:aa.$findMatchingParent(q,z.$isListItemNode)),null!=p&&0<p.getTextContentSize()&&(p.splice(p.getChildrenSize(),0,[k.$createLineBreakNode(),...g.getChildren()]),g.remove()))}}d=e.getChildren();
for(let h of d)d=h,k.$isParagraphNode(d)?(f=d.getFirstChild(),d=null==f||1===d.getChildrenSize()&&k.$isTextNode(f)&&fa.test(f.getTextContent())):d=!1,d&&1<e.getChildrenSize()&&h.remove();null!==k.$getSelection()&&e.selectEnd()}}
function U(a,b,c){var d=a.getTextContent();let e=ja(d,b);if(e){var f,g;if(e[0]===d)var l=a;else{d=e.index||0;let r=d+e[0].length;0===d?[l,f]=a.splitText(r):[g,l,f]=a.splitText(d,r)}l.setTextContent(e[2]);if(a=b.transformersByTag[e[1]])for(let r of a.format)l.hasFormat(r)||l.toggleFormat(r);l.hasFormat("code")||U(l,b,c);g&&U(g,b,c);f&&U(f,b,c)}else V(a,c)}
function V(a,b){a:for(;a;){for(let c of b){let d=a.getTextContent().match(c.importRegExp);if(!d)continue;let e=d.index||0,f=e+d[0].length,g,l;0===e?[g,a]=a.splitText(f):[,g,l]=a.splitText(e,f);l&&V(l,b);c.replace(g,d);continue a}break}}
function ja(a,b){var c=a.match(b.openTagsRegExp);if(null==c)return null;for(let f of c){var d=f.replace(/^\s/,"");c=b.fullMatchRegExpByTag[d];if(null!=c&&(c=a.match(c),d=b.transformersByTag[d],null!=c&&null!=d)){if(!1!==d.intraword)return c;var {index:e=0}=c;d=a[e-1];e=a[e+c[0].length];if(!(d&&!J.test(d)||e&&!J.test(e)))return c}}return null}
function ia(a){let b={},c={},d=[];for(let e of a){({tag:a}=e);b[a]=e;let f=a.replace(/(\*|\^|\+)/g,"\\$1");d.push(f);c[a]=P||Q||S?new RegExp(`(${f})(?![${f}\\s])(.*?[^${f}\\s])${f}(?!${f})`):new RegExp(`(?<![\\\\${f}])(${f})((\\\\${f})?.*?[^${f}\\s](\\\\${f})?)((?<!\\\\)|(?<=\\\\\\\\))(${f})(?![\\\\${f}])`)}return{fullMatchRegExpByTag:c,openTagsRegExp:new RegExp((P||Q||S?"":"(?<![\\\\])")+"("+d.join("|")+")","g"),transformersByTag:b}}
function W(a,b,c){let d=c.length;for(;b>=d;b--){let e=b-d;if(ka(a,e,c,0,d)&&" "!==a[e+d])return e}return-1}function ka(a,b,c,d,e){for(let f=0;f<e;f++)if(a[b+f]!==c[d+f])return!1;return!0}
let la=a=>(b,c,d)=>{d=a(d);d.append(...c);b.replace(d);d.select(0,0)},X=a=>(b,c,d)=>{var e=b.getPreviousSibling(),f=b.getNextSibling();const g=z.$createListItemNode("check"===a?"x"===d[3]:void 0);z.$isListNode(f)&&f.getListType()===a?(e=f.getFirstChild(),null!==e?e.insertBefore(g):f.append(g),b.remove()):z.$isListNode(e)&&e.getListType()===a?(e.append(g),b.remove()):(f=z.$createListNode(a,"number"===a?Number(d[2]):void 0),f.append(g),b.replace(f));g.append(...c);g.select(0,0);c=d[1];b=c.match(/\t/g);
c=c.match(/ /g);d=0;b&&(d+=b.length);c&&(d+=Math.floor(c.length/2));(b=d)&&g.setIndent(b)},Y=(a,b,c)=>{const d=[];var e=a.getChildren();let f=0;for(const l of e)if(z.$isListItemNode(l)){if(1===l.getChildrenSize()&&(e=l.getFirstChild(),z.$isListNode(e))){d.push(Y(e,b,c+1));continue}e=" ".repeat(2*c);var g=a.getListType();g="number"===g?`${a.getStart()+f}. `:"check"===g?`- [${l.getChecked()?"x":" "}] `:"- ";d.push(e+g+b(l));f++}return d.join("\n")},ma={dependencies:[B.HeadingNode],export:(a,b)=>{if(!B.$isHeadingNode(a))return null;
const c=Number(a.getTag().slice(1));return"#".repeat(c)+" "+b(a)},regExp:/^(#{1,6})\s/,replace:la(a=>B.$createHeadingNode("h"+a[1].length)),type:"element"},na={dependencies:[B.QuoteNode],export:(a,b)=>{if(!B.$isQuoteNode(a))return null;a=b(a).split("\n");b=[];for(const c of a)b.push("> "+c);return b.join("\n")},regExp:/^>\s/,replace:(a,b,c,d)=>{if(d&&(c=a.getPreviousSibling(),B.$isQuoteNode(c))){c.splice(c.getChildrenSize(),0,[k.$createLineBreakNode(),...b]);c.select(0,0);a.remove();return}c=B.$createQuoteNode();
c.append(...b);a.replace(c);c.select(0,0)},type:"element"},oa={dependencies:[t.CodeNode],export:a=>{if(!t.$isCodeNode(a))return null;const b=a.getTextContent();return"```"+(a.getLanguage()||"")+(b?"\n"+b:"")+"\n```"},regExp:/^```(\w{1,10})?\s/,replace:la(a=>t.$createCodeNode(a?a[1]:void 0)),type:"element"},pa={dependencies:[z.ListNode,z.ListItemNode],export:(a,b)=>z.$isListNode(a)?Y(a,b,0):null,regExp:/^(\s*)[-*+]\s/,replace:X("bullet"),type:"element"},qa={dependencies:[z.ListNode,z.ListItemNode],
export:(a,b)=>z.$isListNode(a)?Y(a,b,0):null,regExp:/^(\s*)(?:-\s)?\s?(\[(\s|x)?\])\s/i,replace:X("check"),type:"element"},ra={dependencies:[z.ListNode,z.ListItemNode],export:(a,b)=>z.$isListNode(a)?Y(a,b,0):null,regExp:/^(\s*)(\d{1,})\.\s/,replace:X("number"),type:"element"},sa={format:["code"],tag:"`",type:"text-format"},ta={format:["highlight"],tag:"==",type:"text-format"},ua={format:["bold","italic"],tag:"***",type:"text-format"},wa={format:["bold","italic"],intraword:!1,tag:"___",type:"text-format"},
xa={format:["bold"],tag:"**",type:"text-format"},ya={format:["bold"],intraword:!1,tag:"__",type:"text-format"},za={format:["strikethrough"],tag:"~~",type:"text-format"},Aa={format:["italic"],tag:"*",type:"text-format"},Ba={format:["italic"],intraword:!1,tag:"_",type:"text-format"},Ca={dependencies:[G.LinkNode],export:(a,b,c)=>{if(!G.$isLinkNode(a))return null;b=(b=a.getTitle())?`[${a.getTextContent()}](${a.getURL()} "${b}")`:`[${a.getTextContent()}](${a.getURL()})`;const d=a.getFirstChild();return 1===
a.getChildrenSize()&&k.$isTextNode(d)?c(d,b):b},importRegExp:/(?:\[([^[]+)\])(?:\((?:([^()\s]+)(?:\s"((?:[^"]*\\")*[^"]*)"\s*)?)\))/,regExp:/(?:\[([^[]+)\])(?:\((?:([^()\s]+)(?:\s"((?:[^"]*\\")*[^"]*)"\s*)?)\))$/,replace:(a,b)=>{const [,c,d,e]=b;b=G.$createLinkNode(d,{title:e});const f=k.$createTextNode(c);f.setFormat(a.getFormat());b.append(f);a.replace(b)},trigger:")",type:"text-match"},Da=[ma,na,oa,pa,ra],Ea=[sa,ua,wa,xa,ya,ta,Aa,Ba,za],Fa=[Ca],Z=[...Da,...Ea,...Fa];
exports.$convertFromMarkdownString=function(a,b=Z,c){return ha(b)(a,c)};exports.$convertToMarkdownString=function(a=Z,b){return ba(a)(b)};exports.BOLD_ITALIC_STAR=ua;exports.BOLD_ITALIC_UNDERSCORE=wa;exports.BOLD_STAR=xa;exports.BOLD_UNDERSCORE=ya;exports.CHECK_LIST=qa;exports.CODE=oa;exports.ELEMENT_TRANSFORMERS=Da;exports.HEADING=ma;exports.HIGHLIGHT=ta;exports.INLINE_CODE=sa;exports.ITALIC_STAR=Aa;exports.ITALIC_UNDERSCORE=Ba;exports.LINK=Ca;exports.ORDERED_LIST=ra;exports.QUOTE=na;
exports.STRIKETHROUGH=za;exports.TEXT_FORMAT_TRANSFORMERS=Ea;exports.TEXT_MATCH_TRANSFORMERS=Fa;exports.TRANSFORMERS=Z;exports.UNORDERED_LIST=pa;
exports.registerMarkdownShortcuts=function(a,b=Z){let c=I(b),d=H(c.textFormat,({tag:f})=>f[f.length-1]),e=H(c.textMatch,({trigger:f})=>f);for(let f of b)if(b=f.type,"element"===b||"text-match"===b){b=f.dependencies;for(let g of b)if(!a.hasNode(g))throw Error(`MarkdownShortcuts: missing dependency ${g.getType()} for transformer. Ensure node dependency is included in editor initial config.`);}return a.registerUpdateListener(({tags:f,dirtyLeaves:g,editorState:l,prevEditorState:r})=>{if(!f.has("collaboration")&&
!f.has("historic")&&!a.isComposing()){var q=l.read(k.$getSelection);f=r.read(k.$getSelection);if(k.$isRangeSelection(f)&&k.$isRangeSelection(q)&&q.isCollapsed()){r=q.anchor.key;var y=q.anchor.offset,p=l._nodeMap.get(r);!k.$isTextNode(p)||!g.has(r)||1!==y&&y>f.anchor.offset+1||a.update(()=>{if(!p.hasFormat("code")){var m=p.getParent();if(null!==m&&!t.$isCodeNode(m)){var w=q.anchor.offset;b:{var h=c.element,n=m.getParent();if(k.$isRootOrShadowRoot(n)&&m.getFirstChild()===p&&(n=p.getTextContent()," "===
n[w-1]))for(let {regExp:D,replace:E}of h)if((h=n.match(D))&&h[0].length===w){n=p.getNextSiblings();let [F,va]=p.splitText(w);F.remove();n=va?[va,...n]:n;E(m,n,h,!1);m=!0;break b}m=!1}if(!m){b:{h=p.getTextContent();m=e[h[w-1]];if(null!=m){w<h.length&&(h=h.slice(0,w));for(x of m)if(m=h.match(x.regExp),null!==m){h=m.index||0;n=h+m[0].length;var v=void 0;0===h?[v]=p.splitText(n):[,v]=p.splitText(h,n);v.selectNext(0,0);x.replace(v,m);var x=!0;break b}}x=!1}if(!x)b:{n=p.getTextContent();--w;var u=n[w];
if(x=d[u])for(let D of x){var {tag:C}=D;x=C.length;let E=w-x+1;if(!(1<x&&!ka(n,E,C,0,x)||" "===n[E-1])&&(v=n[w+1],!1!==D.intraword||!v||J.test(v))){m=v=p;h=W(n,E,C);for(var A=m;0>h&&(A=A.getPreviousSibling())&&!k.$isLineBreakNode(A);)k.$isTextNode(A)&&(h=A.getTextContent(),m=A,h=W(h,h.length,C));if(!(0>h||m===v&&h+x===E||(C=m.getTextContent(),0<h&&C[h-1]===u||(A=C[h-1],!1===D.intraword&&A&&!J.test(A))))){n=v.getTextContent();n=n.slice(0,E)+n.slice(w+1);v.setTextContent(n);n=m===v?n:C;m.setTextContent(n.slice(0,
h)+n.slice(h+x));n=k.$getSelection();u=k.$createRangeSelection();k.$setSelection(u);w=w-x*(m===v?2:1)+1;u.anchor.set(m.__key,h,"text");u.focus.set(v.__key,w,"text");for(let F of D.format)u.hasFormat(F)||u.formatText(F);u.anchor.set(u.focus.key,u.focus.offset,u.focus.type);for(let F of D.format)u.hasFormat(F)&&u.toggleFormat(F);k.$isRangeSelection(n)&&(u.format=n.format);break b}}}}}}}})}}})}
