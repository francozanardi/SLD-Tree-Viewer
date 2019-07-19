// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

/* Example definition of a simple mode that understands a subset of
 * JavaScript:
 * Sacado de acá: https://codemirror.net/demo/simplemode.html
 * Para entender expresiones regulares en js: https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions#special-word-boundary
 */

// En token ponemos el nombre del estilo que va a tener.
// el estilo está definido en prolog.css, 

CodeMirror.defineSimpleMode("prolog", {
  // The start state contains the rules that are intially used
  start: [
    // The regex matches the token, the token property contains the type
    {regex: /"(?:[^\\]|\\.)*?(?:"|$)/, token: "string"},
	{regex: /[A-Z$][\w$]*/, token: "variable"},
	{regex: /\%.*/, token: "comment"},
    // A next property will cause the mode to move to a different state
    {regex: /\/\*/, token: "comment", next: "comment"}, // inicio de comment en bloque
    // You can match multiple tokens at once. Note that the captured
    // groups must span the whole string in this case
    {regex: /([a-z$][\w$]*)([\s|\(\w*\)]*)(\:\-)/, //esto anda muy mal jaja, pero bueno, la idea sería reconocer un predicado.
     token: ["predicate", null, "keyword"]},
    // Rules are matched in the order in which they appear, so there is
    // no ambiguity between this one and the one above
    {regex: /(?:\:\-|\-\>|\,|\;)/,
     token: "keyword"},
    {regex: /true|false|fail|repeat/, token: "builtin"},
    {regex: /0x[a-f\d]+|[-+]?(?:\.\d+|\d+\.?\d*)(?:e[-+]?\d+)?/i, //creo que acá está leyendo números en not científica.
     token: "number"},
    {regex: /[-+\/*=<>!]+/, token: "operator"},
    // indent and dedent properties guide autoindentation
    {regex: /[\:\-]/, indent: true},
    {regex: /[\.]/, dedent: true},

  ],
  // The multi-line comment state.
  comment: [
    {regex: /.*?\*\//, token: "comment", next: "start"}, /* fin de comment en bloque */
    {regex: /.*/, token: "comment"}
  ],
  // The meta property contains global information about the mode. It
  // can contain properties like lineComment, which are supported by
  // all modes, and also directives like dontIndentStates, which are
  // specific to simple modes.
  meta: {
    dontIndentStates: ["comment"],
    lineComment: "%"
  }
});