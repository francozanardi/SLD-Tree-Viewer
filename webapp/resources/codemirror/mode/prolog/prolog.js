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
		{regex: /\%.*$/, token: "comment"},
	    {regex: /\/\*/, token: "comment", push: "comment"}, // inicio de comment en bloque
	    {regex: /\\"/, token: null},
	    {regex: /"/, token: "string", push: "string1"},
	    {regex: /\\'/, token: null},
	    {regex: /'/, token: "string", push: "string2"},
	    
		{regex: /(\w)(dynamic|use_module|module|if)|(?:(dynamic|use_module|module|if)\b)/,
		token: [null, null, "builtin"], push: "parametros"},	
		{regex: /\:\-/, token: "keyword"},
		
	    {regex: /[a-z]{1}\w*/, token: "predicate", push: "parametros"},


	],
	
	parametros: [
	    {regex: /\\"/, token: null},
	    {regex: /"/, token: "string", push: "string1"},
	    {regex: /\\'/, token: null},
	    {regex: /'/, token: "string", push: "string2"},
		{regex: /\d+(\.\d+){0,1}/, token: "number"},
	    
		{regex: /\./, token: "keyword", pop: true},
		{regex: /(\()([A-Z_]\w*)/, token: [null, "variable"], push: "parametros"},
		{regex: /\(/, token: null, push: "parametros"},
		{regex: /\)/, token: null, pop: true},
		{regex: /(\W){1}([A-Z_]\w*)/, token: [null, "variable"]},
		{regex: /\:\-/, token: "keyword", indent: true, next: "cuerpo"},
	],
	
	cuerpo: [
		{regex: /\%.*$/, token: "comment"},
	    {regex: /\/\*/, token: "comment", push: "comment"}, // inicio de comment en bloque
	    {regex: /\\"/, token: null},
	    {regex: /"/, token: "string", push: "string1"},
	    {regex: /\\'/, token: null},
	    {regex: /'/, token: "string", push: "string2"},
	    {regex: /\d+(\.\d+){0,1}/, token: "number"},
	    
	    {regex: /(\()([A-Z_]\w*)/, token: [null, "variable"], push: "cuerpo"},
		{regex: /\(/, token: null, push: "cuerpo"},
		{regex: /\)/, token: null, pop: true},
		{regex: /(\W){1}([A-Z_]\w*)/, token: [null, "variable"]},
		{regex: /\\\+|\-\>|\;|\,|\!|\=\.\.|\|/, token: "keyword"},
		{regex: /\./, token: "keyword", dedent: true, pop: true},
		
//		{regex: /(\w)(as|is|true|false|fail|repeat|forall|findall|member|length|last|nth0|nth1|not|concat_atom|flatten|min|max|reverse|maplist|sublist|append|assertz|retract|retractall|asserta)|(?:(is|true|false|fail|repeat|forall|findall|member|length|last|nth0|nth1|not|concat_atom|flatten|min|max|reverse|maplist|sublist|append|assertz|retract|retractall|asserta)\b)/,
//		token: [null, null, "builtin"]},
//		evitar repetir código trae el problema de que no reconoce la palabra clave si esta es el primer caracter del estado cuerpo.
		{regex: /\W(is|true|false|fail|repeat|forall|findall|member|length|last|nth0|nth1|not|concat_atom|flatten|min|max|reverse|maplist|sublist|append|assertz|retract|retractall|asserta|assert|write|writeln|nl|between|read|clause|functor|arg|nonvar|var|sleep)\b/, token: "builtin"},
		{regex: /\\\+|\-\>|\;|\,|\!/, token: "keyword"},

	],
	
	string1: [
		{regex: /[^\\"]"/, token: "string", pop: true},
		{regex: /\\"/, token: "string"},
		{regex: /[^"]/, token: "string"},
		{regex: /^"/, token: "string", pop: true},
	],
	
	string2: [
		{regex: /[^\\']'/, token: "string", pop: true},
		{regex: /\\'/, token: "string"},
		{regex: /[^']/, token: "string"},
		{regex: /^'/, token: "string", pop: true},
	],
	
	comment: [
		{regex: /.*?\*\//, token: "comment", pop: true}, /* fin de comment en bloque */
		{regex: /.*/, token: "comment"}
	]

	
	
});
	
//  start: [
//    // indent and dedent properties guide autoindentation
//    {regex: /\:\-/, token:"keyword", indent: true},
//    {regex: /\./, token:"keyword", dedent: true},
//    // The regex matches the token, the token property contains the type
//    {regex: /"(?:[^\\]|\\.)*?(?:"|$)/, token: "string"},
//	{regex: /(\W+)([A-Z_]\w*)|([^A-Z_][A-Z_]\w*)|(^[A-Z_]\w*)/, token: [null, "variable", null, "variable"]},
//	{regex: /\%.*/, token: "comment"},
//    // A next property will cause the mode to move to a different state
//    {regex: /\/\*/, token: "comment", next: "comment"}, // inicio de comment en bloque
//	  {regex: /([a-z]{1}\w*)(\s*)(\(\s*\w+\s*(?:\,\s*\w+)*\s*\)|\(\s*\)){0,1}(\s*(?:\:\-)|(?:\.))/, token: ["predicate", null, null, "keyword"], indent: true},
//    // You can match multiple tokens at once. Note that the captured
//    // groups must span the whole string in this case
//	  {regex: /([a-z]{1}\w*)(\s*)(\(\s*\w+\s*(?:\,\s*\w+)*\s*\)|\(\s*\)){0,1}(\s*\:\-)/,
//	  token: ["predicate", null, null, "keyword"], indent: true},
////    {regex: /([a-z$][\w$]*)([\s|\(\w*\)]*)(\:\-)/, //esto anda muy mal jaja, pero bueno, la idea sería reconocer un predicado.
////     token: ["predicate", null, "keyword"]},
//    //([a-z]{1}[\w]*)(\s*)(\(\w*\)*)(\:\-) otro intento fallido, hay que pensarlo mejor.
//    // Rules are matched in the order in which they appear, so there is
//    // no ambiguity between this one and the one above
//    {regex: /\-\>|\,|\;|\./,
//     token: "keyword"},
//    {regex: /(\W)((?:true|false|fail|repeat)\b)|(\w(?:true|false|fail|repeat))|(?:(true|false|fail|repeat)\b)/, token: [null,"builtin", null, "builtin"]},
//    {regex: /<</, token: "keyword", push: "desbalanceado"},
//    {regex: /0x[a-f\d]+|[-+]?(?:\.\d+|\d+\.?\d*)(?:e[-+]?\d+)?/i, //creo que acá está leyendo números en not científica.
//     token: "number"},
//    {regex: /[-+\/*=<>!]+/, token: "operator"},
//    {regex: /[a-z]/, token: "keyword"}


//  ],
  // The multi-line comment state.
//  comment: [
//    {regex: /.*?\*\//, token: "comment", next: "start"}, /* fin de comment en bloque */
//    {regex: /.*/, token: "comment"}
//  ],
  
//  desbalanceado: [
//	  {regex: /<</, token: "keyword", push: "desbalanceado"},
//	  {regex: />>/, token: "keyword", pop: true}
//	  ],
  // The meta property contains global information about the mode. It
  // can contain properties like lineComment, which are supported by
  // all modes, and also directives like dontIndentStates, which are
  // specific to simple modes.
//  meta: {
//    dontIndentStates: ["comment"],
//    lineComment: "%"
//  }
//});