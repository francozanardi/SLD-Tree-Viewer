editor = CodeMirror.fromTextArea(document.getElementById("sourceCode"), {
	lineNumbers : true,
	matchBrackets : true,
	theme : "prolog"
});


$('#optionsButton').click(() => {
	$('#modalOptions').modal('show');
});

$('#nextStepButton').click(() => {
	document.getElementById("prevStepButton").disabled = false;
	
	$.get('avanzarFotograma')
		.then(fotActual => {
			$('#mySpan').text(fotActual);
			return $.get('getNodos');
		})
		.then(nodos => {
			console.log('nodos: ', nodos);
			return $.get('getRamas');
		})
		.then(ramas => {
			console.log('ramas: ', ramas);
			return $.get('getRamasCut');
		})
		.then(ramasCut => {
			console.log('ramasCut: ', ramasCut);
		});

});


$('#program').submit(
		function(evento) {

			var s = "sourceCode=" + editor.getValue() + "&queryProlog="
					+ $('#queryProlog').val()
			console.log('-> ', s);

			$.post('', s, (nodos => {
				console.log('nodos en crearSLD: ', nodos);
			}));

			document.getElementById("createButton").style.display = "none";
			document.getElementById("optionsButton").style.display = "none";
			document.getElementById("nextButton").style.display = "block";
			document.getElementById("stopButton").style.display = "block";

			document.getElementById("prevStepButton").style.display = "block";
			document.getElementById("nextStepButton").style.display = "block";
			document.getElementById("skipButton").style.display = "block";
			document.getElementById("prevStepButton").disabled = true;

			evento.preventDefault();
		}
);


var s = {
			chart: {
				container: "#sldtree",
				scrollbar: "fancy",
				//animateOnInit: true,
				
				animation: {
					nodeAnimation: "easeOutBounce",
					nodeSpeed: 700,
					connectorsAnimation: "bounce",
					connectorsSpeed: 700
				},
				
				callback: {		
					onCreateNode: function(newNode, newNodeDOM) {
						var redraw = false;
						var esperando = false;
						
						$(newNodeDOM.querySelector('p.node-name')).hover(function () {
							var self = this;
							esperando = true;	
							
							setTimeout(function() {
								if(esperando){
									var wOrig = $(self).width();
									
									$(self).css({'overflow' : 'visible'});
									$(self).css({'max-width' : 'fit-content'});
									
									if($(self).width() > wOrig){
										newNode.width = $(self).width();
										newNode.getTree().redraw();
										redraw = true;
									}
									
									esperando = false;
								}
							}, Math.max(newNode.getTree().CONFIG.animation.nodeSpeed, newNode.getTree().CONFIG.animation.connectorsSpeed));
								

						}, function () {
							if(!esperando){
								$(this).css({'overflow' : 'hidden'});
								$(this).css({'max-width' : '100px'});
								
								if(redraw){
									newNode.width = $(this).width();
									newNode.getTree().redraw();
									redraw = false;
								}
								
								hayCambios = false;
							}
							
							esperando = false;
						});
					},
			
				},
				
				connectors: {
					type: 'straight'
				},
				
				node: {
					collapsable: false
				},
				
			},
			
			nodeStructure: {
				text: { name: "Parent node" },
				HTMLid: 'idNodo0',
				children: [
					{
						text: { name: "First child" },
						HTMLid: 'idNodo1',
						children: [
							{
								text: {name: "Tercero"},
								HTMLid: 'idNodo3',
							}
						]
					},
					{
						text: { name: "Second child" },
						HTMLid: 'idNodo2',
	
					}
				]
			},
		};

var t = new Treant(s, null, $);

var nuevoNodo = {
					text: {
						name: "Hola Hola Hola"
					},
					
					HTMLid: "idnodo4",
				};

var nodoInvisible = {
		text: {
			name: "______"
		},
		HTMLclass: "node_invisible",
		HTMLid: "nodeid"
};
var nodoPadre = t.tree.nodeDB.db[1];


function addNode(){
	var newNode = t.tree.addNode(nodoPadre, nuevoNodo);
	
}


//t.tree.addNode(nodoPadre, nuevoNodo);

/*
 * obtener ubicacion de rama -> sea N el nodo hijo de la rama:
 * M estará en la posición: n.parent().connectorPoint(true);
 * L estará en la posición: n.connectorPoint(false);
 * 
 * Siendo M y L elemento del path en SVG html.

*/

// t.tree.connectionStore[3].attr("stroke", "red"); permite cambiar el color de una rama a rojo. 
// Esto lo podemos usar a la hora de hacer un cut a una rama y querer cambiar su color a gris.

/*
 * Formas de hacer un nodo visible:
 * 	- Sabiendo su HTMLid, lo buscamos en el DOM y seteamos className.
 *  - Guardando una referencia al TreeNode, de esta manera si n fuera el TreeNode haríamos:
 *  	n.nodeDOM.className = ...
 *  
 *  Para cambiar el texto necesitamos una referencia al nodo, o nuevamente un id para buscarlo en el tree.
 *  En este último caso usariamos: t.tree.nodeDB.db[id], si el id es igual al indice del arreglo o buscaríamos elemento a elemento.
 * 
 *  Para agregar o remover las css clases mejor usar jquery:  $(this).addClass('className'),  $(this).removeClass('className');
 * 
*/
