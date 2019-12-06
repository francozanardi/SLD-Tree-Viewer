var myID = null;
var tree;
var mapNodos = new Map(); //mapeo desde id nodo a objeto TreeNode
var mapRamasDisponibles = new Map();
//mapeo que contiene solo las ramas sin nodos hijos,
//es decir el mapeo va desde id rama a objeto TreeNode, el cual es un nodo invisible.

var editor = CodeMirror.fromTextArea(document.getElementById("sourceCode"), {
	lineNumbers : true,
	matchBrackets : true,
	theme : "prolog"
});

window.onbeforeunload = function(event) {
	if(myID){
		$.post('eliminarArbol', 'id='+myID);
		
		event.returnValue = "";
	}
}



$('#optionsButton').click(() => {
	$('#modalOptions').modal('show');
});

$('#stopButton').click(() => {
	$.post('eliminarArbol', 'id='+myID, () => {
		document.getElementById("createButton").style.display = "block";
		document.getElementById("optionsButton").style.display = "block";
		document.getElementById("nextButton").style.display = "none";
		document.getElementById("stopButton").style.display = "none";

		document.getElementById("prevStepButton").style.display = "none";
		document.getElementById("nextStepButton").style.display = "none";
		document.getElementById("skipButton").style.display = "none";
		document.getElementById("prevStepButton").disabled = true;
		
		mapRamasDisponibles.clear();
		mapNodos.clear();
		
		myID = null;
	});
});

$('#nextButton').click(() => {

	$('#nextButton').attr("disabled", true);
	
	$.post('avanzarFotograma', 'id='+myID)
	.then(fotActual => {
		return $.post('getNodos', 'id='+myID);
	})
	.then(nodos => {
		graficarNodos(nodos);
		
		if(nodos[0].rotulo === "[]" || nodos[0].rotulo === "[fail]"){ //es la cláusula vacía o falló
			if(mapRamasDisponibles.size > 0){
				$('#nextButton').attr("disabled", false);
			}
			$('#nextStepButton').attr("disabled", true);
		} else {
			$('#nextStepButton').attr("disabled", false);
		}
	});
	
	
});

$('#skipButton').click(() => {
	var elem = document.getElementById('sldtree');
	
	if (elem.requestFullscreen) {
		elem.requestFullscreen();
	} else if (elem.mozRequestFullScreen) { /* Firefox */
		elem.mozRequestFullScreen();
	} else if (elem.webkitRequestFullscreen) { /* Chrome, Safari & Opera */
		elem.webkitRequestFullscreen();
	} else if (elem.msRequestFullscreen) { /* IE/Edge */
		elem.msRequestFullscreen();
	}
	
	setTimeout(function() {
		tree.tree.redraw();
	}, 500);
	
});



$('#nextStepButton').click(() => {
	document.getElementById("prevStepButton").disabled = false;
	
	$.post('avanzarFotograma', 'id='+myID)
		.then(fotActual => {
			$('#mySpan').text(fotActual);
			return $.post('getRamas', 'id='+myID);
		})
		.then(ramas => {
			console.log('ramas: ', ramas);
			
			graficarRamas(ramas);
			return $.post('getRamasCut', 'id='+myID);
		})
		.then(ramasCut => {
			console.log('ramasCut: ', ramasCut);
			
			graficarRamasCut(ramasCut);
			return $.post('getNodos', 'id='+myID);
		})
		.then(nodos => {
			console.log('nodos: ', nodos);
			
			graficarNodos(nodos);
			
			if(nodos.length == 1 && nodos[0].rotulo === "[]"){ //es la cláusula vacía
				document.getElementById("nextStepButton").disabled = true;
				
				if(mapRamasDisponibles.size > 0){
					document.getElementById("nextButton").disabled = false;
				}
			} else if(nodos.length == 1 && nodos[0].rotulo === "[fail]" && mapRamasDisponibles.size == 0){
				document.getElementById("nextStepButton").disabled = true;
			}
		});

});


$('#program').submit(
		function(evento) {

			var s = "sourceCode=" + encodeURIComponent(editor.getValue()) + "&queryProlog="
					+ encodeURIComponent($('#queryProlog').val());

			$.post('', s)
				.then(id => {
					myID = id;
					
					return $.post('getRaiz', 'id='+myID);
				})
				.then(raiz => {
					crearArbol(raiz);
					
					crearButtonFullScreen();
				})
			

			document.getElementById("createButton").style.display = "none";
			document.getElementById("optionsButton").style.display = "none";
			document.getElementById("nextButton").style.display = "block";
			document.getElementById("stopButton").style.display = "block";

			document.getElementById("prevStepButton").style.display = "block";
			document.getElementById("nextStepButton").style.display = "block";
			document.getElementById("skipButton").style.display = "block";
			
			document.getElementById("prevStepButton").disabled = true;
			document.getElementById("nextButton").disabled = true;
			document.getElementById("nextStepButton").disabled = false;
			

			evento.preventDefault();
		}
);


function crearArbol(nodo){
	console.log('nodo.rotulo: ', nodo.rotulo);
	
	var config = {
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
									var wOrig = $(self).outerWidth();
									
									$(self).css({'overflow' : 'visible'});
									$(self).css({'max-width' : 'fit-content'});
									
									if($(self).width() > wOrig){
										newNode.width = $(self).outerWidth();
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
									newNode.width = $(this).outerWidth();
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
					text: {
						name: nodo.rotulo
					},
					HTMLid: 'nodo_' + nodo.id
			}
			
		};
	
	tree = new Treant(config, null, $);
	
	mapNodos.set(0, tree.tree.nodeDB.db[0]);
	
}

function graficarRamas(ramas){
	for(let i in ramas){
		let rama = ramas[i];
		
		let nodeInvisible = {
				text: {
					name: "IIIIIII"
				},
				HTMLclass: "nodo_invisible",
				HTMLid: "rama_" + rama.id
		};
				
		let nuevoNodoInvisible = tree.tree.addNode(mapNodos.get(rama.padre), nodeInvisible);
		
		mapRamasDisponibles.set(rama.id, nuevoNodoInvisible);
	}
}

function graficarNodos(nodos){
	for(let i in nodos){
		let nodo = nodos[i];
		
		let nodoInvisible = mapRamasDisponibles.get(nodo.idRama);
		
		$(nodoInvisible.nodeDOM).removeClass('nodo_invisible'); //todo: hay que cambiar el htmlID al que corresponda
		/*
		 * La siguiente instrucción se debe a que el quitarle la clase 'nodo_invisible' puede modificar la altura del 'div'
		 * (si se agregase borde al remover esta clase, por ejemplo).
		 * El hecho de que se llame antes de setTextName, es que sabemos que este método ejecutará un redraw(), por lo tanto evitamos
		 * ejecutarlos nosotros para actualizar el cambio en la altura.
		 */
		
		nodoInvisible.height = nodoInvisible.nodeDOM.offsetHeight+1;
		nodoInvisible.setTextName(nodo.rotulo);
		
		mapRamasDisponibles.delete(nodo.idRama);
		mapNodos.set(nodo.id, nodoInvisible);
	}
}

function graficarRamasCut(ramas){
	for(let i in ramas){
		let rama = ramas[i];
		
		mapRamasDisponibles.get(rama.id).cutRama();
		mapRamasDisponibles.delete(rama.id);
	}
}

function crearButtonFullScreen(){
	
	//si todos son nulls entonces no está en fullscreen.
	function isFullScreenOn() {
		return document.fullScreenElement || document.webkitFullscreenElement || document.msFullscreenElement || document.mozFullScreenElement;
	}
	
	function acomodarTree() {
		setTimeout(function() {
			tree.tree.redraw();
		}, 500);
	}
	
	function configExitFullScreen(){
		
		function exitFullScreen() {
			if(!isFullScreenOn()){ 
				btn.style.cssText = "display: block;";
				
				acomodarTree();
			}
		} 
		
		/* Standard syntax */
		document.addEventListener("fullscreenchange", exitFullScreen);
		/* Firefox */
		document.addEventListener("mozfullscreenchange", exitFullScreen);
		/* Chrome, Safari and Opera */
		document.addEventListener("webkitfullscreenchange",  exitFullScreen);
		/* IE / Edge */
		document.addEventListener("msfullscreenchange",  exitFullScreen);
		
	}
	
	function addListnerToFullScreen(){
		btn.addEventListener("click", () => {
			btn.style.cssText = "display: none;";
			
			if (sld.requestFullscreen) {
				sld.requestFullscreen();
			} else if (sld.mozRequestFullScreen) { /* Firefox */
				sld.mozRequestFullScreen();
			} else if (sld.webkitRequestFullscreen) { /* Chrome, Safari & Opera */
				sld.webkitRequestFullscreen();
			} else if (sld.msRequestFullscreen) { /* IE/Edge */
				sld.msRequestFullscreen();
			}
			
			acomodarTree();
		});
	}
	
	function createButton(){
		btn = document.createElement("button");
		btn.className = "fullScreen";
		btn.id = "fullScreenButton";
		
		svg = document.createElement("img");
		svg.src = "resources/img/fullscreen.svg";
		
		btn.appendChild(svg);
		
		sld = document.getElementById("sldtree");
		sld.appendChild(btn);
		

		$(sld).hover(() => {
			if(!isFullScreenOn()){
				btn.style.cssText = "display: block;";
			}
		}, () => {
			btn.style.cssText = "display: none;";
		});
	}
	
	var btn, svg, sld;
	
	createButton();
	addListnerToFullScreen();
	configExitFullScreen();
	
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
