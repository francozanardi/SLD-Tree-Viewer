<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
	pageEncoding="ISO-8859-1"%>
<%@ taglib uri="http://www.springframework.org/tags" prefix="spring"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<!DOCTYPE html>
<html>
<head>
<spring:url value="/resources" var="urlPublic"></spring:url>

<link rel="stylesheet" href="${urlPublic}/codemirror/lib/codemirror.css">
<link rel="stylesheet" href="${urlPublic}/codemirror/mode/prolog/prolog.css">
<link rel="stylesheet" href="${urlPublic}/css/style.css">


<!-- Evaluar la posiblidad de usar require.js para organizar las depedencias e intentar acelerar la velocidad de carga.
Además minimizar los archivos js utilizados, especialmente codemirror.js. Mirar como swish utiliza requirejs. -->
<script src="${urlPublic}/codemirror/lib/codemirror.js"></script>
<script src="${urlPublic}/codemirror/addon/edit/matchbrackets.js"></script>
<script src="${urlPublic}/codemirror/addon/mode/simple.js"></script>
<script src="${urlPublic}/codemirror/mode/prolog/prolog.js"></script>

<link rel="stylesheet"
	href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
	integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"
	crossorigin="anonymous"> <!-- Al ponerlo abajo del todo pareciera que la página carga más ordenada. -->
	
<meta charset="ISO-8859-1">
<title>SLD Tree Generator</title>
</head>

<body>
	<div id="loadDiv" style="display: block; background-color:white; position:absolute; top:0px; left:0px; width:100%; height:100%; z-index:2000;"></div>
	<div class="container h-100">
		<h1>Bienvenidos.</h1>
		<div class="row h-100">
			<div class="form-group col-lg-6 h-100" style="margin-bottom: 10rem;">
				<label>Source code</label>
				<form id='program' action="#" class="h-100" autocomplete="off" spellcheck="false">
					<textarea id="str"></textarea>
					<%-- <form:errors path="sourceCode" cssClass="errorCode" /> --%>

					<%--  <form:errors path="queryProlog" cssClass="errorCode" /> --%>

					<div class="btn-group col-lg-12" style="margin-bottom: 15px; padding-left: 0px; padding-right: 0px;" role="group">
						<input class="btn btn-primary" type="submit" value="Create SLD" id="createButton" style="display: block;"/>
					</div>
				</form>


			</div>

			<div class="form-group col-lg-6 h-100">
				<label>SLDTree</label>
				<div class="form-control rounded-10 h-100" id="sldtree" style="margin-top: 0px; margin-bottom: 10px; overflow: auto;">

				</div>
				
					<div class="btn-group col-lg-12" style="margin-bottom: 15px; padding-left: 0px; padding-right: 0px;" role="group">
						<button class="btn btn-primary" type="submit" name="prevStep" id="prevStepButton">Prev step</button>
						<button class="btn btn-primary" type="submit" name="nextStep" id="nextStepButton" onclick="avanzarSLD();">Next step</button>
						<button class="btn btn-primary" type="submit" name="skip" id="skipButton">Skip</button>
					</div>
						
					<script>				
						document.getElementById("prevStepButton").style.display = "none";
						document.getElementById("nextStepButton").style.display = "none";
						document.getElementById("skipButton").style.display = "none";
					</script>
				<span id="mySpan"></span>	
			</div>
			

		</div>

	</div>


	<script src="https://code.jquery.com/jquery-3.3.1.min.js"></script>

	<script
		src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js"
		integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM"
		crossorigin="anonymous"></script>
		
	<script type="text/javascript">
		window.onload = function(){
			document.getElementById("loadDiv").style.display = "none";
		}
	</script>
	
	
			<script type="text/javascript">		
				$('#program').submit(function(evento) {
					
					 $.ajax({
			            url: 'q',
			            dataType: 'json',
			            method: 'POST',
			            data: JSON.stringify({
			            	str: 'asd'
			            	}),
			           	processData: false,
			           	contentType: "application/json"
			        });
					
					//$.post('');
			      				
					evento.preventDefault(); 
			    });
			
				function avanzarSLD() {
					document.getElementById("prevStepButton").disabled = false;
					
					
					$('#mySpan').load('nextStep', 'param1= Hola mundo');
				}
			</script>
</body>
</html>