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
			<div class="form-group col-lg-6 h-100">
				<label>Source code</label>
				<form:form cssClass="h-100" modelAttribute="programaUsuario" autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false">
					<form:textarea path="sourceCode" id="sourceCode" />
					<form:errors path="sourceCode" cssClass="errorCode" />

					<div class="input-group mb-2">
						<div class="input-group-prepend" style="margin-right: -9px;">
							<div class="input-group-text">?-</div>
						</div>
						<form:input path="queryProlog" id="queryProlog"
							placeholder="example(X)." cssClass="form-control"
							style="font-style: italic;" />
					</div>

					<form:errors path="queryProlog" cssClass="errorCode" />

					<div class="btn-group col-lg-12" style="margin-bottom: 15px; padding-left: 0px; padding-right: 0px;" role="group">
						<input class="btn btn-primary" type="submit" name="createSLD" value="Create SLD" id="createButton" onclick="crearSLD();" style="display: block;"/>
						<input class="btn btn-primary" type="submit" name="next" value="Next" id="nextButton" style="display: none;" />
						<input class="btn btn-primary" type="submit" name="stop" value="Stop" id="stopButton" style="display: none;" />
						<button class="btn btn-primary" type="button" name="options" onclick="showOptions();" id="optionsButton" style="display: block;">Options</button>
					</div>
					
				<script>
					function showOptions() {
						$('#modalOptions').modal('show');
					}
					
/* 					document.getElementById("createButton").style.display = "block";
					document.getElementById("optionsButton").style.display = "block";
					document.getElementById("nextButton").style.display = "none";
					document.getElementById("stopButton").style.display = "none"; */
				</script>


				<!-- Modal -->
				<div class="modal fade" id="modalOptions" tabindex="-1" role="dialog" aria-hidden="true">
					<div class="modal-dialog modal-dialog-centered modal-sm" role="document">
						<div class="modal-content">
							<div class="modal-header">
								<h5 class="modal-title">Opciones</h5>
								<button type="button" class="close" data-dismiss="modal" aria-label="Close">
									<span aria-hidden="true">&times;</span>
								</button>
							</div>
							<div class="modal-body">
								<div class="form-check">
									<form:checkbox path="occurs_check" id="occurs_check"/>
									<label class="form-check-label" for="occurs_check">
									  Occurs check
									</label>
								</div>
								<div class="form-check">
									<form:checkbox path="show_substitutions" id="show_substitutions"/>
									<label class="form-check-label" for="show_substitutions">
									  View substitutions
									</label>
								</div>
							</div>
<!-- 							<div class="modal-footer">
								<button type="button" class="btn btn-secondary" data-dismiss="modal">Close</button>
								<button type="button" class="btn btn-primary">Save changes</button>
							</div> -->
						</div>
					</div>
				</div>
				
				</form:form>



				<script>
					var editor = CodeMirror.fromTextArea(document.getElementById("sourceCode"), {
						lineNumbers : true,
						matchBrackets : true,
						theme : "prolog"
					});
				</script>
			</div>

			<div class="form-group col-lg-6 h-100">
				<label>SLDTree</label>
				<div class="form-control rounded-10 h-100" id="sldtree" style="margin-top: 0px; margin-bottom: 10px; overflow: auto;">
					${svgTreeSLD}
				</div>
				
				<form:form>
					<div class="btn-group col-lg-12" style="margin-bottom: 15px; padding-left: 0px; padding-right: 0px;" role="group">
						<button class="btn btn-primary" type="submit" name="prevStep" id="prevStepButton">Prev step</button>
						<button class="btn btn-primary" type="submit" name="nextStep" id="nextStepButton" onclick="avanzarSLD();">Next step</button>
						<button class="btn btn-primary" type="submit" name="skip" id="skipButton">Skip</button>
					</div>
						
<!-- 					<script>				
						document.getElementById("prevStepButton").style.display = "none";
						document.getElementById("nextStepButton").style.display = "none";
						document.getElementById("skipButton").style.display = "none";
					</script> -->
				</form:form>	
			</div>
			
			<% if(request.getAttribute("svgTreeSLD") == null){ %>
				<script type="text/javascript">
					document.getElementById("createButton").style.display = "block";
					document.getElementById("optionsButton").style.display = "block";
					document.getElementById("nextButton").style.display = "none";
					document.getElementById("stopButton").style.display = "none";
					
					document.getElementById("prevStepButton").style.display = "none";
					document.getElementById("nextStepButton").style.display = "none";
					document.getElementById("skipButton").style.display = "none";
				</script>
			<% } else { %>
				<script type="text/javascript">
					document.getElementById("createButton").style.display = "none";
					document.getElementById("optionsButton").style.display = "none";
					document.getElementById("nextButton").style.display = "block";
					document.getElementById("stopButton").style.display = "block";
					
					document.getElementById("prevStepButton").style.display = "block";
					document.getElementById("nextStepButton").style.display = "block";
					document.getElementById("skipButton").style.display = "block";		
				</script>
				
				<% if(request.getAttribute("seUsoNext") != null){ %>
					<script>
						document.getElementById("prevStepButton").disabled = false;	
					</script>
				<% } else { %>
					<script>
						document.getElementById("prevStepButton").disabled = true;
					</script>
				<% } %>
			<% } %>
			

			<script type="text/javascript">
				function crearSLD() {
					document.getElementById("createButton").style.display = "none";
					document.getElementById("optionsButton").style.display = "none";
					document.getElementById("nextButton").style.display = "block";
					document.getElementById("stopButton").style.display = "block";
					
					document.getElementById("prevStepButton").style.display = "block";
					document.getElementById("nextStepButton").style.display = "block";
					document.getElementById("skipButton").style.display = "block";
					document.getElementById("prevStepButton").disabled = true;
				}
			
				function avanzarSLD() {
					document.getElementById("prevStepButton").disabled = false;
				}
			</script>
		</div>

	</div>


	<script src="https://code.jquery.com/jquery-3.3.1.slim.min.js"
		integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo"
		crossorigin="anonymous"></script>

	<script
		src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js"
		integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM"
		crossorigin="anonymous"></script>
		
	<script type="text/javascript">
		window.onload = function(){
			document.getElementById("loadDiv").style.display = "none";
		}
	</script>
</body>
</html>