editor = CodeMirror.fromTextArea(document.getElementById("sourceCode"), {
	lineNumbers : true,
	matchBrackets : true,
	theme : "prolog"
});


$('#optionsButton').click(() => {
	$('#modalOptions').modal('show');
});

$('#nextButton').click(() => {
	document.getElementById("prevStepButton").disabled = false;

	$('#mySpan').load('nextStep', 'param1= Hola mundo');
});


$('#program').submit(
		function(evento) {

			var s = "sourceCode=" + editor.getValue() + "&queryProlog="
					+ $('#queryProlog').val()
			console.log('-> ', s);

			$.post('', s);

			document.getElementById("createButton").style.display = "none";
			document.getElementById("optionsButton").style.display = "none";
			document.getElementById("nextButton").style.display = "block";
			document.getElementById("stopButton").style.display = "block";

			document.getElementById("prevStepButton").style.display = "block";
			document.getElementById("nextStepButton").style.display = "block";
			document.getElementById("skipButton").style.display = "block";
			document.getElementById("prevStepButton").disabled = true;

			evento.preventDefault();
		});
