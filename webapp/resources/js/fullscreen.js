import {getCantidadNodos} from './graficador.js';

//si todos son nulls entonces no está en fullscreen.
function isFullScreenOn() {
    return document.fullScreenElement || document.webkitFullscreenElement || document.msFullscreenElement || document.mozFullScreenElement;
}

function acomodarTree() {
    setTimeout(function() {
        treant.tree.redraw();
    }, 500);
}

function configExitFullScreen(){
    
    function exitFullScreen() {
        if(!isFullScreenOn()){ 
            btnFullscreen.style.cssText = "display: block;";
            btnreport.style.cssText = "display: block;";
            viewTree.style.cssText = "height: 100%!important;";
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

function addListnerToFullscreen(){
    btnFullscreen.addEventListener("click", () => {
        btnFullscreen.style.cssText = "display: none;";
        btnreport.style.cssText = "display: none;";
        viewTree.style.cssText = "height: 90%!important;";

        var controlsAndView = document.getElementById('controlsAndViewTree');
        
        if (controlsAndView.requestFullscreen) {
            controlsAndView.requestFullscreen();
        } else if (controlsAndView.mozRequestFullScreen) { /* Firefox */
            controlsAndView.mozRequestFullScreen();
        } else if (controlsAndView.webkitRequestFullscreen) { /* Chrome, Safari & Opera */
            controlsAndView.webkitRequestFullscreen();
        } else if (controlsAndView.msRequestFullscreen) { /* IE/Edge */
            controlsAndView.msRequestFullscreen();
        }
        
        acomodarTree();
    });
}

function configHover(){
    $(viewTree).hover(() => {
        if(!isFullScreenOn() && getCantidadNodos() > 0){
            btnFullscreen.style.cssText = "display: block;";
            btnreport.style.cssText = "display: block;";
        }
    }, () => {
        btnFullscreen.style.cssText = "display: none;";
        btnreport.style.cssText = "display: none;";
    });
}

var btnFullscreen, btnreport, viewTree;

btnFullscreen = document.getElementById('fullScreenButton');
btnreport = document.getElementById('reportButton');
viewTree = document.getElementById('viewTree');


configHover();
addListnerToFullscreen();
configExitFullScreen();	