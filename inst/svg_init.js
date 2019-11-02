

$(document).on('shiny:connected', function(event) {
  $(document).on("dblclick", ".grViz svg", function(){
    svgPanZoom(".grViz svg");
  });
  
  $(window).on('resize', function(){
    var inst = svgPanZoom(".grViz svg");
    inst.resize(); // update SVG cached size and controls positions
    inst.fit();
    inst.center();
  });

});


// Shortcuts Functions
$(document).on("keyup", function(e) {
  console.log("e")
  console.log(e)
  var ekey = e.keyCode;
  console.log("e.keyCode: " + e.keyCode)
  console.log("e.ctrlKey && e.ctrlKey: " + e.ctrlKey && e.ctrlKey)
  
  var panZoomTiger = svgPanZoom('.grViz svg');
  
  switch(ekey){
    // Zoom IN ("+")
    case 171: {
      panZoomTiger.zoomBy(1.1);
      break;
    }
     // Zoom Out ("-")
    case 173: {
      panZoomTiger.zoomBy(0.9);
      break;
    }
    // Center and Fit (CTRL + Backspace)
    case 8: {
      if (e.ctrlKey && e.ctrlKey === true) {
        panZoomTiger.resize(); // update SVG cached size and controls positions
        panZoomTiger.fit();
        panZoomTiger.center();
      }
      break;
    }
    // Pan Left (left arrow)
    case 37: {
      panZoomTiger.panBy({x: -50, y: 0});
      break;
    }
    // Pan Right (right arrow)
    case 39: {
      panZoomTiger.panBy({x: 50, y: 0});
      break;
    }
    // Pan Up (up arrow)
    case 38: {
      panZoomTiger.panBy({x: 0, y: -50});
      break;
    }
    // Pan Down (down arrow)
    case 40: {
      panZoomTiger.panBy({x: 0, y: 50});
      break;
    }
  }

});
