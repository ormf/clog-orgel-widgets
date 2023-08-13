function slider(elem, config){
    var barColor      = config.barColor || 'transparent';
    var thumbColor    = config.thumbColor || 'black';
    var minValue      = config.minValue || 0.0;
    var maxValue      = config.maxValue || 1.0;
    var value         = config.value || 0.0;
    var mapping       = config.mapping || 'lin';
    var orientation   = config.orientation || 'vertical';

    var colors = [];
    var slider = elem.get(0);

    var pxRegex = /([0-9]+).*/
    var containerHeight = slider.style.height.match(pxRegex)[1];
    var containerWidth = slider.style.width.match(pxRegex[1]);

    var valueRange = maxValue-minValue;
    
    function getValFraction (val) {
        return((val - minValue) / valueRange);
    }

    var sliderBar = document.createElement("div");
    sliderBar.style.position = 'absolute';
    slider.appendChild(sliderBar);
    sliderBar.style.background = barColor;
    sliderBar.style.border = 'none';
    sliderBar.style.bottom = '0px';
    sliderBar.style.left = '0px';
    
    if (orientation == 'vertical') {
        sliderBar.style.width = '100%';
        sliderBar.style.height = (getValFraction(value) * containerHeight) + 'px';
        sliderBar.style.borderTop = '2px solid ' + thumbColor;
    }
    else {
        sliderBar.style.height = '100%';
        sliderBar.style.width = (getValFraction(value) * containerWidth) + 'px';
        sliderBar.style.borderRight = '2px solid ' + thumbColor;
    }

//    console.log('sliderBarHeight ' + sliderBar.style.height);

    
    const mySetAttribute = slider.setAttribute;
    // override setAttribte
    slider.setAttribute = function (key, value) {
            //        console.log("--trace, key: " + key + ', value: ' + value);
            // use call, to set the context and prevent illegal invocation errors
            mySetAttribute.call(vuMeter, key, value);
            //        if (key == 'data-db') drawBoxes(c, value);
            // if (key == 'data-db') drawLed();

    }
    // Trigger the animation
//        drawLed();
}
