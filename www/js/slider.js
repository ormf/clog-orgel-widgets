const valChangeEvent = new Event("valuechange");

function slider(elem, config){
//    var barColor      = config.barColor || 'transparent';
//    var thumbColor    = config.thumbColor || 'black';
    var thumb         = config.thumb || 'true';


    var slider = elem.get(0);
    var sliderBar = document.createElement("div");
    slider.appendChild(sliderBar);

    
    const pxRegex = /([0-9]+).*/
    var offsetTop = slider.offsetTop;
    var offsetLeft = slider.offsetLeft;
    var sliderHeight;
    var sliderWidth;
    var colors = [];
    var minValue;
    var maxValue;
    var value;
    var mapping;
    var direction;
    var clipZero;
    var valFunction;
    var valueRange;
    var valueRatio;
    var calcBarSize;
    var moveListener;

    var style = window.getComputedStyle(slider, null);
    var thumbColor = style.getPropertyValue('--thumb-color');
    var barColor = style.getPropertyValue('--bar-color');
    
    function setSliderBarStyle () {
        sliderBar.style.position = 'absolute';
        sliderBar.style.background = barColor;
        sliderBar.style.border = 'none';
    }
    
    function setMinMaxMapping() {
        minValue      = parseFloat(slider.getAttribute("data-min")) || 0.0;
        maxValue      = parseFloat(slider.getAttribute("data-max")) || 1.0;
        mapping       = slider.getAttribute("data-mapping") || 'lin';
        if (mapping == 'log') {
            if ((minValue == 0) && (maxValue == 0)) {
                minValue = 0.01;
                maxValue = 1;
                slider.setAttribute('data-min', minvalue);
                slider.setAttribute('data-max', maxValue);
            }
            else {
                if (minValue == 0) {
                    minValue = maxValue / 100;
                    slider.setAttribute('data-min', minValue);
                }
                else {
                    if (maxValue == 0) {
                        maxValue = minValue / 100;
                        slider.setAttribute('data-max', maxValue);
                    }
                }
            }
            valueRatio = maxValue/minValue;
        }
        else {
            valueRange = maxValue-minValue;
        }
        setSliderValue();
        setDirection();
    }

    function setSliderValue () {
        value = parseFloat(slider.getAttribute("data-value")) || 0.0;
        if (maxValue > minValue) {
            if (value < minValue) { value = minvalue }
            else {
                if (value > maxValue) { value = maxvalue }
            }
        }
        else {
            if (value > minValue) { value = minvalue }
            else {
                if (value < maxValue) { value = maxvalue }
            }
        }
    }
    
    function setSliderHeightVal () {
        sliderHeight = parseFloat(slider.style.height.match(pxRegex)[1]);
    }

    function setSliderWidthVal () {
        sliderWidth = parseFloat(slider.style.width.match(pxRegex)[1]);
    }
    
    function getValFraction (val) {
        return ((val - minValue) / valueRange);
    }

    function getYFraction (clientY) {
        let localYFrac = (sliderHeight + slider.offsetTop - clientY) / sliderHeight;
        if (localYFrac < 0) localYFrac = 0;
        if (localYFrac > 1) localYFrac = 1;
        return localYFrac;
    }

    function getXFraction (clientX) {
        let localXFrac = ((clientX - slider.offsetLeft)) / sliderWidth;
        if (localXFrac < 0) localXFrac = 0;
        if (localXFrac > 1) localXFrac = 1;
        return localXFrac;
    }

    function calcBarHeightRev (YFraction) {
        sliderBar.style.height = ((1-YFraction) * (sliderHeight - 1.5)) + 'px';
    }

    function calcBarHeight (YFraction) {
        sliderBar.style.height = (YFraction * (sliderHeight - 1.5)) + 'px';
    }

    function calcBarWidthRev (YFraction) {
        sliderBar.style.width = ((1-YFraction) * (sliderWidth - 1.5)) + 'px';
    }

    function calcBarWidth (YFraction) {
        sliderBar.style.width = (YFraction * (sliderWidth - 1.5)) + 'px';
    }

    function setDirection () {
        direction = slider.getAttribute("data-direction") || 'up';
        switch (direction) {
        case 'right':
            sliderBar.style.height = '100%';
            sliderBar.style.width = (getValFraction(value) * sliderWidth) + 'px';
            sliderBar.style.left = '0px';
            sliderBar.style.right = '';
            sliderBar.style.top = '';
            sliderBar.style.bottom = '0px';
            if (thumb == 'true') {
                sliderBar.style.borderLeft = 'none';
                sliderBar.style.borderRight = (sliderHeight/41) + 'px solid ' + thumbColor;
                sliderBar.style.borderTop = 'none';
                sliderBar.style.borderBottom = 'none';
            }
            if (mapping == 'lin') valFunction = linVal;
            else valFunction = logVal;
            calcBarSize = calcBarWidth;
            moveListener = moveListenerX;
            break;
        case 'left':
            sliderBar.style.height = '100%';
            sliderBar.style.width = (getValFraction(value) * sliderWidth) + 'px';
            sliderBar.style.left = '';
            sliderBar.style.right = '0px';
            sliderBar.style.top = '';
            sliderBar.style.bottom = '0px';
            if (thumb == 'true') {
                sliderBar.style.borderLeft = (sliderHeight/41) + 'px solid ' + thumbColor;
                sliderBar.style.borderRight = 'none';
                sliderBar.style.borderTop = 'none';
                sliderBar.style.borderBottom = 'none';
            }
            if (mapping == 'lin') valFunction = linValRev;
            else valFunction = logValRev;
            calcBarSize = calcBarWidthRev;
            moveListener = moveListenerX;
            break;
        case 'down':
            sliderBar.style.width = '100%';
            sliderBar.style.height = (getValFraction(value) * sliderHeight) + 'px';
            sliderBar.style.left = '0px';
            sliderBar.style.right = '';
            sliderBar.style.top = '0px';
            sliderBar.style.bottom = '';
            if (thumb == 'true') {
                sliderBar.style.borderLeft = 'none';
                sliderBar.style.borderRight = 'none';
                sliderBar.style.borderTop = 'none';
                sliderBar.style.borderBottom = (sliderHeight/41) + 'px solid ' + thumbColor;
            }
            if (mapping == 'lin') valFunction = linValRev;
            else valFunction = logValRev;
            calcBarSize = calcBarHeightRev;
            moveListener = moveListenerY;
            break;
        default: // 'up'
            sliderBar.style.width = '100%';
            sliderBar.style.height = (getValFraction(value) * sliderHeight) + 'px';
            sliderBar.style.left = '0px';
            sliderBar.style.right = '';
            sliderBar.style.top = '';
            sliderBar.style.bottom = '0px';
            if (thumb == 'true') {
                sliderBar.style.borderLeft = 'none';
                sliderBar.style.borderRight = 'none';
                sliderBar.style.borderTop = (sliderHeight/41) + 'px solid ' + thumbColor;
                sliderBar.style.borderBottom = 'none';
            }
            if (mapping == 'lin') valFunction = linVal;
            else valFunction = logVal;
            calcBarSize = calcBarHeight;
            moveListener = moveListenerY;
        }
    }

    var moved = false;
    var oldFraction = -1;
    
    function downListener (event) {
        moved = false;
        moveListener(event);
        document.addEventListener('mousemove', moveListener);
        document.addEventListener('mouseup', upListener);
    }

    function linVal (Frac) {
        return (minValue + (Frac * valueRange));
    }

    function logVal (Frac) {
        if ((Frac == 0) && (clipZero == 'true')) {
            return 0;
        }
        else {
            return (minValue * Math.pow(valueRatio, Frac));
        }
    }

    function linValRev (Frac) {
        return (minValue + ((1 - Frac) * valueRange));
    }

    function logValRev (Frac) {
        if ((Frac == 0) && (clipZero == 'true')) {
            return 0;
        }
        else {
            return (minValue * Math.pow(valueRatio, (1-Frac)));
        }
    }

    function moveListenerY (event) {
        moved = true;
        let YFraction = getYFraction(event.clientY);
        if (YFraction != oldFraction) {
            let newValue = valFunction(YFraction);
            calcBarSize(YFraction);
            slider.setAttribute('data-value', newValue);
            slider.dispatchEvent(valChangeEvent);
            oldFraction = YFraction;
        }
    }

    function moveListenerX (event) {
        moved = true;
        let XFraction = getXFraction(event.clientX);
        if (XFraction != oldFraction) {
            let newValue = valFunction(XFraction);
            calcBarSize(XFraction);
            slider.setAttribute('data-value', newValue);
            slider.dispatchEvent(valChangeEvent);
            oldFraction = XFraction;
        }
    }

    
    function upListener (event){
        document.removeEventListener('mousemove', moveListener);
        document.removeEventListener('mouseup', upListener);
    }

    function initSlider () {
        setSliderBarStyle();
        clipZero = slider.getAttribute("data-clip-zero") || 'false';
        setSliderHeightVal();
        setSliderWidthVal();
        setMinMaxMapping(sliderBar);
        slider.addEventListener('mousedown', downListener)
    }

    initSlider();
}
