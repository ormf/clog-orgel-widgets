
function slider(elem, config){
//    var barColor      = config.barColor || 'transparent';
//    var thumbColor    = config.thumbColor || 'black';
    var thumb         = config.thumb || 'true';

    var slider;
    if (elem.nodeType == undefined)
        slider = elem.get(0);
    else
        slider = elem;


    var sliderBar = document.createElement("div");
    slider.appendChild(sliderBar);

    const valChangeEvent = new Event("valuechange");
    const pxRegex = /([0-9]+).*/
    var offsetTop = slider.offsetTop;
    var offsetLeft = slider.offsetLeft;
    var sliderHeight;
    var sliderWidth;
    var colors;
    var minValue;
    var maxValue;
    var value;
    var mapping;
    var direction;
    var clipZero = slider.getAttribute('data-clip-zero');
    var valFunction;
    var valFunctionRev;
    var valueRange;
    var valueRatio;
    var valueLogRatio;
    var calcBarSize;
    var mouseMoveListener;

    var style = window.getComputedStyle(slider, null);
    var thumbColor = style.getPropertyValue('--thumb-color');
    var barColor = style.getPropertyValue('--bar-color');

    function clamp(number, min, max) {
        return Math.max(min, Math.min(number, max));
    }

    function setSliderBarStyle () {
        sliderBar.style.position = 'absolute';
        sliderBar.style.backgroundColor = barColor;
        sliderBar.style.border = 'none';
        sliderBar.style.borderRadius = 'inherit';
    }
    
    function setMinMaxMapping () {
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
            valueLogRatio = Math.log(valueRatio);
            valFunction = logVal;
            valFunctionRev = logValRev;
        }
        else { // linear mapping
            valueRange = maxValue-minValue;
            valFunction = linVal;
            valFunctionRev = linValRev;
        }
        setSliderValue();
        setDirection();
    }
    
    function setSliderValue () {
        if (maxValue >= minValue)
            value = clamp(parseFloat((slider.getAttribute("data-value")) || 0.0 ), minValue, maxValue);
        else
            value = clamp(parseFloat((slider.getAttribute("data-value")) || 0.0 ), maxValue, minValue);
    }
    
    function getValFraction (val) {
        return ((val - minValue) / valueRange);
    }

    function getYFraction (clientY) {
        let localYFrac = (sliderHeight + slider.offsetTop - clientY) / sliderHeight;
        return clamp(localYFrac, 0, 1);
    }

    function getXFraction (clientX) {
        let localXFrac = ((clientX - slider.offsetLeft)) / sliderWidth;
        return clamp(localXFrac, 0, 1);
    }

    function calcBarHeight (YFraction) {
        sliderBar.style.height = (YFraction * (sliderHeight - thumbWidth)) + 'px';
    }

    function calcBarHeightRev (YFraction) {
        sliderBar.style.height = ((1 - YFraction) * (sliderHeight - thumbWidth)) + 'px';
    }

    function calcBarWidth (YFraction) {
        sliderBar.style.width = (YFraction * (sliderWidth - thumbWidth)) + 'px';
    }

    function calcBarWidthRev (YFraction) {
        sliderBar.style.width = ((1 - YFraction) * (sliderWidth - thumbWidth)) + 'px';
    }

    function setDirection () {
        direction = slider.getAttribute("data-direction") || 'up';
        thumbWidth = 0.5; // will get reset below in case thumb == 'true';
        switch (direction) {
        case 'right':
            sliderBar.style.height = '100%';
            sliderBar.style.width = (getValFraction(value) * sliderWidth) + 'px';
            sliderBar.style.left = '0px';
            sliderBar.style.right = '';
            sliderBar.style.top = '';
            sliderBar.style.bottom = '0px';
            if (thumb == 'true') {
                thumbWidth = 1.5;
                sliderBar.style.borderLeft = 'none';
                sliderBar.style.borderRight = (sliderHeight/41) + 'px solid ' + thumbColor;
                sliderBar.style.borderTop = 'none';
                sliderBar.style.borderBottom = 'none';
            }
            calcBarSize = calcBarWidth;
            mouseMoveListener = mouseMoveListenerX;
            break;
        case 'left':
            sliderBar.style.height = '100%';
            sliderBar.style.width = (getValFraction(value) * sliderWidth) + 'px';
            sliderBar.style.left = '';
            sliderBar.style.right = '0px';
            sliderBar.style.top = '';
            sliderBar.style.bottom = '0px';
            if (thumb == 'true') {
                thumbWidth = 1.5;
                sliderBar.style.borderLeft = (sliderHeight/41) + 'px solid ' + thumbColor;
                sliderBar.style.borderRight = 'none';
                sliderBar.style.borderTop = 'none';
                sliderBar.style.borderBottom = 'none';
            }
            calcBarSize = calcBarWidthRev;
            mouseMoveListener = mouseMoveListenerX;
            break;
        case 'down':
            sliderBar.style.width = '100%';
            sliderBar.style.height = (getValFraction(value) * sliderHeight) + 'px';
            sliderBar.style.left = '0px';
            sliderBar.style.right = '';
            sliderBar.style.top = '0px';
            sliderBar.style.bottom = '';
            if (thumb == 'true') {
                thumbWidth = 1.5;
                sliderBar.style.borderLeft = 'none';
                sliderBar.style.borderRight = 'none';
                sliderBar.style.borderTop = 'none';
                sliderBar.style.borderBottom = (sliderHeight/41) + 'px solid ' + thumbColor;
            }
            calcBarSize = calcBarHeightRev;
            mouseMoveListener = mouseMoveListenerY;
            break;
        default: // 'up'
            sliderBar.style.width = '100%';
            sliderBar.style.height = (getValFraction(value) * sliderHeight) + 'px';
            sliderBar.style.left = '0px';
            sliderBar.style.right = '';
            sliderBar.style.top = '';
            sliderBar.style.bottom = '0px';
            if (thumb == 'true') {
                thumbWidth = 1.5;
                sliderBar.style.borderLeft = 'none';
                sliderBar.style.borderRight = 'none';
                sliderBar.style.borderTop = (sliderHeight/41) + 'px solid ' + thumbColor;
                sliderBar.style.borderBottom = 'none';
            }
            calcBarSize = calcBarHeight;
            mouseMoveListener = mouseMoveListenerY;
        }
    }

    var moved = false;
    var oldFraction = -1;
    
    function mouseDownListener (event) {
        moved = false;
        mouseMoveListener(event);
        document.addEventListener('mousemove', mouseMoveListener);
        document.addEventListener('mouseup', mouseUpListener);
    }

    function linVal (frac) {
        return (minValue + (frac * valueRange));
    }

    function logVal (frac) {
        if ((frac == 0) && (clipZero == 'true')) {
            return 0;
        }
        else {
            return (minValue * Math.pow(valueRatio, frac));
        }
    }

    function linValRev (val) {
        return (minValue + ((1 - frac) * valueRange));
    }

    function logValRev (val) {
        if ((frac == 0) && (clipZero == 'true')) {
            return 0;
        }
        else {
            return (minValue * Math.pow(valueRatio, (1-frac)));
        }
    }

    function mouseMoveListenerY (event) {
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

    slider.setBarSize = function (fraction) {
        let newValue = valFunction(fraction);
        calcBarSize(fraction);
        slider.setAttribute('data-value', newValue);
        return newValue;
    }
    
    function mouseMoveListenerX (event) {
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

    function mouseUpListener (event){
        document.removeEventListener('mousemove', mouseMoveListener);
        document.removeEventListener('mouseup', mouseUpListener);
    }

    slider.removeMouseDownListener = function () {
        slider.removeEventListener('mousedown', mouseDownListener);
    }

    slider.dispatchValChangeEvent = function () {
//        console.log('value changed');
        slider.dispatchEvent(valChangeEvent);
    }


    function initSlider () {
        setSliderBarStyle();
        clipZero = slider.getAttribute("data-clip-zero") || 'false';
        sliderHeight = parseFloat(style.height.match(pxRegex)[1]);
        sliderWidth = parseFloat(style.width.match(pxRegex)[1]);
        setMinMaxMapping(sliderBar);
        slider.addEventListener('mousedown', mouseDownListener)
    }

    initSlider();
}
