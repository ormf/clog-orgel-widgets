function multislider(elem, config){
//    var barColor      = config.barColor || 'transparent';
//    var thumbColor    = config.thumbColor || 'black';
    var thumb         = config.thumb || 'true';


    var multislider = elem.get(0);
    // var multisliderBar = document.createElement("div");
    // multislider.appendChild(sliderBar);

    var numSliders = multislider.getAttribute('data-num-sliders');
//    console.log('numSliders: ', numSliders);
    
    const pxRegex = /([0-9]+).*/
    var offsetTop = multislider.offsetTop;
    var offsetLeft = multislider.offsetLeft;
    var multisliderHeight;
    var multisliderWidth;
//    console.log('colors: ' + multislider.getAttribute('data-colors'));
    var colors = JSON.parse(multislider.getAttribute('data-colors'));
//    console.log('colors: ' + colors[1]);
    var numColors = colors.length;
    var minValue;
    var maxValue;
    var value;
    var mapping;
    var direction;
    var clipZero = multislider.getAttribute('data-clip-zero');
    var valFunction;
    var valueRange;
    var valueRatio;
    var calcBarSize;
    var moveListener;
    var innerBorder;
    var sliders;
    var sliderType;
    var getXFrac;
    var getYFrac;
    
    function makeSlider (div) {
        div.setAttribute('class', sliderType);
        div.setAttribute('style', 'border: 1px solid black;flex: 1 0 auto;');
        div.style.setProperty(innerBorder, 'none');
        div.setAttribute('data-min', minValue);
        div.setAttribute('data-max', maxValue);
        div.setAttribute('data-value', '0');
        div.setAttribute('data-clip-zero', clipZero);
        div.setAttribute('data-mapping', mapping);
        div.setAttribute('data-direction', direction);
        div.style.setProperty('background-color', 'transparent');
        div.style.setProperty('--thumb-color', 'black');
        div.style.width = '';
        div.style.height = '';
//        div.setAttribute('', '');
        return div;
    }

    function createSliders (num, parent) {
        let sliders = new Array(num);
        let tmp;
        let idx;
        for (let i = 0; i < num; i++) {
            tmp = makeSlider(parent.children[i]);
            //            parent.appendChild(tmp);
            idx = tmp.getAttribute('data-idx');
            tmp.style.setProperty("--bar-color", colors[idx%numColors]);
            sliders[idx] = tmp;
            slider(tmp, { thumb: 'nil' });
            tmp.removeDownListener();
        }
        sliders[0].style.setProperty(innerBorder, '');
        return sliders;
    }

    var style = window.getComputedStyle(multislider, null);
    
    function setSliderBarStyle () {
        sliderBar.style.position = 'absolute';
        sliderBar.style.backgroundColor = barColor;
        sliderBar.style.border = 'none';
        sliderBar.style.borderRadius = 'inherit';
    }
    
    function setMinMaxMapping() {
        minValue      = parseFloat(multislider.getAttribute("data-min")) || 0.0;
        maxValue      = parseFloat(multislider.getAttribute("data-max")) || 1.0;
        mapping       = multislider.getAttribute("data-mapping") || 'lin';
        if (mapping == 'log') {
            if ((minValue == 0) && (maxValue == 0)) {
                minValue = 0.01;
                maxValue = 1;
                multislider.setAttribute('data-min', minvalue);
                multislider.setAttribute('data-max', maxValue);
            }
            else {
                if (minValue == 0) {
                    minValue = maxValue / 100;
                    multislider.setAttribute('data-min', minValue);
                }
                else {
                    if (maxValue == 0) {
                        maxValue = minValue / 100;
                        multislider.setAttribute('data-max', maxValue);
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
        value = parseFloat(multislider.getAttribute("data-value")) || 0.0;
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
        multisliderHeight = parseFloat(style.height.match(pxRegex)[1]);
    }

    function setSliderWidthVal () {
        multisliderWidth = parseFloat(style.width.match(pxRegex)[1]);
    }
    
    function getValFraction (val) {
        return ((val - minValue) / valueRange);
    }

    function getYFraction (clientY) {
        let localYFrac = (multisliderHeight + multislider.offsetTop - clientY) / multisliderHeight;
        if (localYFrac < 0) localYFrac = 0;
        if (localYFrac > 1) localYFrac = 1;
        return localYFrac;
    }

    function getYFractionRev (clientY) {
        let localYFrac = (1 - ((multisliderHeight + multislider.offsetTop - clientY) / multisliderHeight));
        if (localYFrac < 0) localYFrac = 0;
        if (localYFrac > 1) localYFrac = 1;
        return localYFrac;
    }

    
    function getXFraction (clientX) {
        let localXFrac = ((clientX - multislider.offsetLeft)) / multisliderWidth;
//        console.log(clientX + ' ' + multisliderWidth  + ' ' + multislider.offsetLeft + ' ' + localXFrac);
        if (localXFrac < 0) localXFrac = 0;
        if (localXFrac > 1) localXFrac = 1;
        return localXFrac;
    }

        function getXFractionRev (clientX) {
            let localXFrac = (1 - ((clientX - multislider.offsetLeft) / multisliderWidth));
//        console.log(clientX + ' ' + multisliderWidth  + ' ' + multislider.offsetLeft + ' ' + localXFrac);
        if (localXFrac < 0) localXFrac = 0;
        if (localXFrac > 1) localXFrac = 1;
        return localXFrac;
    }

    function setDirection () {
        direction = multislider.getAttribute("data-direction") || 'up';
        switch (direction) {
        case 'right':
            sliderType = 'mhslider';
            innerBorder = 'border-top';
            multislider.style.flexDirection = "column";
            moveListener = moveListenerX;
            getYFrac = getYFractionRev;
            getXFrac = getXFraction;
            break;
        case 'left':
            sliderType = 'mhslider';
            innerBorder = 'border-top';
            multislider.style.flexDirection = "column";
            moveListener = moveListenerX;
            getYFrac = getYFractionRev;
            getXFrac = getXFraction;
            break;
        case 'down':
            sliderType = 'mvslider';
            innerBorder = 'border-left';
            moveListener = moveListenerY;
            getYFrac = getYFraction;
            getXFrac = getXFraction;
            break;
        default: // 'up'
            sliderType = 'mvslider';
            innerBorder = 'border-left';
            moveListener = moveListenerY;
            getYFrac = getYFraction;
            getXFrac = getXFraction;
        }
    }

    var moved = false;
    var oldXFraction = -1;
    var oldYFraction = -1;
    
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

    var lastValue;
    var lastIdx;
    function moveListenerY (event) {
        moved = true;
        let YFraction = getYFrac(event.clientY);
        let XFraction = getXFrac(event.clientX);
        idx = Math.floor(getXFrac(event.clientX)*numSliders);
        if (idx >= numSliders) idx = numSliders - 1;
        if ((YFraction != oldYFraction) || (idx != lastIdx)) {
            let newValue = valFunction(YFraction);
//            console.log('idx: ' + idx + ', newValue: ' + newValue);
            
//            multislider.setAttribute('data-value', newValue);
//            multislider.dispatchEvent(valChangeEvent);
            sliders[idx].setBarSize(newValue);
            sliders[idx].dispatchValChangeEvent();
            oldYFraction = YFraction;
            oldXFraction = XFraction;
            lastValue = newValue;
            lastIdx = idx;
        }
    }

    function moveListenerX (event) {
        moved = true;
        let YFraction = getYFrac(event.clientY);
        let XFraction = getXFrac(event.clientX);
        idx = Math.floor(getYFrac(event.clientY)*numSliders);
        if (idx >= numSliders) idx = numSliders - 1;
        if ((XFraction != oldXFraction) || (idx != lastIdx)) {
            let newValue = valFunction(XFraction);
//            console.log('idx: ' + idx + ', newValue: ' + newValue);
            
//            multislider.setAttribute('data-value', newValue);
//            multislider.dispatchEvent(valChangeEvent);
            sliders[idx].setBarSize(newValue);
            sliders[idx].dispatchValChangeEvent();
            oldYFraction = YFraction;
            oldXFraction = XFraction;
            lastValue = newValue;
            lastIdx = idx;
        }
    }

    
    function upListener (event){
        document.removeEventListener('mousemove', moveListener);
        document.removeEventListener('mouseup', upListener);
    }

    function init () {
        setSliderHeightVal();
        setSliderWidthVal();
        setMinMaxMapping();
        setDirection();
        valFunction = linVal;
//        console.log('multisliderHeight: ' + multisliderHeight + 'sliderWidth: ' + multisliderWidth);
        multislider.sliders = createSliders(numSliders, multislider);
        sliders = multislider.sliders;
        moveListener = moveListener;
        multislider.addEventListener('mousedown', downListener);

    }

    init();
}
