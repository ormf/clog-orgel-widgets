function toggle (elem, config) {

    var myToggle = elem.get(0);
//    console.log("toggle " + elem);

    const valChangeEvent = new Event("valuechange");
    var colorOff         = config.colorOff || "black";
    var backgroundOff    = config.backgroundOff || "white";
    var labelOff         = config.labelOff || '';
    var valueOff         = config.valueOff || '0';
    var colorOn          = config.colorOn || "black";
    var backgroundOn     = config.backgroundOn || 'rgba(120,120,120,1.0)';
    var labelOn          = config.labelOn || '';
    var valueOn          = config.valueOn || '1';

    // override setAttribute
    const mySetAttribute = myToggle.setAttribute;
    
    myToggle.setAttribute = function (key, value) {
        mySetAttribute.call(myToggle, key, value);
        if (key == 'data-val') {
            if (externalValueChange == true) {
                value = parseFloat(value).toFixed(0);
                if (value != valueOff) value = valueOn;
            }
            drawToggle(myToggle, value);
            if (externalValueChange == false) {
                myToggle.dispatchEvent(valChangeEvent);
                externalValueChange == true;
            }
        }
    }
    
    var  drawToggle = function (toggle, val) {
        if (val == valueOn) {
            toggle.textContent = labelOn;
            toggle.style.color = colorOn;
            toggle.style.background = backgroundOn;
        }
        else {
            toggle.textContent = labelOff;
            toggle.style.color = colorOff;
            toggle.style.background = backgroundOff;
        }
    }

    function mouseDownListener (event) {
        externalValueChange = false;
        let val = (myToggle.getAttribute('data-val') == myToggle.valueOff)? valueOn : valueOff;
        myToggle.setAttribute('data-val', val);
    }

    myToggle.removeMouseDownListener = () => {
        myToggle.removeEventListener('mousedown', mouseDownListener);
    }

    function init () {
        myToggle.colorOff = colorOff;
        myToggle.backgroundOff = backgroundOff;
        myToggle.labelOff = labelOff;
        myToggle.valueOff = valueOff;
        myToggle.colorOn = colorOn;
        myToggle.backgroundOn = backgroundOn;
        myToggle.labelOn = labelOn;
        myToggle.valueOn = valueOn;
        myToggle.ondragstart = () => { return false; }
        myToggle.addEventListener('mousedown', mouseDownListener);
        let val = parseFloat(myToggle.getAttribute('data-val')).toFixed(0);
        if ((val != valueOff) && (val != valueON)) {
            val = valueOff;
            mySetAttribute.call(myToggle, key, val);
        }
        drawToggle(myToggle, val);
    }

    init();
}

function radio (elem) {

    var myRadio = elem.get(0);

    var colorOff      = JSON.parse(myRadio.getAttribute('data-color-off'))      || ['black'];
    var backgroundOff = JSON.parse(myRadio.getAttribute('data-background-off')) || ['white'];
    var labelOff      = JSON.parse(myRadio.getAttribute('data-label-off'))      || ['Off'];
    var valueOff      = JSON.parse(myRadio.getAttribute('data-value-off'))      || ['0'];
    var colorOn       = JSON.parse(myRadio.getAttribute('data-color-on'))       || ['black'];
    var backgroundOn  = JSON.parse(myRadio.getAttribute('data-background-on'))  || ['rgba(120,120,120,1.0)'];
    var labelOn       = JSON.parse(myRadio.getAttribute('data-label-on'))       || ['On'];
    var valueOn       = JSON.parse(myRadio.getAttribute('data-value-on'))       || ['1'];
    var direction     = JSON.parse(myRadio.getAttribute('data-direction'))      || ['right'];
    var num           = JSON.parse(myRadio.getAttribute('data-num'))            || ['1.0'];

    console.log('init');

    function makeToggle () {
        let div = createElement('div');
        div.setAttribute('class', 'toggle');
        div.setAttribute('style', 'border: 1px solid black;flex: 1 0 auto;');
        div.style.width = '';
        div.style.height = '';
        return div;
    }

    function createToggles (num, parent) {
        let toggles = new Array(num);
        let currSlider;
        let idx;
        for (let i = 0; i < num; i++) {
            let currToggle = makeToggle();
            parent.attachChild(currToggle);
            currToggle.setAttribute('data-idx', i);
            currToggle.style.setProperty("--bar-color", colors[idx%numColors]);
            if (i > 0) currToggle.style.setProperty(innerBorder, 'none');
            toggles[idx] = currToggle;
            toggle(currToggle, { colorOff: colorOff, backgroundOff: backgroundOff, labelOff: labelOff, valueOff: valueOff, colorOn: colorOn, backgroundOn: backgroundOn, labelOn: labelOn, valueOn: valueOn});
//            currToggle.removeMouseDownListener();
        }
        return sliders;
    }

    var style = window.getComputedStyle(multislider, null);
    r
    q
    function init () {
        createToggles(num, myRadio);
    }
    
    init();
}
