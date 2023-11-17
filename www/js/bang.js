function bang (elem, config) {

    var myBang;
    
    if (elem.nodeType == undefined)
        myBang = elem.get(0);
    else
        myBang = elem;
    
    const bangEvent = new Event('bang');
    var flashTime        = config.flashTime || '100'; // flash time in ms
    var colorOff         = config.colorOff || 'black';
    var backgroundOff    = config.backgroundOff || 'white';
    var labelOff         = config.labelOff || '';
    var colorOn          = config.colorOn || 'black';
    var backgroundOn     = config.backgroundOn || 'rgba(120,120,120,1.0)';
    var labelOn          = config.labelOn || '';


    // override setAttribute
//    const mySetAttribute = myBang.setAttribute;

    
    // myBang.setAttribute = function (key, value) {
    //     if (key == 'data-val') {
    //         if (myBang.externalValueChange == true) {
    //             value = parseFloat(value).toFixed(0);
    //             if (value != valueOff) value = valueOn;
    //         }
    //         mySetAttribute.call(myBang, key, value);
    //         drawBang(value);
    //         if (myBang.externalValueChange == false) {
    //             myBang.dispatchEvent(valChangeEvent);
    //             myBang.externalValueChange = true;
    //         }
    //     }
    // }
    function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
    }

    function bang() {
        console.log('bang')
        flashBang();
        if (myBang.externalValueChange == false) {
            myBang.dispatchEvent(bangEvent);
        }
    }

    function pulseOn() {
        myBang.pulseActive = true;
        pulseBang();
    }
 
    function pulseOff() {
        myBang.pulseActive = false;
        myBang.textContent = myBang.labelOff;
        myBang.style.color = myBang.colorOff;
        myBang.style.background = myBang.backgroundOff;
     }

    
    async function pulseBang() {
        if (myBang.pulseActive == true) {
            if (myBang.pulseState == false) {
                myBang.textContent = myBang.labelOn;
                myBang.style.color = myBang.colorOn;
                myBang.style.background = myBang.backgroundOn;
                myBang.pulseState = true;
            }
            else {
                myBang.textContent = myBang.labelOff;
                myBang.style.color = myBang.colorOff;
                myBang.style.background = myBang.backgroundOff;
                myBang.pulseState = false;
            }
            await sleep(myBang.pulseTime);
            pulseBang();
        }
    }

 
    
    async function flashBang() {
        if (myBang.flashTime > 0) {
            myBang.textContent = myBang.labelOn;
            myBang.style.color = myBang.colorOn;
            myBang.style.background = myBang.backgroundOn;
            
            await sleep(myBang.flashTime);

            myBang.textContent = myBang.labelOff;
            myBang.style.color = myBang.colorOff;
            myBang.style.background = myBang.backgroundOff;
        }
    }


    function mouseDownListener (event) {
        myBang.externalValueChange = false;
        bang();
        myBang.externalValueChange = true;
    }

    myBang.removeMouseDownListener = () => {
        myBang.removeEventListener('mousedown', mouseDownListener);
    }

//    myBang.draw = drawBang;

    function disable () { return false };
    
    function init () {
        myBang.flashTime = flashTime;
        myBang.externalValueChange = true;
        myBang.pulseState = false;
        myBang.pulseActive = false;
        myBang.pulseTime = 250;
        myBang.colorOff = colorOff;
        myBang.backgroundOff = backgroundOff;
        myBang.labelOff = labelOff;
        myBang.colorOn = colorOn;
        myBang.backgroundOn = backgroundOn;
        myBang.labelOn = labelOn;
        myBang.ondragstart = disable;
        myBang.addEventListener('mousedown', mouseDownListener);
        myBang.onselectstart = disable;
        myBang.textContent = myBang.labelOff;
        myBang.style.color = myBang.colorOff;
        myBang.style.background = myBang.backgroundOff;
        // myBang.addEventListener('dblclick', function(event) {
        //     event.preventDefault();
        //     event.stopPropagation();
        // }, true);
//        let val = parseFloat(myBang.getAttribute('data-val')).toFixed(0);
        // if ((val != valueOff) && (val != valueOn)) {
        //     val = valueOff;
        //     mySetAttribute.call(myBang, 'data-val', val);
        // }
//        drawBang(val);
        myBang.externalValueChange = true;
        myBang.bang = bang;
        myBang.pulseOn = pulseOn;
        myBang.pulseOff = pulseOff;
    }

    init();
}
