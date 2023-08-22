function vumeter(elem, config){

    // Settings
    var max             = config.max || 100;
    var boxCount        = config.boxCount || 40;
    var ledColors       = config.ledColors || 'green';
    var barColor        = config.barColor || 'rgba(60,60,255,1.0)';
    var vuType          = config.vuType || 'led';
    var ledMapping      = config.ledMapping || 'db-lin';
    var vuDirection     = config.direction || 'up';
    var vuInnerPadding = config.innerPadding || '2px';
    var vuInnerPaddingBottom = config.innerPaddingBottom || '2px';

//    console.log(config);
    
    // Colours
    var redOn     = 'rgba(255,47,30,1.0)';
    var redOff    = 'rgba(64,12,8,1.0)';
    var yellowOn  = 'rgba(255,215,5,1.0)';
    var yellowOff = 'rgba(64,53,0,1.0)';
    var orangeOn  = 'rgba(215,215,5,1.0)';
    var orangeOff = 'rgba(53,53,0,1.0)';
    var greenOn   = 'rgba(53,255,30,1.0)';
    var greenOff  = 'rgba(13,64,8,1.0)';

    var PdPurple  = 'rgba(244,48,240,1.0)';
    var PdRed     = 'rgba(252,40,40,1.0)';
    var PdOrange  = 'rgba(250,171,71,1.0)';
    var PdYellow  = 'rgba(232,232,40,1.0)';
    var PdGreen   = 'rgba(20,232,20,1.0)';
    var colBlue1 = 'rgba(0, 85, 100, 1.0)';
    var colBlue2 = 'rgba(0, 102, 128, 1.0)';
    var colBlue3 = 'rgba(0, 136, 170, 1.0)';
    var colBlue4 = 'rgba(0, 170, 212, 1.0)';
    var colBlue5 = 'rgba(0, 190, 245, 1.0)';
    var colBlue6 = 'rgba(50, 202, 255, 1.0)';
    var colBlue7 = 'rgba(85, 211, 255, 1.0)';
    var colBlue8 = 'rgba(128, 222, 255, 1.0)';
    var colBlue9 = 'rgba(170, 235, 255, 1.0)';
    var colBlue10 = 'rgba(213, 246, 255, 1.0)';

    var colGreen1 = 'rgba(0, 85, 0, 1.0)';
    var colGreen2 = 'rgba(0, 128, 0, 1.0)';
    var colGreen3 = 'rgba(0, 170, 0, 1.0)';
    var colGreen4 = 'rgba(0, 212, 0, 1.0)';
    var colGreen5 = 'rgba(0, 255, 0, 1.0)';
    var colGreen6 = 'rgba(42, 255, 42, 1.0)';
    var colGreen7 = 'rgba(85, 255, 85, 1.0)';
    var colGreen8 = 'rgba(128, 255, 128, 1.0)';
    var colGreen9 = 'rgba(170, 255, 170, 1.0)';
    var colGreen10 = 'rgba(213, 255, 213, 1.0)';

    var colRed1 = 'rgba(85, 0, 0, 1.0)';
    var colRed2 = 'rgba(128, 0, 0, 1.0)';
    var colRed3 = 'rgba(170, 0, 0, 1.0)';
    var colRed4 = 'rgba(213, 0, 0, 1.0)';
    var colRed5 = 'rgba(255, 0, 0, 1.0)';
    var colRed6 = 'rgba(255, 42, 42, 1.0)';
    var colRed7 = 'rgba(255, 85, 85, 1.0)';
    var colRed8 = 'rgba(255, 128, 128, 1.0)';
    var colRed9 = 'rgba(255, 170, 170, 1.0)';
    var colRed10 = 'rgba(255, 213, 213, 1.0)';

    // Derived and starting values

    var colors = new Array(40);

    var vuHeight;
    var vuWidth;
    var drawVu;
    var vuBar;
    var setBarSize;
    
    var vuMeter = elem.get(0);
//    console.log('vuMeter: ' + vuMeter);
//    console.log('ledColors: ' + ledColors);

    var style = window.getComputedStyle(vuMeter, null);

    var lastVal = 0;
    var dbLedIdxLookup;

    function clamp(number, min, max) {
        return Math.max(min, Math.min(number, max));
    }
    
    function createLedContainer (parent) {
        let vuLedContainer = document.createElement("div");
        vuLedContainer.style.height = "100%";
        vuLedContainer.style.width = "100%";
        vuLedContainer.style.padding = "2px";
        vuLedContainer.style.display = "flex";
        vuLedContainer.style.flexDirection = "column";
        vuLedContainer.style.padding = vuInnerPadding;
        vuLedContainer.style.paddingBottom = vuInnerPaddingBottom;
        vuLedContainer.style.justifyContent = "space-between";
        parent.appendChild(vuLedContainer);
        parent.ledContainer = vuLedContainer;
    }

    function createLeds (parent) {
        let leds = [];
        createLedContainer(parent);
        for (i = 39;i>=0;i--) {
            leds[i] = document.createElement("span");
            leds[i].style.width = "100%";
            leds[i].style.height = "100%";
            leds[i].style.border = "thin solid var(--vu-background)";
            leds[i].style.backgroundColor = "var(--vu-background)";
            if (i < 39) { leds[i].style.borderTopStyle = "none"; }
            parent.ledContainer.appendChild(leds[i]);
            parent.leds = leds;
        }
    }


    function createBar (parent) {
        createLedContainer(parent);
        let vuBar = document.createElement("span");
        vuBar.style.height = "100%";
        vuBar.style.width = "100%";
//        vuBar.style.border = "thin solid var(--vu-background)";
        vuBar.style.border = "thin solid var(--vu-background)";
        vuBar.style.backgroundColor = "var(--vu-background)";
//        vuBar.style.display = "flex";
//        vuBar.style.flexDirection = "column";
//        vuBar.style.justifyContent = "space-between";
        parent.ledContainer.appendChild(vuBar);
        parent.vuBar = vuBar;

    }

    function setBarSizeY(db) {
//        console.log(((db/112)*vuHeight) + 'px');
        vuMeter.vuBar.style.height = ((db/112)*vuHeight) + 'px';
    }
    
    function setBarSizeX(db) {
        vuMeter.vuBar.style.width = ((db/112)*vuWidth) + 'px';
    }
    
    function drawBar () {
        var targetDB = clamp((100+parseInt(vuMeter.getAttribute("data-db"), 10)), 0, 112);
//        console.log('drawBar!' + targetDB + ' ' + colors[targetDB]);
        setBarSize(targetDB); 
        vuMeter.vuBar.style.backgroundColor = colors[dbLedIdxLookup[targetDB]];
    }

    function drawLed () {
        let leds = vuMeter.leds;
        var targetDB = clamp((100+parseInt(vuMeter.getAttribute("data-db"), 10)), 0, 112);

//        console.log('redraw! ' + vuMeter.getAttribute("data-db") + ', dB: ' + targetDB);
        
        var targetVal = dbLedIdxLookup[targetDB];

        if (targetVal != lastVal) {
            if (targetVal > lastVal) {
                for (var i = lastVal;i < targetVal;i++) {
                    leds[i].style.backgroundColor = colors[i];
//                    console.log('i: ' + i + ', on: ' + leds[i].style.backgroundColor + ' ' + colors[i]);
                }
            }
            else {
                for (var i = targetVal;i < lastVal;i++) {
                    leds[i].style.backgroundColor = "var(--vu-background-color)";
//                    console.log('i: ' + i + ', off: ' + leds[i].style.backgroundColor);
                }
            }
        
            lastVal = targetVal;
        }
    };
    
    const mySetAttribute = vuMeter.setAttribute;
    // override setAttribte

    vuMeter.setAttribute = function (key, value) {
        mySetAttribute.call(vuMeter, key, value);
        if (key == 'data-db') {
            value = clamp(parseFloat(value), 0 , 112).toFixed(0);
            drawVu();
        }
    }

    function setPdColors () {
        for (i = 0;i<16;i++) { colors[i] = PdGreen; }
        for (i = 16;i<26;i++) { colors[i] = PdYellow; }
        for (i = 26;i<28;i++) { colors[i] = PdOrange; }
        for (i = 28;i<39;i++) { colors[i] = PdRed; }
        colors[39] = PdPurple;
    }

    
    function setBlueColors () {
        for (i = 0;i<1;i++) { colors[i] = colBlue1; }
        for (i = 1;i<4;i++) { colors[i] = colBlue2; }
        for (i = 4;i<7;i++) { colors[i] = colBlue3; }
        for (i = 7;i<10;i++) { colors[i] = colBlue4; }
        for (i = 10;i<13;i++) { colors[i] = colBlue5; }
        for (i = 13;i<16;i++) { colors[i] = colBlue6; }
        for (i = 16;i<19;i++) { colors[i] = colBlue7; }
        for (i = 19;i<22;i++) { colors[i] = colBlue8; }
        for (i = 22;i<25;i++) { colors[i] = colBlue9; }
        for (i = 25;i<28;i++) { colors[i] = colBlue10; }
        for (i = 28;i<31;i++) { colors[i] = colRed9; }
        for (i = 31;i<34;i++) { colors[i] = colRed8; }
        for (i = 34;i<37;i++) { colors[i] = colRed7; }
        for (i = 37;i<40;i++) { colors[i] = colRed6; }
     }

    function setGreenColors () {
        for (i = 0;i<1;i++) { colors[i] = colGreen1; }
        for (i = 1;i<4;i++) { colors[i] = colGreen2; }
        for (i = 4;i<7;i++) { colors[i] = colGreen3; }
        for (i = 7;i<10;i++) { colors[i] = colGreen4; }
        for (i = 10;i<13;i++) { colors[i] = colGreen5; }
        for (i = 13;i<16;i++) { colors[i] = colGreen6; }
        for (i = 16;i<19;i++) { colors[i] = colGreen7; }
        for (i = 19;i<22;i++) { colors[i] = colGreen8; }
        for (i = 22;i<25;i++) { colors[i] = colGreen9; }
        for (i = 25;i<28;i++) { colors[i] = colGreen10; }
        for (i = 28;i<31;i++) { colors[i] = colRed9; }
        for (i = 31;i<34;i++) { colors[i] = colRed8; }
        for (i = 34;i<37;i++) { colors[i] = colRed7; }
        for (i = 37;i<40;i++) { colors[i] = colRed6; }
    }

    function setCustomColors (cols) {
        for (i = 0;i<1;i++) { colors[i] = cols[0]; }
        for (i = 1;i<7;i++) { colors[i] = cols[1]; }
        for (i = 4;i<7;i++) { colors[i] = cols[2]; }
        for (i = 7;i<10;i++) { colors[i] = cols[3]; }
        for (i = 10;i<13;i++) { colors[i] = cols[4]; }
        for (i = 13;i<16;i++) { colors[i] = cols[5]; }
        for (i = 16;i<19;i++) { colors[i] = cols[6]; }
        for (i = 19;i<22;i++) { colors[i] = cols[7]; }
        for (i = 22;i<25;i++) { colors[i] = cols[8]; }
        for (i = 25;i<28;i++) { colors[i] = cols[9]; }
        for (i = 28;i<31;i++) { colors[i] = cols[10]; }
        for (i = 31;i<34;i++) { colors[i] = cols[11]; }
        for (i = 34;i<37;i++) { colors[i] = cols[12]; }
        for (i = 37;i<40;i++) { colors[i] = cols[13]; }
     }

    function setLedMapping () {
//        console.log('ledMapping: ' + ledMapping);
        switch (ledMapping) {
        case 'pd' :
            dbLedIdxLookup = [0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 10, 10, 11, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 19, 19, 20, 21, 22, 23, 24, 25, 27, 29, 31, 33, 34, 35, 36, 37, 37, 38, 39, 39, 39, 40];            
            break;
        case 'db-lin' :
            dbLedIdxLookup = [0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 9, 10, 10, 10, 11, 11, 11, 12, 12, 12, 13, 13, 13, 14, 14, 14, 15, 15, 15, 16, 16, 16, 17, 17, 17, 18, 18, 18, 19, 19, 19, 20, 20, 20, 21, 21, 21, 22, 22, 22, 23, 23, 23, 24, 24, 24, 25, 25, 25, 26, 26, 26, 27, 27, 27, 28, 28, 28, 29, 30, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40];
            break;
        }
    }
    
    function setBarDirection () {
        switch (vuDirection) {
        case 'right' :
            setBarSize = setBarSizeX;
            vuMeter.ledContainer.style.flexDirection = "row";
            break;
        case 'left' :
            setBarSize = setBarSizeX;
            vuMeter.ledContainer.style.flexDirection = "row-reverse";
            break;
        case 'down' :
            setBarSize = setBarSizeY;
            vuMeter.ledContainer.style.flexDirection = "column";
            break;
        default : // 'up'
            setBarSize = setBarSizeY;
            vuMeter.ledContainer.style.flexDirection = "column-reverse";
            break;
        }
    }

    function init() {
        vuMeter.style.background = 'var(--vu-background)';
        vuHeight = parseFloat(style.height);
        vuWidth = parseFloat(style.width);
//        console.log('height: ' + vuHeight + ', width: ' + vuWidth + ' ' + vuType);
        switch (ledColors) {
        case "green":
            setGreenColors();
            break;
        case "blue":
            setBlueColors();
            break;
        case "pd":
            setPdColors();
            break;
        default:
            if (ledColors.length == 14) {
                setCustomColors(ledColors);
            }
            else setGreenColors();
            break;
        }
        setLedMapping();
        switch(vuType) {
        case 'led' :
//            console.log('ledColors: ' + ledColors);
            drawVu = drawLed;
            createLeds(vuMeter);
            break;
        case 'bar' :
//            console.log('drawBar: ' + ledColors);
            createBar(vuMeter);
            setBarDirection();
            drawVu = drawBar;
            break;
        }
        drawVu();
    }
    
    init();

}
