//
// numbox.js
//
// definition of numberbox mouse and event handling in the client.
//
// a numberbox is basically an input of type text with added mouse
// handling for dragging numbers. numbox() has to be called with a
// <input type="text"> element.
//
// WARNING: Currently only changing the value attribute after
// initialization is supported. All other attribute or style changes
// after initialization probably have no or detrimental effects.
//
// **********************************************************************
// Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
//
// Revision history: See git repository.
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the Gnu Public License, version 2 or
// later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
// of this agreement.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// **********************************************************************

function numbox(elem){

    var numbox;
    if (elem.nodeType == undefined)
        numbox = elem.get(0);
    else
        numbox = elem;

//    console.log(numbox.getAttribute('value'));


    const valChangeEvent = new Event("valuechange");
    const pxRegex = /([0-9]+).*/
    var offsetTop = numbox.offsetTop;
    var offsetLeft = numbox.offsetLeft;
    var numboxHeight;
    var numboxWidth;
    var minValue;
    var maxValue;
    var lastValue;
    var numScale;

    var mouseMoveListener;

    var style = window.getComputedStyle(numbox, null);

    var foreground = style.color;
    var background = style.backgroundColor;
    var selectedForeground = style.getPropertyValue('--textbox-selected-foreground');
    var selectedBackground = style.getPropertyValue('--textbox-selected-background');

    var offsetLeft = numbox.offsetLeft;
    
    // Utils

        function disableDrag (elem) {
        elem.ondragstart = () => { return false; }
    }
    
    function clamp(number, min, max) {
        return Math.max(min, Math.min(number, max));

    }
    
    function getPrecision (num) {
        let absNum = Math.abs(num);
        let fraction = (100 * (absNum - Math.floor(absNum)));
        if ((fraction == 0) || (fraction == 100)) return 0;
        if (fraction%10 == 0) return 1;
        return 2}

    function formatNumBox (value) {
        return value.toFixed(getPrecision(value));
    }

    function calcNumScale (mouseX) {
//        console.log('calcNumScale' + ' ' + mouseX);
        if (mouseX < 0) return 100;
        if (mouseX < numboxWidth*0.2) return 10;
        if (mouseX < numboxWidth*0.8) return 1;
        if (mouseX < numboxWidth) return 0.1;
        return 0.01;
    }

    function checkMinMax (val) {
//        console.log(val + ' ' + (val < minValue) + ' ' + (maxValue && (val > maxValue)));
        if (val < minValue) return minValue;
        else if (val > maxValue) return maxValue;
//        console.log(val + ' ' + minValue + ' ' + maxValue);
        return val;
    }
    
    // Attribute change handler

    // externalValueChange is a flag indicating whether a Value Change
    // is triggered either by an external program or by mouse
    // interaction. In case it is triggered by an external program
    // (via setAttribute) no valuechange event is generated.

    numbox.externalValueChange = true;

    // store original setAttribute function

    const mySetAttribute = numbox.setAttribute;

    // override setAttribute extending the original definition.

    numbox.setAttribute = function (key, value) {
        value = parseFloat(value);
//        console.log('setAttribute: ', value);
        if (key == 'value') {
            if (numbox.externalValueChange) {
                numbox.value = formatNumBox(value);
                if (value != lastValue)
                    mySetAttribute.call(numbox, key, numbox.value);
            }
            else { 
                if (value != lastValue) {
                    mySetAttribute.call(numbox, key, numbox.value);
                    numbox.dispatchEvent(valChangeEvent);
                }
            }
        }
    }

    // Mouse Event Handlers
    
    var moved = false;
    var dragging = false;
    var startValue = false;
    
    var lastX, lastY;
    var numScale = 1;
    var mouseStartX;
    var mouseStartY;
    
    function mouseDownListener (event) {
        moved = false;
        dragging = false; // shouldn't be necessary, just in case...
        numbox.externalValueChange = false;
        startValue = parseFloat(numbox.getAttribute('value'));
        mouseStartX = event.clientX;
        mouseStartY = event.clientY;
        document.addEventListener('mousemove', mouseMoveListener);
        document.addEventListener('mouseup', mouseUpListener);
    }
    
    function mouseMoveListener (event) {
        let valString;
        if (moved == false) {
            { // called only once after a click and subsequent move.
                dragging = true;
//                console.log('dragging1');
                numScale = calcNumScale(event.clientX-offsetLeft);
                numbox.style.setProperty('--textbox-selected-background', background);
                numbox.style.setProperty('--textbox-selected-foreground', foreground);
                numbox.style.setProperty('--textbox-caret-color', 'transparent');
                numbox.currValue = checkMinMax(startValue + (mouseStartY - event.clientY) * numScale);
                valString = numbox.currValue.toFixed(2); // while dragging truncate attribute to 2 digits after the comma.
                if (valString != lastValue) {
                    numbox.value = valString;
                    numbox.setAttribute('value', valString);
                    lastValue = numbox.currValue;
                }
                numbox.style.textAlign = 'right'; // and align right.
                lastY = event.clientY;
            }
            moved = true;
        }
        else { // called while dragging
//            console.log('dragging2 ' + numScale + ' ' + event.clientY + ' ' + lastValue + ' ' + lastY);
            if (event.shiftKey) {
                numScale = calcNumScale(event.clientX-offsetLeft);
            }
            numbox.currValue = checkMinMax(lastValue + (lastY - event.clientY) * numScale);
            lastY = event.clientY;
            valString = numbox.currValue.toFixed(2); // while dragging truncate to 2 digits after the comma.
            if (valString != lastValue) {
                numbox.value = valString;
                numbox.setAttribute('value', valString);
                lastValue = numbox.currValue;
                }
        }
    }
    function handleKeyDown (event) {
        let keyCode = event.which? event.which : event.keyCode;
        if ((keyCode > 30 && keyCode < 58)
            || keyCode == 190 ||  keyCode == 37 || keyCode == 39 || keyCode == 8
            || (keyCode == 173 && numbox.selectionStart == 0 &&
                ((numbox.value.substring(0,1) != '-') || numbox.selectionEnd > 0)) || keyCode == 13) {
            if (keyCode == 13) {
                numbox.blur();
                numbox.removeEventListener('keydown', this);
                document.addEventListener('mousedown', mouseDownListener);
            }
            return true;
        }
        else {
            event.preventDefault();
            return false;
        }
    }

    numbox.setEditing = function () {
        numbox.style.setProperty('--textbox-selected-background', selectedBackground);
        numbox.style.setProperty('--textbox-selected-foreground', selectedForeground);
        numbox.style.setProperty('--textbox-caret-color', selectedForeground);
        document.removeEventListener('mousemove', mouseMoveListener);
        document.removeEventListener('mouseup', mouseUpListener);
        numbox.removeEventListener('mousedown', mouseDownListener);
        numbox.addEventListener('blur', onEditBlurListener);
        numbox.addEventListener('keydown', handleKeyDown);
    }

    function onEditBlurListener () {
        let number = parseFloat(numbox.value);
        if (isNaN(number)) number = minValue? minValue : 0;
        else number = checkMinMax(number);
        if (startValue != number)
            numbox.setAttribute('value', number);
        numbox.value = formatNumBox(number);
        numbox.addEventListener('mousedown', mouseDownListener);
        numbox.removeEventListener('blur', onEditBlurListener);
        numbox.externalValueChange = true;
    }

    
    function mouseUpListener (event){
        if (dragging) {
            numbox.blur();
            numbox.style.textAlign = 'center'; // restore alignment
            numbox.value = formatNumBox(parseFloat(numbox.getAttribute('value')))

            document.removeEventListener('mousemove', mouseMoveListener);
            document.removeEventListener('mouseup', mouseUpListener);
            numbox.externalValueChange = true;
            dragging = false;
        }
        else {
            numbox.setEditing();
        }
    }

    numbox.removeMouseDownListener = function () {
        numbox.removeEventListener('mousedown', mouseDownListener);
    }

    numbox.dispatchValChangeEvent = function () {
        numbox.dispatchEvent(valChangeEvent);
    }

// initialization

    function init () {
        disableDrag(numbox);
        numboxHeight = parseFloat(style.height.match(pxRegex)[1]);
        numboxWidth = parseFloat(style.width.match(pxRegex)[1]);
        numbox.addEventListener('mousedown', mouseDownListener);
        minValue = parseFloat(numbox.getAttribute('data-min'));
        maxValue = parseFloat(numbox.getAttribute('data-max'));
        if (isNaN(minValue)) {
            numbox.setAttribute('data-min', "false");
            minValue = false;
        }
        if (isNaN(maxValue)) {
            numbox.setAttribute('data-max', "false");
            maxValue = false;
        }
        let value = parseFloat(numbox.getAttribute('value'));
        if (isNaN(value)) value = minValue;
        else value = checkMinMax(value);
        numbox.value = value;
        numbox.currValue = value;
    }

    init();
}
