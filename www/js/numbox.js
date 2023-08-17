//
// numbox.js
//
// definition of numberbox mouse and event handling in the client.
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

    function disableDrag (elem) {
        elem.ondragstart = () => { return false; }
    }

    disableDrag(numbox);

    const valChangeEvent = new Event("valuechange");
    const pxRegex = /([0-9]+).*/
    var offsetTop = numbox.offsetTop;
    var offsetLeft = numbox.offsetLeft;
    var numboxHeight;
    var numboxWidth;
    var minValue;
    var maxValue;
    var lastValue;

    var mouseMoveListener;

    var style = window.getComputedStyle(numbox, null);

    var foreground = style.color;
    var background = style.backgroundColor;
    var selectedForeground = style.getPropertyValue('--textbox-selected-foreground');
    var selectedBackground = style.getPropertyValue('--textbox-selected-background');

    // Utils
    
    function clamp(number, min, max) {
        return Math.max(min, Math.min(number, max));

    }
    
    // Attribute change handler
    //
    // externalValueChange is a flag indicating whether a Value Change
    // is triggered either by an external program or by mouse
    // interaction. In case it is triggered by an external program
    // (via setAttribute) no valuechange event is generated.

    externalValueChange = true;

    // store original setAttribute function

    const mySetAttribute = numbox.setAttribute;

    // override setAttribute extending the original definition.

    numbox.setAttribute = function (key, value) {
        mySetAttribute.call(numbox, key, value);
        if (key == 'value') {
            if ((externalValueChange) && (value != lastValue)) {
                numbox.value = value;
            }
            else 
                numbox.dispatchEvent(valChangeEvent);
        }
    }

    // Mouse Event Handlers
    
    var moved = false;
    var dragged = false;
    var startValue = false;
    
    var lastX, lastY;
    var numScale = 1;
    var mouseStartX;
    var mouseStartY;
    
    function mouseDownListener (event) {
        moved = false;
        dragged = false;
        externalValueChange = false;
        startValue = parseFloat(numbox.value);
        mouseStartX = event.clientX;
        mouseStartY = event.clientY;
//        console.log('mouseDownListener: ' + event.clientX + ' ' + event.clientY);
        document.addEventListener('mousemove', mouseMoveListener);
        document.addEventListener('mouseup', mouseUpListener);
    }

    numbox.setEditing = function () {
        numbox.style.setProperty('--textbox-selected-background', selectedBackground);
        numbox.style.setProperty('--textbox-selected-foreground', selectedForeground);
        numbox.style.setProperty('--textbox-caret-color', selectedForeground);
        document.removeEventListener('mousemove', mouseMoveListener);
        document.removeEventListener('mouseup', mouseUpListener);
        numbox.removeEventListener('mousedown', mouseDownListener);
        numbox.addEventListener('blur', onEditBlurListener);
        numbox.addEventListener ('keydown', function (event) {
            if (event.key == 'Enter') {
                numbox.blur();
                numbox.removeEventListener('keydown', this);
                document.addEventListener('mousedown', mouseDownListener);
            }
        });
    }

    function onEditBlurListener () {
//        console.log('editingBlur');
        if (startValue != numbox.value)
            numbox.setAttribute('value', numbox.value);
        let number = parseFloat(numbox.value);
        numbox.value = formatNumBox(number);
        numbox.addEventListener('mousedown', mouseDownListener);
        numbox.removeEventListener('blur', onEditBlurListener);
        externalValueChange = true;
    }

    function getPrecision (num) {
        let absNum = Math.abs(num);
        let fraction = Math.round(100 * (absNum - Math.floor(absNum)));
        if ((fraction == 0) || (fraction == 100)) return 0;
        if (fraction%10 == 0) return 1;
        return 2}

    function formatNumBox (value) {
        return value.toFixed(getPrecision(value));
    }

    function calcNumScale (mouseX) {
        if (mouseX < numboxWidth*0.15) return 10;
        if (mouseX < numboxWidth*0.85) return 1;
        if (mouseX < numboxWidth) return 0.1;
        return 0.01;
    }


    
    function mouseMoveListener (event) {
//        console.log('mouseMoveListener: ' + event.clientX + ' ' + event.clientY);
        if ((moved == false) &&
            ((mouseStartX != event.clientX) || (mouseStartY != event.clientY))) { // only called once
//             if (mouseStartX != event.clientX) {
// //                console.log('editing');
//                 numbox.setEditing();
//             }
//             else
            {
//                console.log('dragging');
                dragged = true;
                numbox.style.textAlign = 'right';
                numbox.style.setProperty('--textbox-selected-background', background);
                numbox.style.setProperty('--textbox-selected-foreground', foreground);
                numbox.style.setProperty('--textbox-caret-color', 'transparent');
//                console.log(numbox.style.getPropertyValue('--textbox-selected-background'));
                lastValue = startValue + (mouseStartY - event.clientY) * calcNumScale(event.clientX);
//                console.log('firstDrag: ' + startValue + ' ' + mouseStartY + ' ' + event.clientY + ' ' + numScale);
//                console.log('lastValue: ' + lastValue);
                numbox.value = lastValue.toFixed(2);
                numbox.setAttribute('value', lastValue);
                lastY = event.clientY;
            }
            moved = true;
        }
        else {
//            console.log(lastValue + ' ' + lastY + ' ' + event.clientY + ' ' + numScale);
            lastValue = lastValue + (lastY - event.clientY) * calcNumScale(event.clientX);
            lastY = event.clientY;
            numbox.value = lastValue.toFixed(2);
//            numbox.value = formatNumBox(lastValue);
            numbox.setAttribute('value', lastValue);
            // behaviour while dragging.
        }
    }
    
    function mouseUpListener (event){
        if (dragged) {
//            console.log('draggingBlur');
            numbox.blur();
            numbox.style.textAlign = 'center';
            numbox.value = formatNumBox(parseFloat(numbox.value))
            document.removeEventListener('mousemove', mouseMoveListener);
            document.removeEventListener('mouseup', mouseUpListener);
            externalValueChange = true;
        }
        else {
            numbox.setEditing();
        }
    }

    numbox.removeMouseDownListener = function () {
        numbox.removeEventListener('mousedown', mouseDownListener);
    }

    numbox.dispatchValChangeEvent = function () {
//        console.log('value changed');
        numbox.dispatchEvent(valChangeEvent);
    }

// initialization

    function init () {
        numboxHeight = parseFloat(style.height.match(pxRegex)[1]);
        numboxWidth = parseFloat(style.width.match(pxRegex)[1]);
        numbox.addEventListener('mousedown', mouseDownListener);
    }

    init();
}
