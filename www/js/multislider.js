//
// multislider.js
//
// definition of multislider mouse and event handling in the client.
// For this to work, slider.js needs to have been
// loaded. multislider() gets called with the container of the sliders
// already containing the sliders as uninitialized div elements with
// their idx set in the data-idx attribute.
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

function multislider(elem, config) {
    var thumb         = config.thumb || 'true';
    const pxRegex = /([0-9]+).*/

    var multislider = elem.get(0);
    var numSliders = multislider.getAttribute('data-num-sliders');
    var colors = JSON.parse(multislider.getAttribute('data-colors'));
    var numColors = colors.length;
    
    var offsetTop = multislider.offsetTop;
    var offsetLeft = multislider.offsetLeft;
    var multisliderHeight, multisliderWidth;
    var minValue, maxValue, value;
    var mapping;
    var direction;
    var clipZero = multislider.getAttribute('data-clip-zero');
    var mouseDownListener, mouseMoveListener;
    var sliders;
    var sliderType; // mvslider or mhslider, depending on direction
    var getXFrac, getYFrac;  // functions for calculating the
                             // X/YFraction on mousemove depending on
                             // direction of the sliders.
    var innerBorder;     // we set one of the borders between the
                         // sliders to none except for the border of
    // the first slider. Depending on the direction of the sliders,
    // this is either border-top or border-left.
    
    function clamp(number, min, max) {
        return Math.max(min, Math.min(number, max));
    }

    function disableDrag (elem) {
        elem.ondragstart = () => { return false; }
    }
    
    function makeSlider (div) {
        div.setAttribute('class', sliderType);
        div.setAttribute('style', 'border: 1px solid black;flex: 1 0 auto;');
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
        return div;
    }

    function createSliders (num, parent) {
        let sliders = new Array(num);
        let currSlider;
        let idx;
        for (let i = 0; i < num; i++) {
            currSlider = makeSlider(parent.children[i]);
            idx = currSlider.getAttribute('data-idx');
            currSlider.style.setProperty("--bar-color", colors[idx%numColors]);
            if (i > 0) currSlider.style.setProperty(innerBorder, 'none');
            sliders[idx] = currSlider;
            slider(currSlider, { thumb: 'nil' });
            currSlider.removeMouseDownListener();
        }
        return sliders;
    }

    var style = window.getComputedStyle(multislider, null);
        
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
        setDirection();
    }
    
    function setSliderHeightVal () {
        multisliderHeight = parseFloat(style.height.match(pxRegex)[1]);
    }

    function setSliderWidthVal () {
        multisliderWidth = parseFloat(style.width.match(pxRegex)[1]);
    }

    // Utils
    
    function getValFraction (val) {
        return ((val - minValue) / valueRange);
    }

    function getYFraction (clientY) {
        let localYFrac = (multisliderHeight + multislider.offsetTop - clientY) / multisliderHeight;
        return clamp(localYFrac, 0, 1);
    }

    function getXFraction (clientX) {
        let localXFrac = ((clientX - multislider.offsetLeft)) / multisliderWidth;
        return clamp(localXFrac, 0, 1);
    }

    function getYFractionRev (clientY) {
        let localYFrac = (1 - (multisliderHeight + multislider.offsetTop - clientY) / multisliderHeight);
        return clamp(localYFrac, 0, 1);
    }

    function getXFractionRev (clientX) {
        let localXFrac = (1 - ((clientX - multislider.offsetLeft)) / multisliderWidth);
        return clamp(localXFrac, 0, 1);
    }

    function setDirection () {
        direction = multislider.getAttribute("data-direction") || 'up';
        switch (direction) {
        case 'right':
            sliderType = 'mhslider';
            getXFrac = getXFraction;
            getYFrac = getYFractionRev;
            innerBorder = 'border-top';
            multislider.style.flexDirection = "column";
            mouseMoveListener = mouseMoveListenerX;
            mouseDownListener = mouseDownListenerX;
            break;
        case 'left':
            sliderType = 'mhslider';
            getXFrac = getXFraction;
            getYFrac = getYFractionRev;
            innerBorder = 'border-top';
            multislider.style.flexDirection = "column";
            mouseMoveListener = mouseMoveListenerX;
            mouseDownListener = mouseDownListenerX;
            break;
        case 'down':
            sliderType = 'mvslider';
            getXFrac = getXFraction;
            getYFrac = getYFraction;
            innerBorder = 'border-left';
            mouseMoveListener = mouseMoveListenerY;
            mouseDownListener = mouseDownListenerY;
            break;
        default: // 'up'
            sliderType = 'mvslider';
            getXFrac = getXFraction;
            getYFrac = getYFraction;
            innerBorder = 'border-left';
            mouseMoveListener = mouseMoveListenerY;
            mouseDownListener = mouseDownListenerY;
        }
    }

    // mouse handling
    
    var moved = false;
    var lastIdx = false;
    var lastFraction = false;
    
    function mouseDownListenerY (event) {
        moved = false;
        let YFraction = getYFrac(event.clientY);
        let XFraction = getXFrac(event.clientX);
        idx = Math.floor(getXFrac(event.clientX)*numSliders);
        if (idx >= numSliders) idx = numSliders - 1;
        sliders[idx].setBarSize(YFraction);
        lastFraction = YFraction;
        lastIdx = idx;
        document.addEventListener('mousemove', mouseMoveListener);
        document.addEventListener('mouseup', mouseUpListener);
    }


    function mouseDownListenerX (event) {
        moved = false;
        let YFraction = getYFrac(event.clientY);
        let XFraction = getXFrac(event.clientX);
        idx = Math.floor(getYFrac(event.clientY)*numSliders);
        if (idx >= numSliders) idx = numSliders - 1;
        sliders[idx].setBarSize(XFraction);
        lastFraction = YFraction;
        lastIdx = idx;
        document.addEventListener('mousemove', mouseMoveListener);
        document.addEventListener('mouseup', mouseUpListener);
    }

    function interpolateSetBarSize (idx, fraction) {
        let dIdx = idx - lastIdx;
        let dFraction = fraction - lastFraction;
        if (lastIdx && (idx != lastIdx)) {
            if (idx > lastIdx) {
                for (let i = idx; i > lastIdx;i--) {
                    let f = fraction + ((i-idx)/dIdx * dFraction);
                    sliders[i].setBarSize(f);
                }
            }
            else {
                for (let i = idx; i < lastIdx;i++) {
                    let f = fraction + ((i-idx)/dIdx * dFraction);
                    sliders[i].setBarSize(f);
                    sliders[i].setBarSize(f);
                }
            }
        }
        else sliders[idx].setBarSize(fraction);
    }
    
    function mouseMoveListenerY (event) {
        moved = true;
        let YFraction = getYFrac(event.clientY);
        let XFraction = getXFrac(event.clientX);
        idx = Math.floor(getXFrac(event.clientX)*numSliders);
        if (idx >= numSliders) idx = numSliders - 1;
//        console.log('mouseMoveListener' + idx + ' ' + lastIdx + ' ' + YFraction + ' ' + lastFraction);
        if ((idx != lastIdx) || (YFraction != lastFraction)) {
            interpolateSetBarSize(idx, YFraction);
            lastFraction = YFraction;
            lastIdx = idx;
        }
    }

    function mouseMoveListenerX (event) {
        moved = true;
        let YFraction = getYFrac(event.clientY);
        let XFraction = getXFrac(event.clientX);
        idx = Math.floor(getYFrac(event.clientY)*numSliders);
        if (idx >= numSliders) idx = numSliders - 1;
        if ((idx != lastIdx) || (XFraction != lastFraction)) {
            interpolateSetBarSize(idx, XFraction);
            lastFraction = XFraction;
            lastIdx = idx;
        }
    }

    function mouseUpListener (event){
        document.removeEventListener('mousemove', mouseMoveListener);
        document.removeEventListener('mouseup', mouseUpListener);
    }

    function init () {
        disableDrag(multislider);
        setSliderHeightVal();
        setSliderWidthVal();
        setMinMaxMapping();
        setDirection();
        multislider.sliders = createSliders(numSliders, multislider);
        sliders = multislider.sliders;
        multislider.addEventListener('mousedown', mouseDownListener);

    }

    init();
}
