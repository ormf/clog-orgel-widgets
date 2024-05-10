var sliderConstrainButton;
var sliderConstrained=0;

function toggleSliderConstrain() {

    if (sliderConstrained == 0) {
        sliderConstrained = 1;
        sliderConstrainButton.style.background = 'orange';
    }
    else {
        sliderConstrained = 0;
        sliderConstrainButton.style.background = 'white';
    }
};

function orgelSetupGlobal() {
    sliderConstrainButton = document.getElementsByClassName("slider-constrain") [0];
    sliderConstrainButton.addEventListener('click', toggleSliderConstrain);
}
