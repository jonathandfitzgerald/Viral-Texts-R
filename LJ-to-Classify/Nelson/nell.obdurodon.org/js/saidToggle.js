var originalBackground;
function toggleHighlight() {
    var pos = this.getAttribute('value');
    var color;
    switch (pos) {
        case 'nellNelson':
        color = '#A3C9A8';
        break;
        case 'unknown':
        color = 'orange';
        break;
        case 'female':
        color = '#FFAAEA';
        break;
        case 'male':
        color = '#B9CDDA';
        break; 
        case 'workingGirl':
        color = 'pink';
        break;
        case 'employee':
        color = '#C695A6'
        break;
        case 'employer':
        color = '#F2DA00';
        break;
        case 'foreperson':
        color = '#725752';
        break;
        case 'benefactor':
        color = '#29335C';
        break;
        case 'messenger':
        color = '#00A6ED';
        break;
        case 'promMan':
        color = 'blue';
        break;
        case 'promWoman':
        color = 'red';
        break;
        case 'reporter':
        color = 'green';
        break;
        case 'inspector':
        color = '#07F957';
        break;
        case 'unidentified':
        color = 'orange'
        break;
    }
    var status = this.checked;
    // the status to which you've just changed the checkbox
    var spans = document.getElementsByClassName(pos);
    for (var i = 0; i < spans.length; i++) {
        if (status == true) {
            spans[i].style.backgroundColor = color;
        } else {
            spans[i].style.backgroundColor = originalBackground;
            spans[i].style.color = '#000000';
        }
    }
}
function init() {
    originalBackground = document.body.style.backgroundColor;
    var checkboxes = document.getElementsByTagName('input');
    for (var i = 0; i < checkboxes.length; i++) {
        checkboxes[i].addEventListener('click', toggleHighlight, false);
    }
}
window.addEventListener('DOMContentLoaded', init, false);
