window.pressed = function(){
        var a = document.getElementById('Dir');
        if(a.value === "")
        {
            noFile.innerHTML = "No folder choosen";
        }
        else
        {
            noFile.innerHTML = "";
        }
    };