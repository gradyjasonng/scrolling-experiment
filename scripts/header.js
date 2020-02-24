<script>
console.log("loading");
document.getElementById('Plug').remove();

function resetScroll() {
   xform = 'transform';
   ['webkit', 'Moz', 'O', 'ms'].every(function (prefix) {
      var e = prefix + 'Transform';
      if (typeof document.getElementById('Wrapper').style[e] !== 'undefined') {
         xform = e;
         return false;
      }
      return true;
   });
   window.scroll(0,0);
   document.getElementById('Wrapper').style[xform] = 'translateY(0px)';

document.body.style.backgroundImage = "none";
document.body.style.overflow = "auto";
jQuery(document.getElementById('Wrapper')).css({"width": "100%", "margin-left": "auto", "margin-right": "auto", "background-color": "white"})
document.body.style.background = "white";
}

function scrollInit(m){
   var passage, target, dist = 0, view, relative, min, max, offset, reference, pressed, xform, velocity, frame, timestamp, ticker, amplitude, target, timeConstant = 325, ref, scrolls=0;
   var multiplier = m;
   view = document.getElementById('Wrapper');
   passage = document.getElementById('passage');
refEl = jQuery("#Wrapper strong")[0];
if(refEl == null){
refEl = jQuery("#Wrapper b")[0];
}
ref = refEl.innerHTML.toLowerCase();
console.log(ref)
document.body.style.overflow = "hidden";
jQuery(view).css({"width": "80%", "margin-left": "auto", "margin-right": "auto", "background-color": "white"})

if("${e://Field/condition}" == "pm"){
if(m>1){
document.body.style.backgroundImage = "linear-gradient(174deg, rgba(182,236,245,1) 0%, rgba(239,251,255,1) 44%, rgba(141,235,255,1) 100%)";
}else if(m==1){
document.body.style.backgroundImage = "url('https://i.ibb.co/7VY53Xr/retina-wood-2-X.png')";
}else{
document.body.style.backgroundImage = "url('https://i.ibb.co/y83Sbt7/cork-board.png')";
}
}else{
document.body.style.background = "#cccccc";
}

view.innerHTML = view.innerHTML.replace(ref, "<span id='target'>" + ref + "</span>")
   target = document.getElementById('target');
   max = parseInt(getComputedStyle(view).height, 10) - innerHeight;
   offset = min = 0;
   pressed = false;
   xform = 'transform';
   ['webkit', 'Moz', 'O', 'ms'].every(function (prefix) {
      var e = prefix + 'Transform';
      if (view.style[e] !== 'undefined') {
         xform = e;
         return false;
      }
      return true;
   });

   function ypos(e) {
      if (e.targetTouches && (e.targetTouches.length >= 1)) {
         return e.targetTouches[0].clientY;
      }
      return e.clientY;
   }

   function xpos(e) {
      if (e.changedTouches && (e.changedTouches.length >= 1)) {
         return e.changedTouches[0].clientX;
      }
      return e.clientX;
   }

   function finalypos(e) {
      if (e.changedTouches && (e.changedTouches.length >= 1)) {
         return e.changedTouches[0].clientY;
      }
      return e.clientY;
   }

   function scroll(y) {
      offset = (y > max) ? max : (y < min) ? min : y;
      view.style[xform] = 'translateY(' + -(offset) + 'px)';
   }

   function track() {
      console.log("tracking")
      var now, elapsed, delta, v;
      now = Date.now();
      elapsed = now - timestamp;
      timestamp = now;
      delta = offset - frame;
      frame = offset;
      v = 1000 * delta / (1 + elapsed);
      velocity = 0.8 * v + 0.2 * velocity;
   }

   function autoScroll() {
      var elapsed, delta;
      if (amplitude) {
         elapsed = Date.now() - timestamp;
         delta = -amplitude * Math.exp(-elapsed / timeConstant);
         if (delta > 0.5 || delta < -0.5) {
            dist += Math.abs(delta);
            scroll(target + delta);
            requestAnimationFrame(autoScroll);
         } else {
            scroll(target);
         }
      }
   }

   function tap(e) {
      pressed = true;
      reference = ypos(e);

      velocity = amplitude = 0;
      frame = offset;
      timestamp = Date.now();
      clearInterval(ticker);
      ticker = setInterval(track, 50);

      e.preventDefault();
      e.stopPropagation();
      return false;
   }

   function drag(e) {
      var y, delta;
      if (pressed) {
         y = ypos(e);
         delta = reference - y;
         if (delta > 2 || delta < -2) {
            reference = y;
            scroll(offset + delta*multiplier);
            dist += Math.abs(delta*multiplier);
         }
      }
      e.preventDefault();
      e.stopPropagation();
      return false;
   }


   function release(e) {
      pressed = false;
scrolls += 1;
      clearInterval(ticker);
      if (velocity > 5 || velocity < -5) {
         amplitude = 0.8 * multiplier * velocity;
         target = Math.round(offset + amplitude);
         timestamp = Date.now();
         requestAnimationFrame(autoScroll);

      }
      else{
         var targetRect = document.getElementById('target').getBoundingClientRect();
         // console.log(targetRect)
         // console.log(xpos(e))
         // console.log(finalypos(e))
         // console.log(targetRect.left < xpos(e))
         // console.log(targetRect.right > xpos(e))
         // console.log(targetRect.top < finalypos(e))
         // console.log(targetRect.bottom > finalypos(e))
         if(targetRect.left < xpos(e) && targetRect.right > xpos(e) && targetRect.top < finalypos(e) && targetRect.bottom > finalypos(e)){
            //console.log("passed");
            if (typeof window.ontouchstart !== 'undefined') {
               view.removeEventListener('touchstart',tap);
               view.removeEventListener('touchmove',drag);
               view.removeEventListener('touchend',release);
            }
            view.removeEventListener('mousedown',tap);
            view.removeEventListener('mousemove',drag);
            view.removeEventListener('mouseup',release);
            console.log(jQuery(document.getElementById('target')).closest("div[class*='QuestionOuter']").attr("questionid"));

console.log(scrolls)
            Qualtrics.SurveyEngine.setEmbeddedData(jQuery(document.getElementById('target')).closest("div[class*='QuestionOuter']").attr("questionid") + "_distance", dist);
Qualtrics.SurveyEngine.setEmbeddedData(jQuery(document.getElementById('target')).closest("div[class*='QuestionOuter']").attr("questionid") + "_scrolls", scrolls-1);

            document.getElementById('NextButton').click();
         }
      }
      e.preventDefault();
      e.stopPropagation();
      return false;
   }

   if (typeof window.ontouchstart !== 'undefined') {
      view.addEventListener('touchstart', tap);
      view.addEventListener('touchmove', drag);
      view.addEventListener('touchend', release);
   }
   view.addEventListener('mousedown', tap);
   view.addEventListener('mousemove', drag);
   view.addEventListener('mouseup', release);
}


</script>
