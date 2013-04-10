var main = (function() {
  var v130 = function() {
    var v33 = function(v0,v1,v2) {
      v0.save();
      v0.beginPath();
      var c6 = v1*1.0;
      var c7 = 0-c6;
      v0.moveTo(0,c7);
      var v8 = undefined;
      var c9 = v2%5;
      if(c9==0){
        var c11 = v1*0.8;
        var c12 = 0-c11;
        v0.lineTo(0,c12);
      } else {
        var c14 = v1*0.9;
        var c15 = 0-c14;
        v0.lineTo(0,c15);
      }
      var v16 = undefined;
      var c17 = v2%15;
      if(c17==0){
        v0["lineWidth"] = 8;
      } else {
        v0["lineWidth"] = 3;
      }
      v0.stroke();
      v0.closePath();
      var v20 = undefined;
      var c21 = v2%5;
      if(c21==0){
        var c23 = v1*0.75;
        var c24 = 0-c23;
        v0.translate(c24,0);
        var c26 = 2*Math.PI;
        var c27 = c26/4;
        var c28 = 0-c27;
        v0.rotate(c28);
        var c30 = v2/5;
        var c31 = Math.floor(c30);
        v0.fillText(c31,0,0);
      } else {
      }
      v0.restore();
    };
    var v50 = function(v34,v35,v36,v37,v38) {
      v34.save();
      v34["lineCap"] = "round";
      v34.rotate(v36);
      v34["lineWidth"] = v37;
      v34.beginPath();
      var c43 = v35*0.1;
      v34.moveTo(0,c43);
      var c45 = v35*v38;
      var c46 = 0-c45;
      v34.lineTo(0,c46);
      v34.stroke();
      v34.closePath();
      v34.restore();
    };
    var v80 = function(v51,v52) {
      var v53 = new Date();
      var v54 = v53.getHours();
      var v55 = v53.getMinutes();
      var v56 = v53.getSeconds();
      v51.save();
      v51["lineCap"] = "round";
      var c59 = v55%60;
      var c60 = c59/60;
      var c61 = v54%12;
      var c62 = c61+c60;
      var c63 = 2*Math.PI;
      var c64 = c63/12;
      var c65 = c64*c62;
      v50(v51,v52,c65,15,0.4);
      var c67 = v56%60;
      var c68 = c67/60;
      var c69 = v55%60;
      var c70 = c69+c68;
      var c71 = 2*Math.PI;
      var c72 = c71/60;
      var c73 = c72*c70;
      v50(v51,v52,c73,10,0.7);
      v51["strokeStyle"] = "red";
      var c75 = v56%60;
      var c76 = 2*Math.PI;
      var c77 = c76/60;
      var c78 = c77*c75;
      v50(v51,v52,c78,4,0.9);
      v51.restore();
    };
    var v98 = function(v81,v82) {
      v81.save();
      var c85 = 2*Math.PI;
      var c86 = c85/4;
      v81.rotate(c86);
      var v95 = function(v87) {
        v81.save();
        var c90 = 2*Math.PI;
        var c91 = c90/60;
        var c92 = c91*v87;
        v81.rotate(c92);
        v33(v81,v82,v87);
        v81.restore();
      };
      [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60].forEach(v95);
      v81.restore();
    };
    var v125 = function() {
      var v99 = $("#canvas");
      var v100 = v99.innerWidth();
      var v101 = v99.innerHeight();
      var v102 = $("#canvas");
      var v103 = v102.innerWidth();
      var v104 = v102.innerHeight();
      var v105 = document.getElementById("canvas");
      var v106 = v105.getContext("2d");
      v106.save();
      v106["fillStyle"] = "black";
      v106["strokeStyle"] = "black";
      v106["lineCap"] = "round";
      v106["textAlign"] = "center";
      var c108 = v100>=v101;
      var c109 = c108?v100:v101;
      var c110 = c109/2;
      var c111 = c110*0.1;
      v106["font"] = c111+"px serif";
      v106["textBaseline"] = "top";
      v106.clearRect(0,0,v103,v104);
      var c114 = v104/2;
      var c115 = v103/2;
      v106.translate(c115,c114);
      var c117 = v100>=v101;
      var c118 = c117?v100:v101;
      var c119 = c118/2;
      v98(v106,c119);
      var c121 = v100>=v101;
      var c122 = c121?v100:v101;
      var c123 = c122/2;
      v80(v106,c123);
      v106.restore();
    };
    var v127 = function() {
      v125();
    };
    var v128 = window.setInterval(v127,1000);
    v125();
  };
  return v130;
})();