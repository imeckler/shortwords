var highestLevel = localStorage.getItem('shortwords-level')
highestLevel = highestLevel ? JSON.parse(highestLevel) : 0;
console.log(highestLevel);

var main = Elm.fullscreen(Elm.Main, {setHighestLevel: highestLevel});
main.ports.setHighestLevel.send(highestLevel);

main.ports.setLocalStorage.subscribe(function(i) {
  localStorage.setItem('shortwords-level', i);
});

