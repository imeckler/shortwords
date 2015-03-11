var totalScore = localStorage.getItem('shortwords-score')
totalScore = totalScore ? JSON.parse(totalScore) : 0;

var gameDiv = document.getElementById('game');
var main = Elm.embed(Elm.Main, gameDiv, {setTotalScore: totalScore});
main.ports.setTotalScore.send(totalScore);

main.ports.setLocalStorage.subscribe(function(i) {
  localStorage.setItem('shortwords-score', i);
});

