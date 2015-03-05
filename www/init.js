var totalScore = localStorage.getItem('shortwords-score')
totalScore = totalScore ? JSON.parse(totalScore) : 0;

var main = Elm.fullscreen(Elm.Main, {setTotalScore: totalScore});
main.ports.setTotalScore.send(totalScore);

main.ports.setLocalStorage.subscribe(function(i) {
  localStorage.setItem('shortwords-score', i);
});

