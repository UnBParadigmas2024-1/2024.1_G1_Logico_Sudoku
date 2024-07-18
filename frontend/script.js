const grid = document.getElementById("sudoku-grid");
const api = "http://localhost:8080";

for (let i = 0; i < 81; i++) {
  const celula = document.createElement("input");
  celula.className = "sudoku-board-cell";
  celula.maxLength = 1;
  celula.type = "text";

  if (Math.floor(i / 9) % 3 === 0) celula.classList.add("top-border");
  if (Math.floor(i % 9) % 3 === 0) celula.classList.add("left-border");
  if ((Math.floor(i / 9) + 1) % 3 === 0) celula.classList.add("bottom-border");
  if ((Math.floor(i % 9) + 1) % 3 === 0) celula.classList.add("right-border");

  grid.appendChild(celula);
}

function iniciarJogo() {
  console.log('Iniciar Jogo');
  fetch(`${api}/sudoku/start`, {
    method: 'GET',
  })
    .then((response) => response.json())
    .then((data) => {
      console.log(data)
      popularTabuleiro(data.puzzle);
    })
    .catch((error) => console.error("Erro:", error));
}

function popularTabuleiro(puzzle) {
  const celulas = document.querySelectorAll(".sudoku-board-cell");
  for (let i = 0; i < 81; i++) {
    const row = Math.floor(i / 9);
    const col = i % 9;
    celulas[i].value = puzzle[row][col] !== 0 ? puzzle[row][col] : "";
  }
}

function solveMatrix(puzzle) {
  fetch(`${api}/sudoku/solve`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({ puzzle: puzzle })
  })
  .then(response => response.json())
  .then(data => {
    if (data.solution) {
      console.log('Puzzle resolvido corretamente:', data.solution);
      popularTabuleiro(data.solution);
      alert('Solução correta!');
    } else if (data.error) {
      console.log('Erro na solução:', data.error);
      alert(`Erro: ${data.error} - Vidas restantes: ${data.vidas}`);
    }
  })
  .catch(error => console.error('Erro:', error));
}

function resolveSudoku() {
  const celulas = document.querySelectorAll(".sudoku-board-cell");
  const puzzle = [];
  for (let i = 0; i < 81; i++) {
    const row = Math.floor(i / 9);
    const col = i % 9;
    if (!puzzle[row]) {
      puzzle[row] = [];
    }
    puzzle[row][col] = parseInt(celulas[i].value) || 0;
  }

  fetch(`${api}/sudoku/solve`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify({ puzzle: puzzle }),
  })
    .then((response) => response.json())
    .then((solution) => {
      popularTabuleiro(solution.puzzle);
    })
    .catch((error) => console.error("Erro:", error));
}

document.getElementById("btn-start").addEventListener("click", iniciarJogo);
document.getElementById("btn-solve-step").addEventListener("click", () => alert("Solve Step not implemented"));
document.getElementById("btn-solve-all").addEventListener("click", solveMatrix);
document.getElementById("btn-clear-board").addEventListener("click", () => popularTabuleiro(Array.from({ length: 9 }, () => Array(9).fill(0))));
document.querySelector(".js-candidate-toggle").addEventListener("change", function () {
  if (this.checked) {
    document.querySelectorAll(".candidates").forEach(el => el.style.display = "block");
  } else {
    document.querySelectorAll(".candidates").forEach(el => el.style.display = "none");
  }
});
