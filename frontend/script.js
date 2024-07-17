const grid = document.getElementById("sudoku-grid");
const api = "http://localhost:8080";

for (let i = 0; i < 81; i++) {
  const celula = document.createElement("input");
  celula.className = "celula";
  celula.maxLength = 1;
  celula.type = "text";

  if (Math.floor(i / 9) % 3 === 0) celula.classList.add("top-border");
  if (Math.floor(i % 9) % 3 === 0) celula.classList.add("left-border");
  if ((Math.floor(i / 9) + 1) % 3 === 0) celula.classList.add("bottom-border");
  if ((Math.floor(i % 9) + 1) % 3 === 0) celula.classList.add("right-border");

  grid.appendChild(celula);
}

function iniciarJogo() {
  fetch(`${api}/sudoku/start`)
    .then((response) => response.json())
    .then((data) => {
      popularTabuleiro(data.puzzle);
    })
    .catch((error) => console.error("Erro:", error));
}

function popularTabuleiro(puzzle) {
  const celulas = document.querySelectorAll(".celula");
  for (let i = 0; i < 81; i++) {
    const row = Math.floor(i / 9);
    const col = i % 9;
    celulas[i].value = puzzle[row][col] !== 0 ? puzzle[row][col] : "";
  }
}

function getVidas() {
  fetch(`${api}/sudoku/get-lives`)
    .then((response) => response.json())
    .then((data) => {
      document.getElementById("vidas").textContent = data.vidas;
    })
    .catch((error) => console.error("Erro:", error));
}

function getAjuda() {
  fetch(`${api}/sudoku/get-help`)
    .then((response) => response.json())
    .then((data) => {
      alert(data.mensagem);
    })
    .catch((error) => console.error("Erro:", error));
}

function resolveSudoku() {
  const celulas = document.querySelectorAll(".celula");
  const puzzle = Array.from(celulas).map(
    (celula) => parseInt(celula.value) || 0
  );

  fetch(`${api}/sudoku/solve`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify(puzzle),
  })
    .then((response) => response.json())
    .then((solution) => {
      solution.forEach((value, index) => {
        celulas[index].value = value !== 0 ? value : "";
      });
    });
}
