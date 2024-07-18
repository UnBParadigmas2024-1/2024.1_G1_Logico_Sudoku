const grid = document.getElementById("sudoku-grid");
const api = "http://localhost";
let id = 0;

for (let i = 0; i < 81; i++) {
  const celula = document.createElement("input");
  celula.className = "sudoku-board-cell";
  celula.maxLength = 1;
  celula.type = "number";

  if (Math.floor(i / 9) % 3 === 0) celula.classList.add("top-border");
  if (Math.floor(i % 9) % 3 === 0) celula.classList.add("left-border");
  if ((Math.floor(i / 9) + 1) % 3 === 0) celula.classList.add("bottom-border");
  if ((Math.floor(i % 9) + 1) % 3 === 0) celula.classList.add("right-border");
  celula.id='cell-'+Math.floor(i / 9)+'-'+Math.floor(i % 9);

  celula.addEventListener("input", function () {
    const row = Math.floor(i / 9);
    const col = i % 9;
    const value = parseInt(celula.value) || 0;
    enviarJogada(value, row, col);
  });
  
  grid.appendChild(celula);
}


function iniciarJogo() {
  console.log('Iniciar Jogo');
  id++;
  fetch(`${api}/sudoku/start`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({ id: id })
  })
    .then((response) => response.json())
    .then((data) => {
      console.log(data)
      popularTabuleiro(data.puzzle);
      getLives();
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

function enviarJogada(value, row, col) {
  const vidasElement = document.getElementById('vidas');
  const vidasAntes = vidasElement.textContent;

  fetch(`${api}/sudoku/move`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({ number: value, row: row+1, col: col+1 })
  })
  .then(response => response.json())
  .then(data => {
    console.log('Jogada enviada:', data);
    getStatus();

    fetch(`${api}/sudoku/get-lives`, {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json',
      },
    })
    .then(response => response.json())
    .then(data => {
      const vidasDepois = data.vidas;
      vidasElement.textContent = vidasDepois;

      if (vidasAntes !== vidasDepois.toString()) {
        const inputId = `cell-${row}-${col}`;
        const inputElement = document.getElementById(inputId);
        if (inputElement) {
          inputElement.value = '';
        }
      }

      console.log('Vidas recebidas:', data);
    })
    .catch(error => console.error('Erro ao obter vidas:', error));
  })
  .catch(error => console.error('Erro ao enviar jogada:', error));
}


function getLives() {
  fetch(`${api}/sudoku/get-lives`, {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json',
    },
  })
  .then(response => response.json())
  .then(data => {
    // Atualiza o conteúdo do parágrafo com o id 'vidas'
    document.getElementById('vidas').textContent = data.vidas;
    console.log('Vidas recebidas:', data);
  })
  .catch(error => console.error('Erro:', error));
}

function getStatus() {
  fetch(`${api}/sudoku/status`, {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json',
    },
  })
  .then(response => response.json())
  .then(data => {
    if (data.status === 0) {
      alert("Que pena, você morreu!");
    } else if (data.status === 2) {
      alert("Parabéns, você ganhou!");
    }
    console.log('Status recebido:', data);
  })
  .catch(error => console.error('Erro:', error));
}


document.getElementById("btn-start").addEventListener("click", iniciarJogo);