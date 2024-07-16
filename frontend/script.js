const grid = document.getElementById('sudoku-grid');

for (let i = 0; i < 81; i++) {
    const celula = document.createElement('input');
    celula.className = 'celula';
    celula.maxLength = 1;
    celula.type = 'text';

    if (Math.floor(i / 9) % 3 === 0) celula.classList.add('top-border');
    if (Math.floor(i % 9) % 3 === 0) celula.classList.add('left-border');
    if ((Math.floor(i / 9) + 1) % 3 === 0) celula.classList.add('bottom-border');
    if ((Math.floor(i % 9) + 1) % 3 === 0) celula.classList.add('right-border');

    grid.appendChild(celula);
}


function resolveSudoku() {
    const celulas = document.querySelectorAll('.celula');
    const puzzle = Array.from(celulas).map(celula => parseInt(celula.value) || 0);
    
    fetch('http://localhost:8080/solve', {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        body: JSON.stringify(puzzle)
    })
    .then(response => response.json())
    .then(solution => {
        solution.forEach((value, index) => {
            celulas[index].value = value !== 0 ? value : '';
        });
    });
}
