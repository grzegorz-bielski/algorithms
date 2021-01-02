function gaussianElimination(a) {
  const rows = a.length
  const cols = a[0].length
  let row = 0;
  for (let col = 0; col < cols - 1; ++col) {
    // console.log("col", col)

    let pivot = row;
    for (let i = row + 1; i < rows; ++i) {
      // console.log("i", i)
      if (Math.abs(a[i][col]) > Math.abs(a[pivot][col])) {
        pivot = i;
      }
    }
    // console.log("pivot", pivot)

    if (a[pivot][col] === 0) {
      console.log("The matrix is singular.");
      continue;
    }

    if (col !== pivot) {
      const t = a[col];
      // console.log(col, "before", a)
      a[col] = a[pivot];
    
      // console.log(col, "before", a)
      a[pivot] = t;
      // console.log(col, "after", a)
    }
    
    // // console.log("a", a)

    for (let i = row + 1; i < rows; ++i) {
      // console.log("i", i)
      const scale = a[i][col] / a[row][col];

      for (let j = col + 1; j < cols; ++j) {
        // console.log("j", j)
        a[i][j] -= a[row][j] * scale;
      }

      a[i][col] = 0;
    }

    ++row;
  }
  return a;
}

function backSubstitution(a) {
  const rows = a.length;
  const cols = a[0].length;
  const sol = [];

  for (let i = rows - 1; i >= 0; --i) {
    // console.log(i)
    let sum = 0;
    for (let j = cols - 2; j > i; --j) {
      console.log(j, "xd")
      sum += sol[j] * a[i][j];
    }

    console.log(sum, "sum")

    sol[i] = (a[i][cols - 1] - sum) / a[i][i];
  }
  return sol;
}

function gaussJordan(a) {
  const cols = a[0].length;
  let row = 0;

  for (let col = 0; col < cols - 1; ++col) {
    if (a[row][col] !== 0) {
      for (let i = cols - 1; i > col - 1; --i) {
        a[row][i] /= a[row][col];
      }

      for (let i = 0; i < row; ++i) {
        for (let j = cols - 1; j > col - 1; --j) {
          a[i][j] -= a[i][col] * a[row][j];
        }
      }

      ++row;
    }
  }
}

function printMatrixRow(row) {
  const text = row
    .map(v => (v < 0 ? " " : "  ") + v.toPrecision(8))
    .join("");

  console.log(text);
}

function printMatrix(a) {
  for (const row of a) {
    printMatrixRow(row);
  }
}

// const a = [
//   [3,  2, -4,  3],
//   [2,  3,  3, 15],
//   [5, -3,  1, 14]
// ];

const a = [
  // [2, 3, 4, 6],
  // [1, 2, 3, 4],
  // [3, -4, 0, 10]
[1,  1,  0,  1,  0,  0,  0,  0,  0, 57],  
[1,  1,  1,  0,  1,  0,  0,  0,  0, 56],  
[0,  1,  1,  0,  0,  1,  0,  0,  0, 60], 
[1,  0,  0,  1,  1,  0,  1,  0,  0, 48], 
[0,  1,  0,  1,  1,  1,  0,  1,  0, 75],  
[0,  0,  1,  0,  1,  1,  0,  0,  1, 72],  
[0,  0,  0,  1,  0,  0,  1,  1,  0, 39],  
[0,  0,  0,  0,  1,  0,  1,  1,  1, 56],  
[0,  0,  0,  0,  0,  1,  0,  1,  1, 48]
]

gaussianElimination(a);
console.log("Gaussian elimination:");
printMatrix(a);

// gaussJordan(a);
// console.log("\nGauss-Jordan:");
// printMatrix(a);

const sol = backSubstitution(a);
console.log("\nSolutions are:");
printMatrixRow(sol);