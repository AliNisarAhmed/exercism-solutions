export class Triangle {
    constructor(numRows) {
        this.numRows = numRows;
    }

    get lastRow() {
      return this.rows[this.rows.length - 1];
    }

    get rows() {
      return this.generateRows([], 0);
    }

    generateRows(rows, numRows) {
      if (numRows === this.numRows) return rows;
      if (numRows === 0) {
        return this.generateRows([ [1] ], numRows + 1);
      } else {
        let lastRow = rows[rows.length - 1];
        let newRow = [];
        for (let i = 1; i < lastRow.length; i++) {
          newRow.push( lastRow[i - 1] + lastRow[i] );
        }
        return this.generateRows( [ ...rows, [1, ...newRow, 1] ], numRows + 1);
      }
    }
}

// module.exports = Triangle;
