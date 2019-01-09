const data = require('./data.js');

function print(value) {
  console.log(value.toString());
  return data.theUndef;
}

const ALL = new Map();
ALL.set('print', new data.Func('print', print));

module.exports = {
  ALL,
};
