const builtins = require('./builtins.js');
const data = require('./data.js');

function createTopLevelEnv() {
  const env = new data.Environment();
  for (const [name, value] of builtins.ALL.entries()) {
    env.ensure(name).value = value;
  }
  return env;
}

module.exports = {
  createTopLevelEnv,
};
