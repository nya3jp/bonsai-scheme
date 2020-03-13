import {parseList} from "./parser";
import {createTopLevelEnv} from "./stdenv";
import {evaluate} from "./eval";

const fs = require('fs');
const process = require('process');
const util = require('util');

async function batchMain(path: string) {
  const code = await util.promisify(fs.readFile)(path, 'utf8');
  const exprs = parseList(code);
  const env = createTopLevelEnv();
  for (const expr of exprs) {
    evaluate(env, expr);
  }
}

async function interactiveMain() {
  throw new Error('not implemented');
}

async function mainImpl() {
  const argv = process.argv;
  if (argv.length === 2) {
    return await interactiveMain();
  }
  if (argv.length === 3) {
    return await batchMain(argv[2]);
  }
  throw new Error('Wrong number of arguments');
}

function main() {
  mainImpl().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}

main();
