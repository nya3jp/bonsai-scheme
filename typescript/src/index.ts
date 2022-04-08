import {parseList} from './parser';
import {createTopLevelEnv} from './stdenv';
import {evaluate} from './eval';
import fs = require('fs');
import process = require('process');
import util = require('util');

async function batchMain(path: string): Promise<void> {
  const code = await util.promisify(fs.readFile)(path, 'utf8');
  const exprs = parseList(code);
  const env = createTopLevelEnv();
  for (const expr of exprs) {
    evaluate(env, expr);
  }
}

async function interactiveMain(): Promise<void> {
  throw new Error('not implemented');
}

async function mainImpl(): Promise<void> {
  const argv = process.argv;
  if (argv.length === 2) {
    return await interactiveMain();
  }
  if (argv.length === 3) {
    return await batchMain(argv[2]);
  }
  throw new Error('Wrong number of arguments');
}

function main(): void {
  mainImpl().catch(err => {
    console.error(err);
    process.exit(1);
  });
}

main();
