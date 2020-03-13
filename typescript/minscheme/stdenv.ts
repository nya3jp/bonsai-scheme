import {Environment} from "./data";
import {allBuiltins} from "./builtins";

export function createTopLevelEnv(): Environment {
  const env = new Environment();
  for (const [name, value] of allBuiltins) {
    env.ensure(name).value = value;
  }
  return env;
}
