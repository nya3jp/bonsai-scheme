import sys
from typing import List

from minilisp import data
from minilisp import eval
from minilisp import parser
from minilisp import stdenv


def repr_main() -> int:
    env = stdenv.make_top_level_env()
    while True:
        sys.stdout.write('> ')
        sys.stdout.flush()
        try:
            code = input()
        except (KeyboardInterrupt, EOFError):
            break
        result = data.UNDEF
        try:
            for expr in parser.parse(code):
                result = eval.evaluate(env, expr)
        except Exception as e:
            print('ERROR', *e.args)
        print(result)
    return 0


def batch_main(path: str) -> int:
    with open(path, 'r') as f:
        code = f.read()
    env = stdenv.make_top_level_env()
    for expr in parser.parse(code):
        eval.evaluate(env, expr)
    return 0


def main(argv: List[str]) -> int:
    if len(argv) == 1:
        return repr_main()
    elif len(argv) == 2:
        return batch_main(argv[1])
    print('usage: minilisp [file]')
    return 1


if __name__ == '__main__':
    sys.exit(main(sys.argv))
