from minilisp import builtins
from minilisp import data


def make_top_level_env() -> data.Environment:
    env = data.Environment(parent=None)
    for name, func in builtins.ALL_MAP.items():
        env.ensure(name).value = data.Func(name, func)
    return env
